{-# LANGUAGE OverloadedStrings, ScopedTypeVariables, TypeFamilies, TemplateHaskell #-}

module TH where

import Language.Haskell.TH
import Language.Sunroof.Types as SRT
import Language.Sunroof.Classes
import Language.Sunroof.JS.Object
import Language.Sunroof.JS.Bool
import Data.Boolean

-- | @derive@ derives incomplete instances.
--
-- you write the newtype explictly, and @derive@ does the rest.
--
-- > newtype JSX = JSX JSObject
--
-- and then the start of the JSTuple instance, and the rest gets filled in
--
-- > derive [d| instance (SunroofArgument o) => JSTuple (JSX o) where
-- >                type Internals (JSX o) = (JSString,JSNumber)
-- >        |]
--
-- generates
--
-- > instance Show JSX where
-- >    show (JSX o) = show o
--

deriveJSTuple :: Q [Dec] -> Q [Dec]
deriveJSTuple decsQ = do
        decs <- decsQ
        fmap concat $ mapM complete decs
  where
        complete :: Dec -> Q [Dec]
        complete (InstanceD cxt hd@(AppT (ConT typeClass) ty) decls) = do
                let k decls' = InstanceD cxt hd (decls ++ decls')
                let findClass (ConT t) = t
                    findClass (AppT t1 _) = findClass t1
                    findClass _ =  error $ "strange instance head found in derive " ++ show ty
                let tConTy = findClass ty
                -- Next, find the type instance
                let internalTy = case decls of
                        [TySynInstD tyFun [arg] internalTy] | tyFun == ''Internals -> internalTy
                        _  -> error $ "can not find usable type instance inside JSTuple"
                runIO $ print internalTy
                let findInternalStructure (TupleT n) ts = do
                        vs <- sequence [ newName "v" | _ <- ts ]
                        return (TupE,TupP [ VarP v | v <- vs], vs `zip` [ "f" ++ show i | (i::Int) <- [1..]])
                    findInternalStructure (AppT t1 t2) ts = findInternalStructure t1 (t2 : ts)

                (builder :: [Exp] -> Exp,unbuilder :: Pat, vars :: [(Name,String)]) <- findInternalStructure internalTy []

                runIO $ print (unbuilder,vars)

                -- Now work with the tConTy, to get the tCons
                info <- reify tConTy
                let tCons = case info of
                      TyConI (NewtypeD _ _ _ (NormalC tCons [(NotStrict,ConT o)]) [])
                        | o /= ''JSObject -> error $ "not newtype of JSObject"
                        | typeClass /= ''JSTuple -> error $ "not instance of JSTuple" ++ show (tConTy,''JSTuple)
                        | otherwise -> tCons
                      _ -> error $ "strange info for newtype type " ++ show info

--                                decls' <- completeWith typeClass tConTy tCons
--                                return $ k decls'


                o <- newName "o"
                n <- newName "n"

                return [ InstanceD cxt (AppT (ConT ''Show) ty)
                           [ FunD 'show
                              [ Clause [ConP tCons [VarP o]]
                                         (NormalB (AppE (VarE 'show) (VarE o))) []]]
                       , InstanceD cxt (AppT (ConT ''Sunroof) ty)
                           [ FunD 'box
                              [ Clause [VarP n] (NormalB (AppE (ConE tCons)
                                                               (AppE (VarE 'box) (VarE n)))) []]
                          , FunD 'unbox
                              [ Clause [ConP tCons [VarP o]]
                                                (NormalB (AppE (VarE 'unbox) (VarE o))) []]
                           ]
                       , InstanceD cxt (AppT (ConT ''IfB) ty)
                              [ ValD (VarP 'ifB) (NormalB (VarE 'jsIfB)) [] ]
                       , TySynInstD ''BooleanOf [ty] (ConT ''JSBool)
                       , InstanceD cxt (AppT (ConT ''JSTuple) ty) $ decls ++
                           [ FunD 'SRT.match
                              [Clause [VarP o] (NormalB (builder
                                  [ AppE (AppE (VarE $ mkName "!") (VarE o))
                                         (AppE (VarE 'attr) (LitE $ StringL $ s))
                                  | (_,s) <- vars ])) []]
                           , FunD 'SRT.tuple
                              [ Clause [unbuilder] (NormalB (DoE (
                                        [ BindS (VarP o) (AppE (AppE (VarE 'new) (LitE $ StringL $ "Object")) (TupE []))
                                        ] ++
                                        [ NoBindS $
                                          let assign = AppE (AppE (ConE $ mkName ":=")
                                                                  (AppE (VarE 'attr) (LitE $ StringL $ s)))
                                                            (VarE v)

                                          in  AppE (AppE (VarE $ mkName "#")
                                                         (VarE o))
                                                   (assign)
                                        | (v,s) <- vars
                                        ] ++
                                        [ NoBindS $ AppE (VarE 'return) (AppE (ConE tCons) (VarE o))
                                        ]))) []
                              ]
                           ]
                       ]

        complete dec = error $ "can not complete derive for " ++ show dec

        completeWith :: Name -> Name -> Name -> Q [Dec]
        completeWith tyClass tConTy tCons
           | tyClass == ''Show = do
                o <- newName "o"
                return [ FunD 'show
                              [ Clause [ConP tCons [VarP o]]
                                         (NormalB (AppE (VarE 'show) (VarE o))) []]]
           | tyClass == ''Sunroof = do
                o <- newName "o"
                n <- newName "n"
                return [ FunD 'box
                          [ Clause [VarP n] (NormalB (AppE (ConE tCons)
                                                           (AppE (VarE 'box) (VarE n)))) []]
                       ,FunD 'unbox
                          [ Clause [ConP tCons [VarP o]]
                                            (NormalB (AppE (VarE 'unbox) (VarE o))) []]
                       ]
           | tyClass == ''IfB =
                return [ ValD (VarP 'ifB) (NormalB (VarE 'jsIfB)) []
                       ]
           | otherwise = error $ "trying to complete " ++ show tyClass

deriveX :: Q [Dec] -> Q [Dec]
deriveX decsQ = do
        decs <- decsQ
        mapM complete decs
  where
        complete :: Dec -> Q Dec
        complete (InstanceD cxt hd@(AppT (ConT typeClass) ty) decls) = do
                let k decls' = InstanceD cxt hd (decls ++ decls')
                let findClass (ConT t) = t
                    findClass (AppT t1 _) = findClass t1
                    findClass _ =  error $ "strange instance head found in derive " ++ show ty
                let tConTy = findClass ty
                info <- reify tConTy
                case info of
                    TyConI (NewtypeD _ _ _ (NormalC tCons [(NotStrict,ConT o)]) [])
                        | o == ''JSObject -> do
                                decls' <- completeWith typeClass tConTy tCons
                                return $ k decls'
                        | otherwise -> error $ "not newtype of JSObject"
                    _ -> error $ "strange info for newtype type " ++ show info
        complete dec = error $ "can not complete " ++ show dec

        completeWith :: Name -> Name -> Name -> Q [Dec]
        completeWith tyClass tConTy tCons
           | tyClass == ''Show = do
                o <- newName "o"
                return [ FunD 'show
                              [ Clause [ConP tCons [VarP o]]
                                         (NormalB (AppE (VarE 'show) (VarE o))) []]]
           | tyClass == ''Sunroof = do
                o <- newName "o"
                n <- newName "n"
                return [ FunD 'box
                          [ Clause [VarP n] (NormalB (AppE (ConE tCons)
                                                           (AppE (VarE 'box) (VarE n)))) []]
                       ,FunD 'unbox
                          [ Clause [ConP tCons [VarP o]]
                                            (NormalB (AppE (VarE 'unbox) (VarE o))) []]
                       ]
           | tyClass == ''IfB =
                return [ ValD (VarP 'ifB) (NormalB (VarE 'jsIfB)) []
                       ]
           | otherwise = error $ "trying to complete " ++ show tyClass

jstuple :: Name -> Q [Dec]
jstuple nm = do
        info <- reify nm
        case info of
          TyConI (TySynD _ [] tyRhs) -> js2 (unroll tyRhs [])
          _ -> error $ "jstuple: " ++ Prelude.show nm ++ " is not a type constructor, it is a : " ++ Prelude.show info
  where
        -- The new name of the JS-version of the structure
        jsName :: Name
        jsName = mkName $ "JS" ++ nameBase nm

        js2 :: (Type,[Type]) -> Q [Dec]
        js2 (t@(TupleT n),ts) | length ts == n = do
                runIO $ print $ jsName
                o <- newName "o"
                let field_name :: [(String,Type)]
                    field_name = [ ("f" ++ Prelude.show n ,t) | (n::Int,t) <- zip [1..] ts ]

                vs <- sequence [ newName "v" | _ <- ts ]

                return
                   [ NewtypeD
                        []
                        jsName
                        []
                        (NormalC jsName [(NotStrict,ConT $ mkName "JSObject")])
                        []
                      -- this is a simple wrapper roound JSObject
                   , InstanceD
                        []
                        (AppT (ConT ''Sunroof) (ConT $ jsName))
                        [ FunD 'box
                          [ Clause [VarP o] (NormalB (AppE (ConE jsName)
                                                           (AppE (VarE 'box) (VarE o)))) []]
                        , FunD 'unbox
                          [ Clause [ConP jsName [VarP o]]
                                            (NormalB (AppE (VarE 'unbox) (VarE o))) []]
                        ]
                   , InstanceD
                        []
                        (AppT (ConT ''JSTuple) (ConT $ jsName))
                        [ TySynInstD ''Internals [ConT jsName] (foldl AppT t ts)

                        , FunD 'SRT.match
                          [ Clause [VarP o] (NormalB (TupE
                                        [ SigE (AppE (AppE (VarE $ mkName "!") (VarE o))
                                                           (AppE (VarE 'attr) (LitE $ StringL $ f_nm)))
                                               t
                                        | (f_nm,t) <- field_name
                                        ])) []
                          ]
                        , FunD 'SRT.tuple
                          [ Clause [TupP $ map VarP vs] (NormalB (DoE (
                                        [ BindS (VarP o) (AppE (AppE (VarE 'new) (LitE $ StringL $ "Object")) (TupE []))
                                        ] ++
                                        [ NoBindS $
                                          let assign = AppE (AppE (ConE $ mkName ":=")
                                                                  (AppE (VarE 'attr) (LitE $ StringL $ f_nm)))
                                                            (VarE v)

                                          in  AppE (AppE (VarE $ mkName "#")
                                                         (VarE o))
                                                   (assign)
                                        | ((f_nm,t),v) <- field_name `zip` vs
                                        ] ++
                                        [ NoBindS $ AppE (VarE 'return) (AppE (ConE jsName) (VarE o))
                                        ]))) []
                          ]

                        ]
                   ]
        js2 (o,_) = error $ "jstuple: " ++ Prelude.show o ++ " is not a tuple"

        unroll :: Type -> [Type] -> (Type,[Type])
        unroll (AppT t1 t2) ts = unroll t1 (t2 : ts)
        unroll other ts        = (other,ts)

--  ''Slide ''JSSlide
