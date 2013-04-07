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
                    findInternalStructure (ConT v) ts = do
                            info <- reify v
                            runIO $ print info
                            case info of
                              TyConI (DataD [] _ [] [NormalC internalCons args] []) -> do
                                vs <- sequence [ newName "v" | _ <- args ]
                                return ( foldl AppE (ConE internalCons)
                                       , ConP internalCons [ VarP v | v <- vs]
                                       , vs `zip` [ "f" ++ show i | (i::Int) <- [1..]]
                                       )
                              TyConI (NewtypeD [] _ [] (NormalC internalCons args) []) -> do
                                vs <- sequence [ newName "v" | _ <- args ]
                                return ( foldl AppE (ConE internalCons)
                                       , ConP internalCons [ VarP v | v <- vs]
                                       , vs `zip` [ "f" ++ show i | (i::Int) <- [1..]]
                                       )
                              TyConI (DataD [] _ [] [RecC internalCons args] []) -> do
                                vs <- sequence [ newName "v" | _ <- args ]
                                return ( foldl AppE (ConE internalCons)
                                       , ConP internalCons [ VarP v | v <- vs]
                                       , vs `zip` [ nameBase x | (x,_,_) <- args ]
                                       )

                              o -> error $ "can not find internal structure of cons " ++ show (v,ts,info)
                    findInternalStructure o ts = error $ "can not find internal structure of type " ++ show (o,ts)

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



--  ''Slide ''JSSlide
