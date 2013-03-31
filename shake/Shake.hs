-- This is the shake script for our system

module Main where

import Development.Shake as Shake
import System.Directory
import System.Process
import System.Environment
import Development.Shake.FilePath
import Control.Monad
import Data.Char
import Data.List

scanned = "scanned"     -- the scanned input
pages   = "pages"       -- each page of the exam, in some order

main = do
        args <- getArgs
        main2 args

main2 ["clean"] = do
        system "rm -f pages/*"
        return ()

main2 [] = shake (shakeOptions) $ do
        -- First, make sure the png pages are generated

        action $ do
                files <- getDirectoryFiles scanned ["*.pdf"]
                liftIO $ print files
                need [ pages </> addExtension (dropExtension nm) (show res ++ "-0.png")
                     | nm <- files
                     , res <- [10,300]
                     ]

--                files <- getDirectoryFiles repo ["//*.hs"]
--                liftIO $ print files
--                need $ map (repo </>) files
        want [ ]

        -- generating the large and small pdf


        "pages/*-0.png" *> \ out -> do

                let src = dropExtension
                        $ dropExtension
                        $ dropDirectory1
                        $ out

                let res = id
                        $ tail
                        $ init $ init
                        $ takeExtension
                        $ dropExtension out

                let input = "scanned" </> src `addExtension` "pdf"
                -- Actually writes to Foo-0.png, not Foo.png
                let output = pages </> src `addExtension` (res ++ ".png")
                need [input]
                liftIO $ putStrLn $ "Writing to: " ++ output
                system' "convert"
                        [ "-density"
                        , res
                        , input
                        , output
                        ]

                liftIO $ print (out,src,res)
{-
        let autogen = "inplace-autogen/Paths_sunroof_examples.hs"
        let targets =
             [ "dist" </> "build" </> nm </> nm
             | (nm,ex) <- execs
             , xs == ["all" ]|| nm `elem` xs
             ]
        want [autogen]

        want targets

        autogen *> \ out ->  do
                pwd <- liftIO $ getCurrentDirectory
                writeFile' out $ unlines
                  [ "module Paths_sunroof_examples where"
                  , "getDataDir :: IO String"
                  , "getDataDir = return " ++ show pwd
                  ]

        targets **> \ out -> do
                liftIO $ print out
                let nm = takeFileName out
                liftIO $ print nm
                let Just exec = lookup nm execs
                liftIO $ print exec

                need [ the_dir exec </> the_main exec]

                sequence_
                   [ do files <- getDirectoryFiles repo ["*.cabal"]
                        liftIO $ print files
                        need $ map (repo </>) files
                        files <- getDirectoryFiles repo ["//*.hs"]
                        liftIO $ print files
                        need $ map (repo </>) files
                   | repo <- repos
                   ]

                liftIO $ putStrLn $ "Building: " ++ out
                -- compile inside the build dir
                let cache = takeDirectory out </> "cache"
                systemCwd "."
                          "ghc"
                          ["--make",the_dir exec </> the_main exec, the_opts exec,
                           "-hidir",cache,"-odir",cache,
                           "-dcore-lint",
                           "-o", out,
                           "-i" ++ concat (intersperse  ":"  (the_dir exec : "inplace-autogen" : repos))
                           ]

-}

