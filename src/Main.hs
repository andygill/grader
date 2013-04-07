{-# LANGUAGE OverloadedStrings, ScopedTypeVariables, TypeFamilies, TemplateHaskell #-}

module Main where

import Data.Default ( Default(..) )
import Data.Semigroup ( (<>) )
import Control.Monad
import Data.Boolean
import Data.Boolean.Numbers as N

import Language.Sunroof
import Language.Sunroof.Server
import Language.Sunroof.Compiler
import Language.Sunroof.TH
import Language.Sunroof.JS.Canvas
import Language.Sunroof.JS.Browser as B
import Language.Sunroof.JS.JQuery
import Network.Wai.Middleware.Static

import Types


ourPolicy :: Policy -> Policy
ourPolicy p = p
        <|> (hasSuffix ".html" >-> addBase "html")
        <|> (hasPrefix "pages/")

main :: IO ()
main = do
-- dataDir <- getDataDir
 sunroofServer (def { sunroofVerbose = 3
                      , cometResourceBaseDir = "." -- dataDir
                      , cometPolicy = ourPolicy (cometPolicy def)
                      , cometIndexFile = "html/view.html"
                      }) $ \ doc -> asyncJS doc prog



-- how big do we scale our pdf picture, in dots per inch?
dpi :: JSNumber
dpi = 300

prog :: JSA ()
prog = do
      -- set up the slider listener
      ch <- newChan
      jq "body" >>= on "slide" ".slide" (\ (a :: JSObject, aux :: JSObject) -> do
                the_id    :: JSString <- jq (cast $ this) >>= invoke "attr" ("id" :: JSString)
                the_index :: JSNumber <- evaluate $ N.round (aux ! "value" :: JSNumber)
                tuple (Slide the_id the_index) >>= \ (o :: JSSlide) -> ch # writeChan o)


      -- channel of what to show on the graphical viewport
      viewportChan :: JSChan JSViewPort <- newChan

      -- Here is the model
      model :: JSModel <- tuple $ Model
                { mPage  = 0
                , mUID   = ""
                , mX     = 400
                , mY     = 100
                , mScale = 2.0
                }

      modelChan :: JSChan (JSFunction JSModel JSModel) <- newChan

      let upModel :: (SunroofThread t) => (Model -> JSA Model) -> JS t ()
          upModel f = do
                  g <- function (\ m -> do
                                let jsm = match m
                                jsm' <- f jsm
                                tuple jsm')
                  modelChan # writeChan g

      forkJS $ loop model $ \ m -> do
              up <- modelChan # readChan
              console # B.log ("updateModel" :: JSString)
              m' <- up $$ m
              let jsm = match m'
              -- and propogate the model
              vp :: JSViewPort <- tuple $ ViewPort ("pages/exam.300-" <> cast (mPage jsm - 1) <> ".png") 400 100 2.0
              viewportChan # writeChan vp
              return m'

      forkJS $ loop () $ \ () -> do
        o :: JSSlide <- ch # readChan
        let (Slide the_id aux) = match o
        console # B.log ("Slide : " <> cast the_id <> " " <> cast aux :: JSString)
        switch the_id [ ("page-slider", upModel $ \ jsm -> return $ jsm { mPage = aux })
                      ]

      return ()



{-
      -- global state changers
      setPage :: JSContinuation JSNumber <- continuation $ \ (n :: JSNumber) -> do

              vp :: ViewPort <- (viewerState # takeMVar) >>= (return . match)
              let vp' = vp { vpPage = n }
              return ()
-}
      -- The cached image, a non-connected DOM object, that is also an <img>
      imageObj :: JSObject <- new "Image" ()

      canvas <- document # getElementById("myCanvas")
      context <- canvas # getContext("2d")

      okToDraw :: JSMVar () <- newMVar ()

      forkJS $ loop () $ \ () -> do
              console # B.log ("starting draw image loop" :: JSString)
              -- First get the event
              o <- viewportChan # readChan
              let vp = match o

              console # B.log ("waiting for ok To draw" :: JSString)
              -- next, wait until you have permision to start drawing
              okToDraw # takeMVar

              console # B.log ("got ok To draw" :: JSString)

              -- figure out if we have the correct image loaded

--              paint <- function


              -- load new image object
              imageObj # src := vpFile vp;
              console # B.log ("loading image " <> vpFile vp :: JSString)

              -- check imageObject.complete

              jq (cast $ imageObj) >>= on "load" "" (\ () -> do
                      console # B.log ("on-load" :: JSString)
--                        alert("loaded image")
                      context # drawImageClip imageObj (400, 100) (2050, 1490) (0, 0) (960, 600)
                      forkJS (okToDraw # putMVar ()))

              -- and go again
              console # B.log ("end of loop" :: JSString)
              return ()

--      vp :: JSViewPort <- tuple $ ViewPort "pages/exam.300-2.png" 400 100 2.0
--      viewportChan # writeChan vp

      return ()


{-
      jq "body" >>= on "click" ".click" (\ () -> do
                the_id :: JSString <- jq (cast $ this) >>= invoke "attr" ("id" :: JSString)
                o <- new "Object" ()
                o # "id" := the_id
                ch # writeChan o)


      obj <- new "Object" ()
      obj # attr "model" := (0 :: JSNumber)

      -- This is using the imperative update to enable the
      let slider :: JSNumber -> JSObject -> JSB JSObject
          slider nm = invoke "slider"  ("value" :: JSString, nm)

          update :: String -> JSNumber -> JSNumber -> JSNumber -> JSB ()
          update nm val mn mx =
              ifB ((val <=* mx) &&* (val >=* mn))
                  (obj # attr nm := val)
                  (return ())

          switchB _   []         def = def
          switchB tag ((a,b):xs) def = ifB (tag ==* a) b (switchB tag xs def)

      fib <- fixJSA $ \ fib (n :: JSNumber) -> do
          ifB (n <* 2)
              (return (1 :: JSNumber))
              (liftM2 (+) (apply fib (n - 1)) (apply fib (n - 2)))

      loop () $ \() -> do
          res <- ch # readChan
--          res <- wait "body" (slide <> click)
          model <- evaluate (obj ! "model") :: JSB JSNumber

          switchB (res ! "id" :: JSString)
                  [ ("slider", update "model" (res ! "value") 0 25)
                  , ("up"    , update "model" (model + 1)     0 25)
                  , ("down"  , update "model" (model - 1)     0 25)
                  , ("reset" , update "model" 0               0 25)
                  ] $ return ()

          model <- evaluate (obj ! "model") :: JSB JSNumber
          jQuery "#slider"  >>= slider (cast model)
          liftJS $ do
                jQuery "#fib-out" >>= setHtml ("fib " <> cast model <> "...")
                res <- apply fib model
                jQuery "#fib-out" >>= setHtml ("fib " <> cast model <> " = " <> cast res)
                return ()
-}
      return ()

default(JSNumber, JSString, String)

----------------------------------------------------------------
{-
test1 :: forall o . JSTuple o => Proxy o -> IO ()
test1 Proxy = do
        txt <- sunroofCompileJSA def "main" $ do
                o :: o <- liftM cast $ new "Test" ()
                o' <- fn1 o
                alert(cast o' :: JSString)
        putStrLn txt

fn1 :: JSTuple o => o -> JS t o
fn1 o = do
        o' <- tuple (match o)
        return o'
-}