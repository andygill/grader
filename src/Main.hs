{-# LANGUAGE OverloadedStrings, ScopedTypeVariables, TypeFamilies, TemplateHaskell #-}

module Main where

import Data.Default ( Default(..) )
import Data.Semigroup ( (<>) )
import Control.Monad
import Data.Boolean
import Data.Boolean.Numbers as N
import Data.Proxy

import Language.Sunroof
import Language.Sunroof.Server
import Language.Sunroof.Compiler
import Language.Sunroof.JS.Canvas
import Language.Sunroof.JS.Browser as B
import Language.Sunroof.JS.JQuery
import Network.Wai.Middleware.Static

import TH

type  Slide2 = (JSString,JSNumber)

$(jstuple ''Slide2)

newtype Slide = Slide JSObject

derive [d| instance Show Slide |]

newtype JSX a = JSX JSObject

-- | Show the Javascript.
derive [d| instance (SunroofArgument o) => Show (JSX o) |]
derive [d| instance (SunroofArgument o) => Sunroof (JSX o) |]

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



instance Sunroof Slide where
  box   = Slide . box
  unbox (Slide o) = unbox o
  typeOf _ = Base

instance JSTuple Slide where
  type Internals Slide = (JSString,JSNumber)
  match o = (o ! "id", o ! "value")
  tuple (id_,num) = do
    o <- new "Object" ()
    o # "id" := id_
    o # "value" := num
    return $ Slide o

newtype JSViewPort = JSViewPort JSObject

instance Sunroof JSViewPort where
  box   = JSViewPort . box
  unbox (JSViewPort o) = unbox o

data ViewPort = ViewPort
        { vpPage  :: JSNumber   -- page number, first page is page zero
        , vpX     :: JSNumber
        , vpY     :: JSNumber
        , vpScale :: JSNumber   -- scale
        }

instance JSTuple JSViewPort where
  type Internals JSViewPort = ViewPort
  match o = ViewPort
                { vpPage  = o ! "page"
                , vpX     = o ! "x"
                , vpY     = o ! "y"
                , vpScale = o ! "scale"
                }
  tuple vp = do
    o <- new "Object" ()
    o # "page"  := vpPage  vp
    o # "x"     := vpX     vp
    o # "y"     := vpY     vp
    o # "scale" := vpScale vp
    return $ JSViewPort o

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
                tuple (the_id,the_index) >>= \ (o :: Slide) -> ch # writeChan o)

      forkJS $ loop () $ \ () -> do
        o :: Slide <- ch # readChan
        let (the_id,aux) = match o
        console # B.log ("Slide : " <> cast the_id <> " " <> cast aux :: JSString)
        return ()

      -- channel of what to show on the graphical viewport
      viewportChan :: JSChan JSViewPort <- newChan


      -- global of the current viewer state
      vp :: JSViewPort <- tuple $ ViewPort 0 400 100 2.0
      viewerState :: JSMVar JSViewPort <- newMVar vp


      -- global state changers
      setPage :: JSContinuation JSNumber <- continuation $ \ (n :: JSNumber) -> do
              vp :: ViewPort <- (viewerState # takeMVar) >>= (return . match)
              let vp' = vp { vpPage = n }
              return ()

      -- The cached image, a non-connected DOM object, that is also an <img>
      imageObj :: JSObject <- new "Image" ()

      canvas <- document # getElementById("myCanvas")
      context <- canvas # getContext("2d")

      drawViewPort <- continuation $ \ () -> do
              o <- viewportChan # readChan
              let vp = match o

              -- figure out if we have the correct image loaded
              -- TODO

              -- load new image object
              imageObj # src := "pages/exam.300-2.png";

              jq (cast $ imageObj) >>= on "load" "" (\ () -> do
                        alert("loaded image")
                        context # drawImageClip imageObj (400, 100) (2050, 1490) (0, 0) (960, 600))

              return ()

      forkJS (goto drawViewPort () :: JSB ())

      viewportChan # writeChan vp

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
