{-# LANGUAGE OverloadedStrings, ScopedTypeVariables, TypeFamilies, TemplateHaskell, ViewPatterns #-}

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
import Language.Sunroof.JS.Map
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
imageDpi :: JSNumber
imageDpi = 300

monitorDpi :: JSNumber
monitorDpi = 75


mkSliderBar :: (Fractional n) => JSString -> n -> n -> n -> SliderBar n
mkSliderBar name res mn mx = SliderBar name res fn inv_fn
  where
      fn     x = x * ((mx - mn) / (res - 1)) + mn
      inv_fn x = (x - mn) * (res - 1) / (mx - mn)

data SliderBar n = SliderBar JSString n (n -> n) (n -> n)

fnSliderBar :: SliderBar n -> n -> n
fnSliderBar (SliderBar _ _ fn _) = fn

invFnSliderBar :: SliderBar n -> n -> n
invFnSliderBar (SliderBar _ _ _ fn) = fn

test1 :: SliderBar Float
test1 = mkSliderBar "X" 101 0.5 3

initSlider :: SliderBar JSNumber -> JS t ()
initSlider (SliderBar nm mx _ _) = do
        () <- jq nm >>= invoke "slider" ("option" :: JSString,"min" :: JSString,0      :: JSNumber)
        () <- jq nm >>= invoke "slider" ("option" :: JSString,"max" :: JSString,mx - 1 :: JSNumber)
        () <- jq nm >>= invoke "slider" ("option" :: JSString,"value" :: JSString,0    :: JSNumber)
        return ()

setSlider :: SliderBar JSNumber -> JSNumber -> JS t ()
setSlider (SliderBar nm _ _ fn) n = do
        jq nm >>= invoke "slider" ("option" :: JSString,"value" :: JSString,fn n :: JSNumber)


newMenu :: (SunroofKey k) => JSString -> JS t (JSMenu k)
newMenu dom_id = do
        tbl <- newMap
        lk  <- newMap
        tuple (Menu dom_id tbl lk)

addMenuItem :: (SunroofKey k) => k -> JSString -> JSMenu k -> JS t ()
addMenuItem key val (match -> Menu dom_id tbl lk) = do
       jq (dom_id) >>= append (cast ("<option>" <> val <> "</option>" :: JSString) :: JSObject)
       tbl # insertMap val key
       lk # insertMap key val

-- Which item is selected right now, defaults to first
whichMenuItem :: (SunroofKey k) => JSMenu k -> JS t k
whichMenuItem (match -> Menu dom_id tbl lk) = do
        txt :: JSString <- jq (dom_id <> " option:selected") >>= invoke "attr" ("value" :: JSString)
        tbl # lookupMap txt

selectMenuItem :: (SunroofKey k) => k -> JSMenu k -> JS t ()
selectMenuItem k (match -> Menu dom_id tbl lk) = do
        txt <- lk # lookupMap k
        o <- jq dom_id
        o :: JSObject <- o # invoke "val" (txt :: JSString)
        o # invoke "attr" ("selected"::JSString,true::JSBool)
--      $("#selectID").val( theValue ).attr('selected',true);


prog :: JSA ()
prog = do
      -- set up the slider(s)

      let pageSlider     :: SliderBar JSNumber = mkSliderBar "#page-slider"  6   1   6
      let scaleSlider    :: SliderBar JSNumber = mkSliderBar "#scale-slider" 101 2   4
      let questionSlider :: SliderBar JSNumber = mkSliderBar "#question-slider" 10  1   10

      initSlider pageSlider
      initSlider scaleSlider
      initSlider questionSlider

      pageMenu <- newMenu "#page-select"

      ch <- newChan

      pageDB <- new "Object" ()

      sequence [ do pageMenu # addMenuItem (js n) ("Page #" <> cast (js n) :: JSString)
               | n :: Int <- [1..6]
               ]




      -- $("#page-select option:selected").attr("id")

--      var theValue = "whatever";
--      $("#selectID").val( theValue ).attr('selected',true);


      -- set up the slider listener
      jq "body" >>= on "slide" ".slide" (\ (a :: JSObject, aux :: JSObject) -> do
                the_id    :: JSString <- jq (cast $ this) >>= invoke "attr" ("id" :: JSString)
                the_index :: JSNumber <- evaluate $ N.round (aux ! "value" :: JSNumber)
                tuple (Slide the_id the_index) >>= \ (o :: JSSlide) -> ch # writeChan o)

      let offset :: JSSelector JSObject
          offset = "offset"

          top, left :: JSSelector JSNumber
          top = "top"
          left = "left"

      offsetStart :: JSRef JSObject <- newJSRef nullJS

      -- the Drag Channel
      dragCh <- newChan
      jq "#myCanvas" >>= on "dragstart" "" (\ (event :: JSObject, aux:: JSObject) -> do
                        offsetStart # writeJSRef (aux ! offset))

      jq "#myCanvas" >>= on "drag" "" (\ (event :: JSObject, aux:: JSObject) -> do
                        start <- offsetStart # readJSRef
                        end <- evaluate (aux ! offset)
                        let drag = Drag ((end ! left) - (start ! left)) ((end ! top) - (start ! top))
                        console # B.log ("offset " <> cast (dragX drag)
                                            <> " " <> cast (dragY drag)
                                            :: JSString)
                        tuple drag >>= \ (o::JSDrag) -> dragCh # writeChan o
                        offsetStart # writeJSRef end
                )

      -- channel of what to show on the graphical viewport
      viewportChan :: JSChan JSViewPort <- newChan

      -- Here is the model
      let model = Model
                { mQuestion = 1
                , mPage  = 3
                , mUID   = ""
                , mX     = 400
                , mY     = 100
                , mScale = 2.0
                }
      jsModel :: JSModel <- tuple model

      modelChan :: JSChan (JSFunction JSModel JSModel) <- newChan

      let upModel :: (SunroofThread t) => (Model -> JSA Model) -> JS t ()
          upModel f = do
                  g <- function (\ m -> do
                                let jsm = match m
                                jsm' <- f jsm
                                tuple jsm')
                  modelChan # writeChan g

      forkJS $ loop jsModel $ \ m -> do
              up <- modelChan # readChan
              console # B.log ("updateModel" :: JSString)
              m' <- up $$ m
              let jsm = match m'
              -- Display the model
              txt <- fun "$.toJSON" $$ m'
              console # B.log ("MODEL: " <> txt :: JSString)

              -- and propogate the model
              vp :: JSViewPort <- tuple
                        $ ViewPort ("/pages/exam.300-" <> cast (mPage jsm - 1) <> ".png")
                                   (mX jsm)
                                   (mY jsm)
                                   (mScale jsm)
              viewportChan # writeChan vp

              -- Write the side boxes and sliders
              jq("#page-slider-counter") >>= setHtml("" <> cast (mPage jsm) :: JSString)
              setSlider pageSlider (mPage jsm)
--              jq(("#page-select") >>= invoke "val" ( pageDB ! mPage jsm ).attr('selected',true);


              let precision :: JSNumber -> JSB JSString
                  precision n = ifB (n <* 1.0)
                                    (n # invoke "toPrecision" (2 :: JSNumber))
                                    (n # invoke "toPrecision" (3 :: JSNumber))

              scale_txt :: JSString <- mScale jsm # precision

              jq("#scale-slider-counter") >>= setHtml("" <> cast (scale_txt) :: JSString)
              setSlider scaleSlider (mScale jsm)

              jq("#question-slider-counter") >>= setHtml("" <> cast (mQuestion jsm) :: JSString)
              setSlider questionSlider (mQuestion jsm)

              return m'

      -- listen on sliders
      forkJS $ loop () $ \ () -> do
        o :: JSSlide <- ch # readChan
        let (Slide the_id aux) = match o
        console # B.log ("Slide : " <> cast the_id <> " " <> cast aux :: JSString)
        switch the_id [ ("page-slider", upModel $ \ jsm -> return $ jsm { mPage  = fnSliderBar pageSlider aux
                                                                        , mX     = mX model
                                                                        , mY     = mY model
                                                                        , mScale = mScale model
                                                                        })
                      , ("scale-slider", upModel $ \ jsm -> return $ jsm { mScale = fnSliderBar scaleSlider aux })
                      , ("question-slider", upModel $ \ jsm -> return $ jsm { mQuestion = fnSliderBar questionSlider aux })
                      ]

      -- listen of drags
      forkJS $ loop () $ \ () -> do
        o :: JSDrag <- dragCh # readChan
        let (Drag dx dy) = match o
        console # B.log ("Drag : " <> cast dx <> " " <> cast dy :: JSString)
        upModel $ \ jsm -> return $ jsm { mX = mX jsm - dx / ((monitorDpi / imageDpi) * mScale jsm), mY = mY jsm - dy / ((monitorDpi / imageDpi) * mScale jsm) }






      return ()



{-
      -- global state changers
      setPage :: JSContinuation JSNumber <- continuation $ \ (n :: JSNumber) -> do

              vp :: ViewPort <- (viewerState # takeMVar) >>= (return . match)
              let vp' = vp { vpPage = n }
              return ()
-}
      -- The cached image, a non-connected DOM object, that is also an <img>
      imageObj    :: JSObject <- new "Image" ()
      imageAnchor :: JSObject <- document # createElement "a"
      imageLoad   :: JSMVar () <- newEmptyMVar

      jq (cast $ imageObj) >>= on "load" "" (\ () -> do
                              console # B.log ("on-load" :: JSString)
                              imageLoad # putMVar ()
                              console # B.log ("pushed from on-load" :: JSString)
                              )

      canvas <- document # getElementById("myCanvas")
      context <- canvas # getContext("2d")


      forkJS $ loop () $ \ () -> do
              console # B.log ("starting draw image loop" :: JSString)
              -- First get the event
              o <- viewportChan # readChan
              let vp = match o

              -- figure out if we have the correct image loaded

              let scale = (monitorDpi / imageDpi) * vpScale vp :: JSNumber-- vpScale vp

              console # B.log ("scale " <> cast scale :: JSString)

              paint <- function $ \ () -> do
                      h :: JSNumber <- evaluate $ imageObj ! "height"
                      w :: JSNumber <- evaluate $ imageObj ! "width"
                      let x0 :: JSNumber = 0
                          y0 :: JSNumber = 0
                          x1 = h - 1
                          y1 = h - 1
                          boundX = maxB x0 . minB x1
                          boundY = maxB y0 . minB y1

                          startX = boundX $ vpX vp
                          startY = boundY $ vpY vp

                          endX = boundX $ vpX vp + 960 / scale
                          endY = boundY $ vpY vp + 600 / scale

                      context # drawImageClip imageObj (startX,startY)
                                                       (endX - startX,endY - startY)
                                                       (0, 0)
                                                       (960, 600)
                      console # B.log ("painted" :: JSString)



              console # B.log ("internals : " <> cast (imageObj ! "complete" :: JSBool)
                                  <> " " <> cast (imageObj ! src :: JSString)
                                  <> " " <> cast (imageAnchor ! "pathname" :: JSString)
                                  <> " " <> cast (vpFile vp) :: JSString)

              ifB ((imageAnchor ! "pathname") /=* vpFile vp)
                  (do  -- need to reload image
                      imageObj # src := vpFile vp
                      imageAnchor # "href" := vpFile vp
                      -- wait till we've loaded the image
                      imageLoad # takeMVar
                      -- *THEN* paint
                      apply paint ()
                 )
                 (return ())

              apply paint ()

              -- and go again
              console # B.log ("end of loop" :: JSString)
              return ()

      -- read the changes in menu

      jq "body" >>= on "change" ".change" (\ (a :: JSObject, aux :: JSObject) -> do
                the_id    :: JSString <- jq (cast $ this) >>= invoke "attr" ("id" :: JSString)
                console # B.log ("change:" :: JSString)
                console # B.log (the_id)
                sub_id    :: JSString <- jq ("#" <> the_id <> " option:selected") >>= invoke "attr" ("value" :: JSString)
                console # B.log (sub_id)

                n :: JSNumber <- evaluate $ pageDB ! label sub_id

                upModel $ \ jsm -> return $ jsm { mPage  = n
                                                , mX     = mX model
                                                , mY     = mY model
                                                , mScale = mScale model
                                                }

                -- Now we have a name and a inside to the name

--              the_index :: JSNumber <- evaluate $ N.round (aux ! "value" :: JSNumber)
--              tuple (Slide the_id the_index) >>= \ (o :: JSSlide) -> ch # writeChan o
--              console # B.log (the_index)
          )



      -- null update, to do first redraw
      upModel $ return

      return ()

default(JSNumber, JSString, String)

