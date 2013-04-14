{-# LANGUAGE ViewPatterns #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Select where

import Data.Default ( Default(..) )
import Data.Semigroup ( (<>) )
import Control.Monad
import Data.Boolean
import Data.Boolean.Numbers as N

import Language.Sunroof.JS.Browser as B
import Language.Sunroof as SR
import Language.Sunroof.Server
import Language.Sunroof.Compiler
import Language.Sunroof.TH
import Language.Sunroof.JS.Canvas
import qualified Language.Sunroof.JS.Map as M
import Language.Sunroof.JS.Browser as B
import Language.Sunroof.JS.JQuery
import Network.Wai.Middleware.Static

import Types

newSelect :: (SunroofKey k) => JSString -> JS t (JSSelect k)
newSelect name = do
  key_fm <- M.newMap
  key_fm_1 <- empty
  txt_arr <- empty
  sel_ids <- empty
  sel_ids_1 <- M.newMap
  render <- function $ \ arr -> do
        console # B.log ("render backcall : " <> cast (arr ! length') :: JSString)

        jq ("#" <> name) >>= setHtml ("<ul></ul>")
        arr # forEach (\ k -> do
                kId :: JSNumber <- key_fm # M.lookup k
                val :: JSString <- evaluate $ SR.lookup' kId txt_arr
                let str :: JSString
                    str = "<li>"
                       <> "<a class=\"click\" id=\""
                       <> name <> "-" <> cast kId
                       <> "\">"
                       <> val
                       <> "</a></li>"

                jq ("#" <> name <> " ul") >>= append (cast str)

                return ())

        return ()

  tuple $ Select
        { selectId     = name
        , selectMap    = key_fm
        , selectMap_1  = key_fm_1
        , selectText   = txt_arr
        , selectIds    = sel_ids
        , selectIds_1  = sel_ids_1
        , selectRender = render
        }

-- Add a specific entry into our dictionary
insertOption :: (SunroofKey k) => k -> JSString -> JSSelect k -> JS t ()
insertOption key txt (match -> sel) = do
        -- first, push the key and txt which also gets the new id-#
        sz <- selectMap_1 sel # push key
        _  <- selectText sel # push txt

        let id_num = sz - 1
        let id_txt = selectId sel <> "-" <> cast id_num

        selectMap sel # M.insert key id_num
        selectIds sel # push id_txt
        selectIds_1 sel # M.insert id_txt id_num

        return ()

-- draws specific list of
drawSelect :: (SunroofKey k) => JSArray k -> JSSelect k -> JS t ()
drawSelect = invoke "selectRender"
--        arr (match -> sel) =
--        selectRender sel $$ arr

keysSelect :: (SunroofKey k) => JSSelect k -> JS t (JSArray k)
keysSelect (match -> sel) =
        -- techincally, this should clone the array
        evaluate $ selectMap_1 sel

idSelect :: (SunroofKey k) => k -> JSSelect k -> JS t JSString
idSelect k (match -> sel) = do
        idNo :: JSNumber <- selectMap sel # M.lookup k
        evaluate $ lookup' idNo (selectIds sel)

setClass :: k -> String -> JS t ()
setClass = undefined

resetClass :: k -> String -> JS t ()
resetClass = undefined

-- This gets called each time an option gets selected/clicked
addCallback :: (SunroofKey k) => (k -> JSB ()) -> JSSelect k -> JS t ()
addCallback callback (match -> sel) = do
      jq ("#" <> selectId sel) >>= on "click" ".click" (\ (a :: JSObject, aux :: JSObject) -> do
              the_id    :: JSString <- jq (cast $ this) >>= invoke "attr" ("id" :: JSString)

              let toJSON :: (Sunroof o) => o -> JS t JSString
                  toJSON x = fun "$.toJSON" $$ x

              txt <- toJSON (selectIds_1 sel)

              kId <- selectIds_1 sel # M.lookup the_id

              k <- evaluate $ lookup' kId (selectMap_1 sel)

              callback k
           )
--                the_index :: JSNumber <- evaluate $ N.round (aux ! "value" :: JSNumber)
--                tuple (Slide the_id the_index) >>= \ (o :: JSSlide) -> ch # writeChan o)









