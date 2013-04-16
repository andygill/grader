{-# LANGUAGE OverloadedStrings, ScopedTypeVariables, TypeFamilies, TemplateHaskell #-}
{-# OPTIONS_GHC -fno-warn-missing-methods #-}

module Types where

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

newtype JSSlide = JSSlide JSObject

data Slide = Slide { id :: JSString, value :: JSNumber }

deriveJSTuple
  [d| instance JSTuple JSSlide where
          type Internals JSSlide = Slide
  |]

newtype JSDrag = JSDrag JSObject

data Drag = Drag { dragX :: JSNumber, dragY :: JSNumber }

deriveJSTuple
  [d| instance JSTuple JSDrag where
          type Internals JSDrag = Drag
  |]

----------------------------------------------------------------
{-
newtype JSStudent = JSStudent JSObject

data Student = Student
        { sUID  :: JSString             -- the key to get here
        , sName :: JSString             -- Normalized Name
        , sKUID :: JSString             -- textual version of number
        }

deriveJSTuple
  [d| instance JSTuple JSStudent where
          type Internals JSStudent = JSStudent
  |]
-}
----------------------------------------------------------------

newtype JSPosition = JSPosition JSObject

data Position = Position
  { pPage :: JSNumber
  , pX     :: JSNumber     -- where from the page we are viewing
  , pY     :: JSNumber     --
  , pScale :: JSNumber     -- zooming
  }

deriveJSTuple
  [d| instance JSTuple JSPosition where
          type Internals JSPosition = Position
  |]

----------------------------------------------------------------

newtype JSQA = JSQA JSObject

data QA = QA
  { qaQ :: JSString
  , qaA :: JSString
  }

deriveJSTuple
  [d| instance JSTuple JSQA where
          type Internals JSQA = QA
  |]

----------------------------------------------------------------

newtype JSServerResponse = JSServerResponse JSObject

data ServerResponse = ServerResponse
  { sUID :: JSString                    -- this person
  , sQA :: JSMap JSString JSString      -- has these Q and As
  }

deriveJSTuple
  [d| instance JSTuple JSServerResponse where
          type Internals JSServerResponse = ServerResponse
  |]

----------------------------------------------------------------

newtype JSModel = JSModel JSObject

data Model = Model
        { mQuestion :: JSString  -- The currently selected question number, always at least one selected
        , mPage  :: JSNumber     -- Page number of the exam (starts at 1)
        , mUID   :: JSString     -- location of the exam script

--        , mUIDs  :: JSMap JSString JSStudent

        , mX     :: JSNumber     -- where from the page we are viewing
        , mY     :: JSNumber     --
        , mScale :: JSNumber     -- zooming
        , mQA    :: JSQA
        , mComment :: JSMap JSString JSString
        , mStep :: JSNumber
        }

deriveJSTuple
  [d| instance JSTuple JSModel where
          type Internals JSModel = Model
  |]


--------------------------------------------------



newtype JSViewPort = JSViewPort JSObject

data ViewPort = ViewPort
        { vpFile  :: JSString   -- literal file to load
        , vpX     :: JSNumber
        , vpY     :: JSNumber
        , vpScale :: JSNumber   -- scale
        }

deriveJSTuple
  [d| instance JSTuple JSViewPort where
          type Internals JSViewPort = ViewPort
  |]

--------------------------------------------------

newtype JSMenu a = JSMenu JSObject

data Menu a = Menu
        { menuId     :: JSString   -- id of this menu
        , menuTable  :: JSMap JSString a
        , menuLookup :: JSMap a JSString
        }

deriveJSTuple
  [d| instance (SunroofKey o, Sunroof o) => JSTuple (JSMenu o) where
          type Internals (JSMenu o) = Menu o
  |]


newtype JSSelect a = JSSelect JSObject

data Select k = Select
        { selectId     :: JSString                        -- ^ DOM id of this menu (without #)

        , selectMap    :: JSMap k JSNumber                -- ^ from labels to the uniq number for this k
        , selectMap_1  :: JSArray k                       -- ^ and the reverse fn

        , selectText   :: JSArray JSString                -- ^ list of HTML text for each key

        , selectIds    :: JSArray JSString                -- ^ map form uniq-number to (sub) ids
        , selectIds_1  :: JSMap JSString JSNumber         -- ^ map from (sub) ids to the uniq-number

        , selectRender :: JSFunction (JSArray k) ()       -- render the widget
        }

deriveJSTuple
  [d| instance (SunroofKey o) => JSTuple (JSSelect o) where
          type Internals (JSSelect o) = Select o
  |]
