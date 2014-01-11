{-|
Widget including the jQuery javascript file.
-}
module Handler.JQueryWidget where

import Import

jQuery :: Widget
jQuery = do
  addScript $ StaticR js_jquery_1_10_2_js
