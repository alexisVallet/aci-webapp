{-|
Widget including all necessary bootstrap scripts and stylesheets.
-}
module Handler.BootstrapWidget where

import Import

bootstrap :: Widget
bootstrap = do
  addStylesheet $ StaticR css_bootstrap_css
  addScript $ StaticR js_bootstrap_js
