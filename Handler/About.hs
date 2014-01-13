{-|
About page giving info about the web app.
-}
module Handler.About where

import Import

getAboutR :: Handler Html
getAboutR = do
  defaultLayout $ do
    [whamlet|
         <div .container>
           <div .page-header>
             <h1>
              About
           <p .lead>
             This web app allows users to upload and tag animation images. Intended as a demonstration of the animation image analysis algorithms I designed for tag prediction and other nice stuff :) .
    |]