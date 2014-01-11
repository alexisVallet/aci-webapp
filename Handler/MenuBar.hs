{-|
This module defines the menubar widget used by almost all handlers.
-}
module Handler.MenuBar where

import Yesod.Auth

import Import

import Handler.BootstrapWidget
import Handler.JQueryWidget

menuBar :: Widget
menuBar = do
  jQuery
  bootstrap
  toWidget $ [cassius|
   body
     padding-top: 40px
  |]
  muser <- handlerToWidget maybeAuth
  [whamlet|
   <div .navbar .navbar-default .navbar-fixed-top role=navigation>
     <div .container>
       <div .navbar-header>
         <button type=button .navbar-toggle data-toggle=collapse data-target=".navbar-collapse">
           <span .sr-only>Toggle navigation
           <span .icon-bar>
           <span .icon-bar>
           <span .icon-bar>
         <a .navbar-brand href=@{HomeR}>ACI Web application
       <div .collapse .navbar-collapse>
         <ul .nav .navbar-nav>
           <li>
             <a href=@{HomeR}>
               Home
           <li>
             <a href=@{AboutR}>
               About
           <li>
             $maybe Entity _ user <- muser
               <a href=@{AuthR LogoutR}>
                 Sign out #{userMail user}
             $nothing
               <a href=@{AuthR LoginR}>
                 Sign in
   |]