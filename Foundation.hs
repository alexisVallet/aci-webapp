module Foundation where

import Prelude
import Yesod
import Yesod.Static
import Yesod.Auth
import Yesod.Auth.GoogleEmail
import Yesod.Default.Config
import Yesod.Default.Util (addStaticContentExternal)
import Network.HTTP.Conduit (Manager)
import qualified Settings
import Settings.Development (development)
import qualified Database.Persist
import Database.Persist.Sql (SqlPersistT)
import Settings.StaticFiles
import Settings (widgetFile, Extra (..))
import Model
import Text.Jasmine (minifym)
import Text.Hamlet (hamletFile)
import Yesod.Core.Types (Logger)

-- | The site argument for your application. This can be a good place to
-- keep settings and values requiring initialization before your application
-- starts running, such as database connections. Every handler will have
-- access to the data present here.
data App = App
    { settings :: AppConfig DefaultEnv Extra
    , getStatic :: Static -- ^ Settings for static file serving.
    , connPool :: Database.Persist.PersistConfigPool Settings.PersistConf -- ^ Database connection pool.
    , httpManager :: Manager
    , persistConfig :: Settings.PersistConf
    , appLogger :: Logger
    }

-- Set up i18n messages. See the message folder.
mkMessage "App" "messages" "en"

-- This is where we define all of the routes in our application. For a full
-- explanation of the syntax, please see:
-- http://www.yesodweb.com/book/routing-and-handlers
--
-- Note that this is really half the story; in Application.hs, mkYesodDispatch
-- generates the rest of the code. Please see the linked documentation for an
-- explanation for this split.
mkYesodData "App" $(parseRoutesFile "config/routes")

type Form x = Html -> MForm (HandlerT App IO) (FormResult x, Widget)

-- | Widget including the jQuery script files.
jQuery :: Widget
jQuery = do
  addScript $ StaticR js_jquery_1_10_2_js

-- | Widget including the bootstrap css and script files.
bootstrap :: Widget
bootstrap = do
  addStylesheet $ StaticR css_bootstrap_css
  addScript $ StaticR js_bootstrap_js

-- | Navbar for the default layout.
navbar :: Widget
navbar = do
  jQuery
  bootstrap
  toWidget $ [cassius|
   body
     padding-top: 60px
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
           $maybe Entity _ _ <- muser
             <li>
               <a href=@{UploadR}>
                 Upload image
   |]

-- Please see the documentation for the Yesod typeclass. There are a number
-- of settings which can be configured by overriding methods here.
instance Yesod App where
    approot = ApprootMaster $ appRoot . settings

    -- Store session data on the client in encrypted cookies,
    -- default session idle timeout is 120 minutes
    makeSessionBackend _ = fmap Just $ defaultClientSessionBackend
        (120 * 60) -- 120 minutes
        "config/client_session_key.aes"
    -- | Default layout with navbar and copyright notice. Automatically includes
    -- bootstrap and jQuery.
    defaultLayout widget = do
        master <- getYesod
        mmsg <- getMessage

        -- We break up the default layout into two components:
        -- default-layout is the contents of the body tag, and
        -- default-layout-wrapper is the entire page. Since the final
        -- value passed to hamletToRepHtml cannot be a widget, this allows
        -- you to use normal widget features in default-layout.

        pc <- widgetToPageContent $ do
            $(combineStylesheets 'StaticR
                [ css_normalize_css
                ])
            $(widgetFile "default-layout")
        giveUrlRenderer $(hamletFile "templates/default-layout-wrapper.hamlet")

    -- This is done to provide an optimization for serving static files from
    -- a separate domain. Please see the staticRoot setting in Settings.hs
    urlRenderOverride y (StaticR s) =
        Just $ uncurry (joinPath y (Settings.staticRoot $ settings y)) $ renderRoute s
    urlRenderOverride _ _ = Nothing

    -- The page to be redirected to when authentication is required.
    authRoute _ = Just $ AuthR LoginR

    -- This function creates static content files in the static folder
    -- and names them based on a hash of their content. This allows
    -- expiration dates to be set far in the future without worry of
    -- users receiving stale content.
    addStaticContent =
        addStaticContentExternal minifym genFileName Settings.staticDir (StaticR . flip StaticRoute [])
      where
        -- Generate a unique filename based on the content itself
        genFileName lbs
            | development = "autogen-" ++ base64md5 lbs
            | otherwise   = base64md5 lbs

    -- Place Javascript at bottom of the body tag so the rest of the page loads first
    jsLoader _ = BottomOfBody

    -- What messages should be logged. The following includes all messages when
    -- in development, and warnings and errors in production.
    shouldLog _ _source level =
        development || level == LevelWarn || level == LevelError

    makeLogger = return . appLogger
    -- | Checks authorization for upload and modification of images.
    isAuthorized UploadR _ = isLogged
    isAuthorized (ImageR imageId) True = isUploader imageId
    isAuthorized _ _ = return Authorized

-- | Checks that the user is logged, and return the corresponding
-- authorization result.
isLogged :: Handler AuthResult
isLogged = do
  mu <- maybeAuthId
  return $ case mu of
    Nothing -> AuthenticationRequired
    Just _ -> Authorized

-- | Checks that the user is logged and is the uploader of a specific image.
isUploader :: ImageId -> Handler AuthResult
isUploader imageId = do
  mUser <- maybeAuth
  case mUser of
    Nothing -> return AuthenticationRequired
    Just (Entity userId _) -> do
      mImage <- runDB $ get imageId
      case mImage of
        Nothing -> return $ Unauthorized "Image doesn't exist."
        Just image -> do
          if userId == imageUploader image
            then return Authorized
            else return $ Unauthorized "You are not allowed to modify this image."

-- How to run database actions.
instance YesodPersist App where
    type YesodPersistBackend App = SqlPersistT
    runDB = defaultRunDB persistConfig connPool
instance YesodPersistRunner App where
    getDBRunner = defaultGetDBRunner connPool

instance YesodAuth App where
    type AuthId App = UserId

    -- Where to send a user after successful login
    loginDest _ = HomeR
    -- Where to send a user after logout
    logoutDest _ = HomeR

    getAuthId creds = runDB $ do
        x <- getBy $ UniqueUser $ credsIdent creds
        case x of
            Just (Entity uid _) -> return $ Just uid
            Nothing -> do
                fmap Just $ insert $ User (credsIdent creds)

    -- You can add other plugins like BrowserID, email or OAuth here
    authPlugins _ = [authGoogleEmail]
    loginHandler = lift $ do
      master <- getYesod
      defaultLayout $ do
        [whamlet|
         <div .container>
           <div .page-header>
             <h1>
               Login
           <p .lead>
             Please log in using one of the following method:
           $forall plugin <- authPlugins master
             ^{apLogin plugin AuthR}
        |]
                         
    authHttpManager = httpManager

-- This instance is required to use forms. You can modify renderMessage to
-- achieve customized and internationalized form validation messages.
instance RenderMessage App FormMessage where
    renderMessage _ _ = defaultFormMessage

-- | Get the 'Extra' value, used to hold data from the settings.yml file.
getExtra :: Handler Extra
getExtra = fmap (appExtra . settings) getYesod

-- Note: previous versions of the scaffolding included a deliver function to
-- send emails. Unfortunately, there are too many different options for us to
-- give a reasonable default. Instead, the information is available on the
-- wiki:
--
-- https://github.com/yesodweb/yesod/wiki/Sending-email
