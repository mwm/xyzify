{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE QuasiQuotes #-}

module Web where

import Prelude hiding (concat)

import Data.ByteString.Lazy.Char8 (ByteString, unpack)
import Data.Conduit (runResourceT, ($$))
import Data.Conduit.Binary (sinkLbs)
import Data.Text (Text, concat, stripSuffix)
import Data.Text.Encoding (encodeUtf8)
import Yesod

import GCode

data App = App

instance Yesod App

instance RenderMessage App FormMessage where
  renderMessage _ _ = defaultFormMessage

mkYesod "App" [parseRoutes|/ HomeR GET POST|]

main :: IO ()
main = warpEnv App

getHomeR :: Handler Html
getHomeR = do
    (formWidget, formEncType) <- generateFormPost uploadForm
    defaultLayout $ do
        setTitle "XYZifier"
        toWidget [whamlet|
<h1>XYZifier
<form method=post action=@{HomeR} enctype=#{formEncType}>
  ^{formWidget}
  <input type="submit" value="Convert">
|]

postHomeR :: Handler Html
postHomeR = do
    ((result, _), _) <- runFormPost uploadForm
    case result of
      FormSuccess fi -> do
        output <- fmap makeXYZ . runResourceT $ fileSource fi $$ sinkLbs
        addHeader "Content-Disposition" $ concat
            [ "attachment; filename=\"", trunc (fileName fi), ".3w\""]
        sendResponse (encodeUtf8 (fileContentType fi), toContent output)
      _ -> return ()
    redirect HomeR

trunc :: Text -> Text
trunc up = maybe up id (stripSuffix ".gcode" up)

uploadForm = renderDivs $ fileAFormReq "File: "
