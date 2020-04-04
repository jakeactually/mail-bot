{-# LANGUAGE OverloadedStrings #-}

import Control.Monad
import Control.Concurrent
import qualified Data.ByteString as S
import qualified Data.ByteString.Lazy as B
import Data.Time.Clock
import Data.Time.LocalTime
import Data.Time.Calendar.WeekDate
import Network.HaskellNet.IMAP.SSL
import Network.HaskellNet.SMTP.SSL as SMTP
import Network.HaskellNet.Auth (AuthType(LOGIN))
import Network.Mail.Mime
import qualified Data.Text.Lazy as T

main :: IO ()
main = do
    putStrLn "mail bot started"
    loop

loop :: IO ()
loop = do
    now <- getCurrentTime
    timezone <- getCurrentTimeZone
    let localTime = utcToLocalTime timezone now
    let day = localDay localTime
    let (_, _, weekDay) = toWeekDate day
    let (TimeOfDay hour minute second) = localTimeOfDay localTime
    when (weekDay >= 1 && weekDay <= 5) $ do
        when (hour == 9 && minute == 0) $ clock True
        when (hour == 18 && minute == 0) $ clock False
    threadDelay $ 1000000 * 60
    main

from = "jcaicedo@eclipdata.com"
password = ""
to = ["ngruezo@eclipsoft.com", "hmoreno@eclipdata.com"]
subject = "Marcador"

clock :: Bool -> IO ()
clock isEntry = doSMTPSTARTTLS "smtp.office365.com" $ \c -> do
    authSucceed <- SMTP.authenticate LOGIN from password c
    renderedMail <- renderMail' $ newMail isEntry
    if authSucceed
      then sendMail from to (S.concat . B.toChunks $ renderedMail) c
      else print "Authentication error."

newMail :: Bool -> Mail
newMail isEntry = Mail (Address (Just "Jeyko Caicedo") "jcaicedo@eclipdata.com")
    [Address (Just "Nadia Gruezo") "ngruezo@eclipsoft.com"]
    [Address (Just "Hernan Moreno") "hmoreno@eclipdata.com"]
    []
    [("Subject", subject)]
    [[plainPart $ plainBody isEntry]]

plainBody :: Bool -> T.Text
plainBody True = T.pack $ "Buenos días,\n\n\
    \Este es mi correo de entrada\n\n\
    \Saludos,\n\n\
    \Jeyko"
plainBody False = T.pack $ "Buenos tardes,\n\n\
    \Este es mi correo de salida\n\n\
    \Saludos,\n\n\
    \Jeyko"
