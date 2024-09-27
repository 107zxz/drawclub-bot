{-# LANGUAGE OverloadedStrings #-}  -- allows "string literals" to be Text
module Main (main) where

import           Control.Monad
-- import           UnliftIO.Concurrent
import qualified Data.Text.IO as TIO
import qualified Data.Text as T

import           Discord
import qualified Discord.Requests as R

import Debug.Trace
import Data.Text (pack)

main :: IO ()
main = do

    -- allPrompts <- readPrompts
    -- let idxs = randomRs (0 length allPrompts) getStdGen :: [Int]
    -- bigThree <- take 3 $ [allPrompts !! x | x <- randomRs (0, length allPrompts) getStdGen]

    secret <- readFile "secret.txt"

    userFacingError <- runDiscord $ def
             { discordToken = pack secret
             , discordOnLog = \s -> TIO.putStrLn s >> TIO.putStrLn ""
             , discordOnStart = do sendPrompts; stopDiscord
             } -- if you see OnLog error, post in the discord / open an issue

    TIO.putStrLn userFacingError
    -- userFacingError is an unrecoverable error
    -- put normal 'cleanup' code in discordOnEnd (see examples)

-- readPrompts :: IO [String]
-- readPrompts =
    -- lines <$> readFile "prompts.txt"

-- samplePrompts :: [String] -> [String]
-- samplePrompts prompts =
--     random

sendPrompts :: DiscordHandler ()
sendPrompts =
    sendMessage "Fuckk yeah!"

sendMessage :: T.Text -> DiscordHandler ()
sendMessage message =
    trace ("Sending message: " ++ show message)
    void $ restCall $ R.CreateMessage (read "788239141548589060") message