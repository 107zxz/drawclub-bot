{-# LANGUAGE OverloadedStrings #-}
module Main (main) where

import Control.Monad
import qualified Data.Text.IO as TIO

import Discord
import qualified Discord.Requests as R

import Debug.Trace
import Data.Text (pack)

import System.Random
import Text.Printf (printf)
import System.Environment (getEnv)


main :: IO ()
main = do
    nouns <- readPrompts "nouns.txt"
    verbs <- readPrompts "verbs.txt"
    adjectives <- readPrompts "adjectives.txt"

    chosen <- mapM samplePrompt [verbs, adjectives, nouns]

    secret <- getEnv "DISCORD_SECRET"
    channelID <- getEnv "DISCORD_CHANNEL_ID"

    userFacingError <- runDiscord $ def
             { discordToken = pack secret
             , discordOnLog = \s -> TIO.putStrLn s >> TIO.putStrLn ""
             , discordOnStart = do sendPrompts chosen channelID; stopDiscord
             }

    TIO.putStrLn userFacingError

readPrompts :: FilePath -> IO [String]
readPrompts path =
    lines <$> readFile path

samplePrompt :: [String] -> IO String
samplePrompt prompts = do
    n <- randomRIO (0,  len)
    return $ prompts !! n
    where len = length prompts

sendPrompts :: [String] -> String -> DiscordHandler ()
sendPrompts prompts =
    sendMessage message
    where   promptString = concat ["\n**" ++ p ++ "**" | p <- prompts]
            message = printf "This week's prompts are: " ++ promptString

sendMessage :: String -> String -> DiscordHandler ()
sendMessage message channelId =
    trace ("Sending message: " ++ message)
    void $ restCall $ R.CreateMessage (read channelId) (pack message)