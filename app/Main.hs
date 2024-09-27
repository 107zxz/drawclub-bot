{-# LANGUAGE OverloadedStrings #-}
module Main (main) where

import Control.Monad
import qualified Data.Text.IO as TIO

import Discord
import qualified Discord.Requests as R

import Debug.Trace
import Data.Text (pack)

import System.Random.Shuffle
import System.Random
import Text.Printf (printf)
import System.Environment (getEnv)


main :: IO ()
main = do
    gen <- newStdGen
    allPrompts <- readPrompts
    let numPrompts = 3
    let chosen = samplePrompts allPrompts gen numPrompts

    secret <- getEnv "DISCORD_SECRET"
    channelID <- getEnv "DISCORD_CHANNEL_ID"

    userFacingError <- runDiscord $ def
             { discordToken = pack secret
             , discordOnLog = \s -> TIO.putStrLn s >> TIO.putStrLn ""
             , discordOnStart = do sendPrompts chosen channelID; stopDiscord
             }

    TIO.putStrLn userFacingError

readPrompts :: IO [String]
readPrompts =
    lines <$> readFile "prompts.txt"

samplePrompts :: [String] -> StdGen -> Int -> [String]
samplePrompts prompts gen count = do
    take count $ shuffle' prompts (length prompts) gen

sendPrompts :: [String] -> String -> DiscordHandler ()
sendPrompts prompts channelId = do
    let promptstring = unwords ["\n**" ++ p ++ "**" | p <- prompts]
    let message = printf "This week's prompts are: " ++ promptstring

    sendMessage message channelId

sendMessage :: String -> String -> DiscordHandler ()
sendMessage message channelId =
    trace ("Sending message: " ++ message)
    void $ restCall $ R.CreateMessage (read channelId) (pack message)