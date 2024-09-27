{-# LANGUAGE OverloadedStrings #-}
module Main (main) where

import           Control.Monad
-- import           UnliftIO.Concurrent
import qualified Data.Text.IO as TIO

import           Discord
import qualified Discord.Requests as R

import Debug.Trace
import Data.Text (pack)

import System.Random.Shuffle
import System.Random
import Text.Printf (printf)


main :: IO ()
main = do

    gen <- newStdGen
    allPrompts <- readPrompts
    let chosen = samplePrompts allPrompts gen
    let numPrompts = 3

    secret <- readFile "secret.txt"

    userFacingError <- runDiscord $ def
             { discordToken = pack secret
             , discordOnLog = \s -> TIO.putStrLn s >> TIO.putStrLn ""
             , discordOnStart = do sendPrompts $ chosen numPrompts; stopDiscord
             } -- if you see OnLog error, post in the discord / open an issue

    TIO.putStrLn userFacingError
    -- userFacingError is an unrecoverable error
    -- put normal 'cleanup' code in discordOnEnd (see examples)

readPrompts :: IO [String]
readPrompts =
    lines <$> readFile "prompts.txt"

samplePrompts :: [String] -> StdGen -> Int -> [String]
samplePrompts prompts gen count = do
    take count $ shuffle' prompts (length prompts) gen

sendPrompts :: [String] -> DiscordHandler ()
sendPrompts prompts = do
    let promptstring = unwords ["\n**" ++ p ++ "**" | p <- prompts]
    let message = printf "Today's weekly prompts are: " ++ promptstring

    sendMessage message

sendMessage :: String -> DiscordHandler ()
sendMessage message =
    trace ("Sending message: " ++ message)
    void $ restCall $ R.CreateMessage (read "788239141548589060") (pack message)