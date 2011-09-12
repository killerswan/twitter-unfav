-- |
-- Module      : Main
-- Copyright   : (c) 2011 Kevin Cantu
--
-- License     : BSD-style
-- Maintainer  : Kevin Cantu <me@kevincantu.org>
-- Stability   : experimental
--
-- A program to clear out old Twitter favorites


--module Main (main) where

import qualified Data.ByteString.Lazy as B
import System
import System.IO
import IO
import System.Console.GetOpt
import qualified Data.Text.Lazy as T
import qualified Data.Text.Lazy.Encoding as E
import Data.Word

import Web.Twitter         -- provided by askitter
import Web.Twitter.OAuth   -- provided by askitter
import Data.Time.Parse
import Data.Time.LocalTime
import Data.Time.Clock
import Data.Time.Calendar
import Data.Time.Calendar


version = "0.1"


-- command line options
data Options = Options { genMode          :: Bool
                       , cleanMode        :: Bool
                       , consumerKey      :: String
                       , consumerSecret   :: String
                       }


-- command line defaults
defaultOpts :: Options
defaultOpts = Options { genMode          = False
                      , cleanMode        = False
                      , consumerKey      = ""
                      , consumerSecret   = ""
                      }


-- command line description
-- this format is kinda bone headed:
--   [Option short [long] (property setter-function hint) description]
options :: [ OptDescr (Options -> IO Options) ]
options = [
            -- GEN MODE
            Option "g" ["generate"] 
                   (NoArg $ \opt -> return opt { genMode = True })
                   $ "toggle generate mode, to save a Twitter API token\n" ++ 
                     "(requires you specify a KEY and SECRET, too)"

          , Option "k" ["key"] 
                   (ReqArg (\arg opt -> return opt { consumerKey = arg }) "KEY")
                   "a consumer key for this app"

          , Option "s" ["secret"] 
                   (ReqArg (\arg opt -> return opt { consumerSecret = arg }) "SECRET")
                   "a consumer secret for this app"

            -- CLEAN MODE
          , Option "x" ["clean"] 
                   (NoArg $ \opt -> return opt { cleanMode = True })
                   "toggle clean up mode, to remove a saved token"

            -- HELP
          , Option "h" ["help"] 
                   (NoArg  $ \_ -> do
                        prg <- getProgName
                        hPutStrLn stderr $ usageInfo prg options
                        exitWith ExitSuccess)
                   "display help"

            -- VERSION
          , Option "v" ["version"] 
                   (NoArg $ \_ -> do
                        me <- getProgName
                        hPutStrLn stderr $ me ++ " version " ++ version
                        exitWith ExitSuccess)
                   "display version"
          ]


generateAndSaveToken key secret = 
   do 
      token <- authenticate $ Consumer key secret
      name  <- getProgName
      writeToken token $ name ++ ".token"
      -- TODO: this is not the best way or place to save the token


getFavorites' count = getProgName >>= \name -> readToken (name ++ ".token") >>= (getFavorites [Count count])


printLocalNow = 
   do
      now <- getCurrentTime
      zone <- getTimeZone now
      
      let now' = utcToLocalTime zone now

      putStrLn (shows now' "")


printTwoWeeksAgo =
   do
      now <- getCurrentTime

      let twoWeeks = 14 * (60 * 60 * 24) :: NominalDiffTime
      let twoWeeksAgo = addUTCTime (-twoWeeks) now

      putStrLn (shows twoWeeksAgo "")


deleteOldFavorites =
   do
      favs <- getFavorites' 1
      let ctime = fcreated_at $ favs !! 0
   
      printTwoWeeksAgo

{-
      let ctime' = strptime "%a %b %d %T %z %Y" ctime
      if ctime' <= (now - oneday)
      then
         putStrLn "(more than a day old)"
      else
         putStrLn "(less than a day old)"
-}


   -- > readFavorites
   -- e.g. [Favorite {fcreated_at = "Mon Jul 25 08:51:41 +0000 2011", fid_str = "95415914727616512"},
   --       Favorite {fcreated_at = "Mon Jul 25 08:38:07 +0000 2011", fid_str = "95412499763036160"}]
   -- 
   -- also:
   -- e.g. Just (2011-07-26 05:08:24,"")
   

main :: IO ()
main = 
   do 
      args <- getArgs

      -- call getOpt, ignoring errors
      let (actions, nonOptions, _) = getOpt Permute options args

      -- process the defaults with those actions
      opts <- foldl (>>=) (return defaultOpts) actions

      case (genMode opts, cleanMode opts) of
         (True, True)   -> error "gen and clean modes are mutually exclusive..."

         (True, False)  -> if consumerKey opts == "" || consumerSecret opts == ""
                           then error "to make a token, you need an OAuth consumer key and secret..."
                           else generateAndSaveToken (consumerKey opts) (consumerSecret opts)

         (False, True)  -> putStrLn "clean mode, yay!" -- TODO

         _              -> deleteOldFavorites


