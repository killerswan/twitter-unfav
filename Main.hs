-- |
-- Module      : Main
-- Copyright   : (c) 2011 Kevin Cantu
--
-- License     : BSD-style
-- Maintainer  : Kevin Cantu <me@kevincantu.org>
-- Stability   : experimental
--
-- A program to clear out old Twitter favorites


module Main (main) where

import System
import System.IO
import IO
import System.Console.GetOpt
import Data.Time.Parse
import Data.Time.LocalTime
import Data.Time.Clock
import Data.Time.Calendar
import Data.Time.Calendar
import Data.Maybe
import Web.Twitter         -- provided by askitter
import Web.Twitter.OAuth   -- provided by askitter


version = "0.2"


-- command line options
data Options = Options { genMode          :: Bool
                       , tokenFile        :: String
                       , consumerKey      :: String
                       , consumerSecret   :: String
                       }


-- command line defaults
defaultOpts :: Options
defaultOpts = Options { genMode          = False
                      , tokenFile        = error "no file specified..."
                      , consumerKey      = error "no consumer key specified..."
                      , consumerSecret   = error "no consumer secret specified..."
                      }


-- command line description
-- this format is kinda bone headed:
--   [Option short [long] (property setter-function hint) description]
options :: [ OptDescr (Options -> IO Options) ]
options =
   [
     -- REGULAR OR GEN MODE
     Option "f" ["file"] 
         (ReqArg (\arg opt -> return opt { tokenFile = arg }) "FILE")
         "name of a file where the token is (or will be) saved"

     -- GEN MODE
   , Option "g" ["generate"] 
         (NoArg $ \opt -> return opt { genMode = True })
         $ "toggle generate mode, to save a Twitter API token\n" ++ 
         "(requires a FILE, KEY, and SECRET, too)"

   , Option "k" ["key"] 
         (ReqArg (\arg opt -> return opt { consumerKey = arg }) "KEY")
         "a consumer key for this app"

   , Option "s" ["secret"] 
         (ReqArg (\arg opt -> return opt { consumerSecret = arg }) "SECRET")
         "a consumer secret for this app"

      -- HELP
   , Option "h" ["help"] 
         (NoArg $ \_ ->
            do
               prg <- getProgName
               hPutStrLn stderr $ usageInfo prg options
               exitWith ExitSuccess)
         "display help"

      -- VERSION
   , Option "v" ["version"] 
         (NoArg $ \_ ->
            do
               me <- getProgName
               hPutStrLn stderr $ me ++ " version " ++ version
               exitWith ExitSuccess)
         "display version"
   ]


generateAndSaveToken tokenFile key secret =
   do 
      token <- authenticate $ Consumer key secret
      putStrLn "writing token file..."
      writeToken token tokenFile


twoWeeksAgo =
   do
      now <- getCurrentTime

      let twoWeeks = 14 * (60 * 60 * 24) :: NominalDiffTime

      return $ addUTCTime (-twoWeeks) now


deleteOldOnes token fav =
   do
      let ctime = strptime "%a %b %d %T %z %Y" $ fcreated_at fav
  
      twoWeeksAgo' <- twoWeeksAgo

      ctime' <- case ctime of
                  Just (t,_) -> getCurrentTimeZone >>= \z -> return (localTimeToUTC z t)
                  _          -> error "that created time is not recognized"

      if ctime' <= twoWeeksAgo'
         then putStr "x" >> unFavorite (fid_str fav) token >> return ()
         else return ()



deleteOldFavorites page stopPage token =
   do
      putStr $ " <- getting page " ++ (shows page "") ++ "...  "

      favs  <- getFavorites [Page page] token

      sequence_ $ return $ mapM (deleteOldOnes token) favs

      putStrLn ""
      
      if favs == []
         then putStrLn "no more favorites received..."
         else return ()

      if page < stopPage && favs /= []
         then deleteOldFavorites (page + 1) stopPage token
         else return ()


main :: IO ()
main =
   do 
      args <- getArgs

      -- call getOpt, ignoring errors
      let (actions, nonOptions, _) = getOpt Permute options args

      -- process the defaults with those actions
      opts <- foldl (>>=) (return defaultOpts) actions

      if genMode opts
         then generateAndSaveToken (tokenFile opts) (consumerKey opts) (consumerSecret opts)
         else
            do
               token  <- readToken (tokenFile opts)
               totals <- getTotals token
               let max = favorites totals `div` 20 + 1
               putStrLn $ "number of favorites: " ++ (shows (favorites totals) "") ++ "..."
               deleteOldFavorites 1 max token


