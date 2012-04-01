-- |
-- Module      : Main
-- Copyright   : (c) 2011 Kevin Cantu
--
-- License     : BSD3
-- Maintainer  : Kevin Cantu <me@kevincantu.org>
-- Stability   : experimental
--
-- A program to clear out old Twitter favorites


module Main (main) where

import Control.Monad (when)
import Data.Time.Clock (addUTCTime, getCurrentTime, NominalDiffTime(), UTCTime())
import Data.Time.LocalTime (getCurrentTimeZone, localTimeToUTC)
import Data.Time.Parse (strptime)
import Network.OAuth.Consumer (Token())
import System.Environment (getArgs, getProgName)
import System.Exit (exitWith, ExitCode(ExitSuccess))
import System.IO (stderr, hPutStrLn)
import System.Console.GetOpt (getOpt, usageInfo, OptDescr(Option), ArgDescr(ReqArg, NoArg), ArgOrder(Permute))

-- provided by askitter
import Web.Twitter (Favorite(), favorites, getFavorites, fcreated_at, getTotals, fid_str, unFavorite, Option(Page))
import Web.Twitter.OAuth (authenticate, Consumer(Consumer), readToken, writeToken)


version :: String
version = "0.2"


-- command line options
data Options = Options { genMode          :: Bool
                       , tokenFileName    :: String
                       , consumerKey      :: String
                       , consumerSecret   :: String
                       }


-- command line defaults
defaultOpts :: Options
defaultOpts = Options { genMode          = False
                      , tokenFileName    = error "no file specified..."
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
     Option "t" ["tokenFile"] 
         (ReqArg (\arg opt -> return opt { tokenFileName = arg }) "FILE")
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

 
generateAndSaveToken :: FilePath -> String -> String -> IO ()
generateAndSaveToken filename key secret =
   do 
      token <- authenticate $ Consumer key secret
      putStrLn "writing token file..."
      writeToken token filename


oneWeekAgo :: IO UTCTime
oneWeekAgo =
   do
      now <- getCurrentTime

      let oneWeek = 7 * (60 * 60 * 24) :: NominalDiffTime

      return $ addUTCTime (-oneWeek) now


deleteOldOnes :: Token -> Favorite -> IO ()
deleteOldOnes token fav =
   do
      let ctime = strptime "%a %b %d %T %z %Y" $ fcreated_at fav
  
      oneWeekAgo' <- oneWeekAgo

      ctime' <- case ctime of
                  Just (t,_) -> getCurrentTimeZone >>= \z -> return (localTimeToUTC z t)
                  _          -> error "that created time is not recognized"

      when (ctime' <= oneWeekAgo') $
         putStr "x" >> unFavorite (fid_str fav) token >> return ()


deleteOldFavorites :: Integer -> Integer -> Token -> IO ()
deleteOldFavorites page stopPage token =
   do
      putStr $ " <- getting page " ++ shows page "" ++ "...  "

      favs  <- getFavorites [Page page] token

      sequence_ $ return $ mapM (deleteOldOnes token) favs

      putStrLn ""
      
      when (favs == []) $
         putStrLn "no more favorites received..."

      when (page < stopPage && favs /= []) $
         deleteOldFavorites (page + 1) stopPage token


main :: IO ()
main =
   do 
      args <- getArgs

      -- call getOpt, ignoring errors
      let (actions, _, _) = getOpt Permute options args

      -- process the defaults with those actions
      opts <- foldl (>>=) (return defaultOpts) actions

      if genMode opts
         then generateAndSaveToken (tokenFileName opts) (consumerKey opts) (consumerSecret opts)
         else
            do
               token  <- readToken (tokenFileName opts)
               totals <- getTotals token
               let max' = favorites totals `div` 20 + 1
               putStrLn $ "number of favorites: " ++ shows (favorites totals) "" ++ "..."
               deleteOldFavorites 1 max' token


