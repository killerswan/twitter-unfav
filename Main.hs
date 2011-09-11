-- |
-- Module      : Main
-- Copyright   : (c) 2011 Kevin Cantu
--
-- License     : BSD-style
-- Maintainer  : Kevin Cantu <me@kevincantu.org>
-- Stability   : experimental
--
-- A program to clear out old Twitter favorites
-- 
module Main (main) where

import qualified Data.ByteString.Lazy as B
import System
import System.IO
import IO
import System.Console.GetOpt
import qualified Data.Text.Lazy as T
import qualified Data.Text.Lazy.Encoding as E
import Data.Word


-- command line options
data Options = Options { authMode         :: Bool
                       , cleanMode        :: Bool
                       , consumerKey      :: String
                       , consumerSecret   :: String
                       }


-- command line defaults
defaultOpts :: Options
defaultOpts = Options { authMode         = False
                      , cleanMode        = False
                      , consumerKey      = ""
                      , consumerSecret   = ""
                      }


-- command line description
-- this format is kinda bone headed:
--   [Option short [long] (property setter-function hint) description]
options :: [ OptDescr (Options -> IO Options) ]
options = [
            -- AUTH MODE
            Option "a" ["auth"] 
                   (NoArg $ \opt -> return opt { authMode = True })
                   "request and save an access token from Twitter (interactive)"

{-
          , Option "s" ["secret"] 
                   (ReqArg $ \arg opt -> return opt { consumerSecret = arg })
                   "a consumer secret for this app to use"

          , Option "k" ["key"] 
                   (ReqArg $ \arg opt -> return opt { consumerKey = arg })
                   "a consumer key for this app to use"
-}

            -- CLEAN MODE
          , Option "x" ["clean"] 
                   (NoArg $ \opt -> return opt { cleanMode = True })
                   "clean up: remove the saved the access token"

            -- HELP
          , Option "h" ["help"] 
                   (NoArg  $ \_ -> do
                        prg <- getProgName
                        hPutStrLn stderr $ usageInfo prg options
                        exitWith ExitSuccess)
                   "display this help and exit"

            -- VERSION
          , Option "v" ["version"] 
                   (NoArg $ \_ -> do
                        me <- getProgName
                        hPutStrLn stderr $ me ++ " version 0.5"
                        exitWith ExitSuccess)
                   "display version and exit"
          ]



main :: IO ()
main = 
   do 
      args <- getArgs

      -- call getOpt with the option description
      -- ignoring errors
      let (actions, nonOptions, _) = getOpt Permute options args

      -- process the defaults with those actions
      -- returns command line properties
      opts <- foldl (>>=) (return defaultOpts) actions

      -- assign the results
      -- via destructuring assignment
      let Options { authMode         = am
                  , cleanMode        = cm
                  , consumerKey      = ck
                  , consumerSecret   = cs
                  } = opts

      case (am, cm) of
                  (True, True)   -> error "auth mode and clean mode are mutually exclusive..."
                  (True, False)  -> putStrLn "auth mode, yay!"
                  (False, True)  -> putStrLn "clean mode, yay!"
                  _              -> putStrLn "neither mode, yay!"


      -- ...

{-

> :m + Web.Twitter
> :m + Web.Twitter.OAuth
> 
> let tk = authenticate $ Consumer "SNIP" "SNIP"
> 
> tk >>= (\t -> writeToken t "kevin.token") 
open https://api.twitter.com/oauth/authorize?oauth_token=SNIP
verifier: SNIP
> 
> 
> 
> let snd message = readToken "kevin.token" >>= (\t -> updateStatus t message)
> 
> snd "omfg"
> 
> rd
[Status {user        = "mathpunk",
         text        = "Strong truth. G+ and pseudonyms. RT @BoraZ: Pseudonym IS a name:  http://bit.ly/pW2yEQ",
         status_id   = 95257690909061121}]
> 
> getStatus "95257597556432896" []
Status {user = "doingitwrong", text = "RT @quinnnorton: These evil lesbian succubi are coming for your heterosexual marriage! http://bit.ly/o7ElE2", status_id = 95257597556432896}
> 
> 
> let rdf = readToken "kevin.token" >>= (getFavorites [Count 2])
> rdf
[Favorite {fcreated_at = "Mon Jul 25 08:51:41 +0000 2011", fid_str = "95415914727616512"},Favorite {fcreated_at = "Mon Jul 25 08:38:07 +0000 2011", fid_str = "95412499763036160"}]
> 
> 
> :m + Data.Time.Parse
> strptime "%a %b %d %T %z %Y" "Tue Jul 26 05:08:24 +0000 2011"
Just (2011-07-26 05:08:24,"")

-}


