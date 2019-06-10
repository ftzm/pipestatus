{-# LANGUAGE TupleSections #-}
{-# LANGUAGE TypeSynonymInstances #-}
{-# LANGUAGE TemplateHaskell #-}


module Pipestatus (pipestatus) where

import Text.Read
import Data.List
import Data.List.Split
import Data.Maybe
import qualified Data.Set as S
import qualified Data.Map.Strict as M
import Data.Char
import Control.Monad
import Control.Lens
import System.Process
import System.IO
import System.IO.Error
import System.Posix.Files

-------------------------------------------------------------------------------
-- General

firstDiff :: [a -> String] -> a -> a -> Maybe String
firstDiff fs r1 r2 = msum $ map match fs
  where match f | f r1 /= f r2 = Just $ f r2
                | otherwise = Nothing

class (Eq a, Show a) => Status a where
  parse :: String -> Maybe a
  diff :: a -> a -> Maybe String

-------------------------------------------------------------------------------
-- XMonad

type Workspaces = S.Set String

showWs =  unwords . S.toList

data XMonad = XMonad
  { focused :: String
  , workSpaces :: Workspaces
  , layout  :: String
  } deriving (Show, Eq)
emptyXMonad = XMonad "" S.empty ""

parseXMonad :: String -> Maybe XMonad
parseXMonad s = do
    (rawWs, mode, occ) <- getPieces s
    let (foc, ws) = parseWorkspaces rawWs occ
    return $ XMonad foc ws mode

parseWorkspaces :: String -> Bool -> (String, S.Set String)
parseWorkspaces s occ = (cur, if occ then S.insert cur wSet else wSet )
  where
    (curList, occList) = partition (elem '[') $ words s
    cur = tail $ init $ head curList
    wSet = S.fromList occList

getPieces :: String -> Maybe (String, String, Bool)
getPieces s = let parts = splitOn " : " s in
  case length parts of
    0 -> Nothing
    1 -> Nothing
    2 -> Just (head parts, parts !! 1, False)
    _ -> Just (head parts, parts !! 1, True)

instance Status XMonad where
  parse = parseXMonad
  diff = firstDiff [focused, showWs . workSpaces, layout]

-------------------------------------------------------------------------------
-- Battery

data Battery = Battery
  { percent :: Int
  , state :: String
  } deriving (Show, Eq)

emptyBattery = Battery 100 "Full"

parseACPI :: String -> Maybe Battery
parseACPI s = do
  state <- M.lookup "state" m
  percent <- readMaybe . filter isDigit =<< M.lookup "percentage" m :: Maybe Int
  Just $ Battery percent state
  where
    ls = splitOn ";" $ filter (not . isSpace) s
    fields = map ((\(x, y) -> (x, drop 1 y)) . span (':' /=)) ls
    m = M.fromList fields :: M.Map String String

instance Status Battery where
  parse = parseACPI
  diff x y | x == y = Nothing
           | x /= y = Just (show ( percent y) ++ " - " ++ state y)

-------------------------------------------------------------------------------
-- Volume

type Volume = Int

instance Status Volume where
  parse = readMaybe
  diff x y = Just $ show y

-------------------------------------------------------------------------------
-- Statuses

type DunstId = Int

data Statuses = Statuses
  { _xmonad :: (DunstId, XMonad)
  , _volume :: (DunstId, Volume)
  , _battery :: (DunstId, Battery)
  } deriving (Show)
makeLenses ''Statuses

emptyStatuses :: Statuses
emptyStatuses = Statuses
  { _xmonad = (0, emptyXMonad)
  , _volume = (0, 0)
  , _battery = (0, emptyBattery)
  }

-------------------------------------------------------------------------------
-- Runner

dunstify :: Int -> String -> IO Int
dunstify i msg = fromMaybe 0. readMaybe
  <$> catchIOError cmd (\_ -> return "0")
  where cmd = readProcess "dunstify" ["-r", show i, "-p", msg] []

handle :: Status a => String -> Int -> a -> IO (Int, a)
handle s i x = fromMaybe (return (i, x)) $ do
      parsed <- parse s
      diff <- diff x parsed
      return $ (,parsed) <$> dunstify i diff

parseLine :: Statuses -> String -> IO Statuses
parseLine r (label:';':s) =
  let go x = r & x %%~ uncurry (handle s)
  in case label of
    'x' -> go xmonad
    'v' -> go volume
    'b' -> go battery
    's' -> go battery
    _   -> parseLine r s
parseLine r s = dunstify 1 s >> return r

makePipe = catchIOError makePipe' handler
  where
    handler _ = putStrLn "File already exists at /tmp/statuspipe.fifo"
    makePipe' = createNamedPipe "/tmp/statuspipe.fifo" accessModes

pipestatus :: IO ()
pipestatus = do
  makePipe
  h <- openFile "/tmp/statuspipe.fifo" ReadWriteMode
  statusFifo <- hGetContents h
  foldM_ parseLine emptyStatuses $ lines statusFifo
