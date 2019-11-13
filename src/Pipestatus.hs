{-# LANGUAGE TupleSections #-}
{-# LANGUAGE TypeSynonymInstances #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE RecordWildCards #-}

module Pipestatus (pipestatus) where

import           Control.Lens
import           Control.Monad
import           Data.List
import           Data.Maybe
import           Data.Void
import           System.IO
import           System.IO.Error
import           System.Posix.Files
import           System.Process
import           Text.Read hiding (choice)
import           Text.Printf

import           Prelude hiding (catch)
import           System.Directory
import           Control.Exception hiding (handle)
import           System.IO.Error hiding (catch)

import           Data.Time
import           Text.Megaparsec
import           Text.Megaparsec.Char
import           Text.Megaparsec.Char.Lexer (decimal)

-------------------------------------------------------------------------------
-- Dunst

data Urgency = Low | Normal | Critical deriving (Show)

data Dunstify = Dunstify
  { urgency :: Urgency
  , timeOut :: Int
  , message :: String
  , replaceId :: Int
  }

stdMsg :: String -> Int -> Dunstify
stdMsg = Dunstify Normal 2000

dunstify :: Dunstify -> IO DunstId
dunstify Dunstify {..} =
  fromMaybe 0 . readMaybe <$> catchIOError cmd (\e -> hPutStrLn stderr (show e) >> return "0")
  where
    cmd = readProcess "dunstify"
      ["-r"
      , show replaceId
      , "-u"
      , show urgency
      , "-t"
      , show timeOut
      , "-p"
      , message
      ]
      []

-------------------------------------------------------------------------------
-- General

type Parser = Parsec Void String

firstDiff :: [a -> String] -> a -> a -> Maybe String
firstDiff fs r1 r2 = msum $ map match fs
  where match f | f r1 /= f r2 = Just $ f r2
                | otherwise = Nothing

class (Eq a, Show a) => Status a where
  parseStatus:: String -> Maybe a
  diff :: a -> a -> Maybe (Int -> Dunstify)

-------------------------------------------------------------------------------
-- XMonad

data Workspace = Focused String | Unfocused String
type Workspaces = [String]

data XMonad = XMonad
  { focused :: String
  , workSpaces :: Workspaces
  , layout  :: String
  } deriving (Show, Eq)

emptyXMonad = XMonad "" [] ""

parseXMonad :: String -> Maybe XMonad
parseXMonad = parseMaybe $ do
  (u, f) <- parseWorkspaces
  chunk ": "
  l <- some alphaNumChar
  takeRest
  return $ XMonad f u l

parseWorkspaces :: Parser (Workspaces, String)
parseWorkspaces = sw <$> sepEndBy1 (f <|> u) (char ' ')
  where
    f = Focused <$> between (char '[') (char ']') (some alphaNumChar)
    u = Unfocused <$> some alphaNumChar
    sw xs = ([s | Unfocused s <- xs] , head [s | Focused s <- xs])

instance Status XMonad where
  parseStatus = parseXMonad
  diff x y = stdMsg <$> firstDiff [focused, unwords . workSpaces, layout] x y

-------------------------------------------------------------------------------
-- Battery

data BatStatus = Full | Charging | Discharging deriving (Show, Eq)

data Battery = Battery
  { percent :: Int
  , state :: BatStatus
  , remaining :: String -- (hours, minutes)
  , alert :: Bool
  } deriving (Show, Eq)

emptyBattery = Battery 100 Full "0.0" False

parseACPI :: Parser Battery
parseACPI = do
  state <- choice
    [ (chunk "Full" <|> chunk "Unknown") >> pure Full
    , chunk "Charging" >> pure Charging
    , chunk "Discharging" >> pure Discharging
    ]
  space
  percent <- decimal
  space
  energyNow <- fromIntegral <$> decimal
  space
  powerNow <- fromIntegral <$> decimal
  let remaining = getRemaining energyNow powerNow
  let alert = state == Discharging && percent < 10
  pure $ Battery {..}
  where
    getRemaining e p =
      let (h, m) = quotRem (round $ fromIntegral e / fromIntegral p * 60) 60
      in show h ++ "." ++ show m

displayBattery :: Battery -> String
displayBattery b = case state b of
  Full -> "Charged"
  Charging -> "Charging: " ++ p
  Discharging -> "Discharging: " ++ p ++ " / " ++ remaining b ++ " remaining"
  where
    p = (show $ percent b) ++ "%"

instance Status Battery where
  parseStatus = parseMaybe parseACPI
  diff x y | x == y = Nothing
           | x /= y = Just $ Dunstify
                      (if alert y then Critical else Normal)
                      10000
                      (displayBattery y)

-------------------------------------------------------------------------------
-- Volume

type Volume = Int

instance Status Volume where
  parseStatus = readMaybe
  diff x y = Just $ stdMsg $ show y

-------------------------------------------------------------------------------
-- Statuses

type DunstId = Int

data Statuses = Statuses
  { _xmonad :: (DunstId, XMonad)
  , _volume :: (DunstId, Volume)
  , _battery :: (DunstId, Battery)
  , _reportage :: DunstId
  } deriving (Show)
makeLenses ''Statuses

emptyStatuses :: Statuses
emptyStatuses = Statuses
  { _xmonad = (0, emptyXMonad)
  , _volume = (0, 0)
  , _battery = (0, emptyBattery)
  , _reportage = 0
  }

-------------------------------------------------------------------------------
-- Report

getTime :: IO String
getTime = do
  utc <- utcToLocalTime <$> getCurrentTimeZone <*> getCurrentTime
  let local = localTimeOfDay utc
  return $ (show $ todHour local) ++ "." ++ (printf "%02d" $ todMin local)

report :: Statuses -> DunstId -> IO DunstId
report r i = do
  time <- getTime
  let x = r ^. xmonad . _2
  let x_u = workSpaces x
  let x_f = focused x
  let x_l = layout x
  let x_string = x_f ++ "-" ++ x_l ++ " ( " ++ (intercalate " " x_u) ++ " )"
  let b_s = r ^. battery . _2 & displayBattery
  let v_s = "vol: " ++ (r ^. volume . _2 & show)
  dunstify $ stdMsg (intercalate "  •  " [x_string, v_s, b_s, time]) i

-------------------------------------------------------------------------------
-- Pipe

removeIfExists :: FilePath -> IO ()
removeIfExists fileName = removeFile fileName `catch` handleExists
  where handleExists e
          | isDoesNotExistError e = return ()
          | otherwise = throwIO e

makePipe :: FilePath -> IO ()
makePipe fileName = do
  catchIOError makePipe' handler
  where
    handler _ = putStrLn $ "File already exists at" ++ fileName
    makePipe' = createNamedPipe fileName accessModes

fifo :: FilePath
fifo = "/tmp/statuspipe.fifo"

-------------------------------------------------------------------------------
-- Runner

handle :: Status a => String -> Int -> a -> IO (Int, a)
handle s i x = fromMaybe (return (i, x)) $ do
      parsed <- parseStatus s
      diff <- diff x parsed
      return $ (,parsed) <$> (dunstify $ (diff i))

parseLine :: Statuses -> String -> IO Statuses
parseLine r ('?':_) = r & reportage %%~ report r
parseLine r (label:';':s) =
  let go x = r & x %%~ uncurry (handle s)
  in print s >> case label of
    'x' -> go xmonad
    'v' -> go volume
    'b' -> go battery
    's' -> go battery
    _   -> parseLine r s
parseLine r s = dunstify (stdMsg s 1) >> return r

pipestatus :: IO ()
pipestatus = do
  removeIfExists fifo
  makePipe fifo
  h <- openFile fifo ReadWriteMode
  statusFifo <- hGetContents h
  foldM_ parseLine emptyStatuses $ lines statusFifo
