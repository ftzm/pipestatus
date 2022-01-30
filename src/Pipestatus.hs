{-# LANGUAGE TupleSections #-}
{-# LANGUAGE TypeSynonymInstances #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Pipestatus (pipestatus) where

import           Control.Lens
import           Control.Monad
import           Data.List
import           Data.Maybe
import           Data.Void
import qualified Data.Map as M
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
import           Data.Aeson
import qualified Data.ByteString as BS
import qualified Data.ByteString.Lazy.UTF8 as BSL

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
      , "-a"
      , "pipestatus"
      , "-u"
      , show urgency
      , "-t"
      , show timeOut
      , "-p"
      , "" -- empty title
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

instance Status Int where
  parseStatus = readMaybe
  diff x y = Just $ stdMsg $ show y

-------------------------------------------------------------------------------
-- Sway

data Workspace = Focused String
               | Unfocused String
  deriving (Show, Eq)

data Sway = Sway
  { focused :: String
  , workSpaces :: [Workspace]
  , layout  :: String
  } deriving (Show, Eq)

emptySway = Sway "" [] ""

parseSway :: String -> Maybe Sway
parseSway s = do
  let (input :: BSL.ByteString) = BSL.fromString s
  m <- decode input :: Maybe (M.Map String Bool)
  let workspaces = map (\(k, v) -> if v then Focused k else Unfocused k ) $ M.toList m
  let focused = fromMaybe "None" $ fst <$> (find snd $ M.toList m)
  return $ Sway focused workspaces ""

instance Status Sway where
  parseStatus = parseSway
  diff x y = stdMsg
    <$> firstDiff [focused, unwords . (map show) . workSpaces, layout] x y

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

-------------------------------------------------------------------------------
-- Screen

type Brightness = Int

-------------------------------------------------------------------------------
-- Statuses

type DunstId = Int

data Statuses = Statuses
  { _sway :: (DunstId, Sway)
  , _volume :: (DunstId, Volume)
  , _battery :: (DunstId, Battery)
  , _brightness :: (DunstId, Brightness)
  , _reportage :: DunstId
  } deriving (Show)
makeLenses ''Statuses

emptyStatuses :: Statuses
emptyStatuses = Statuses
  { _sway = (0, emptySway)
  , _volume = (0, 0)
  , _battery = (0, emptyBattery)
  , _reportage = 0
  , _brightness = (0, 0)
  }

-------------------------------------------------------------------------------
-- Report

getTime :: IO String
getTime = showTime <$> getHourMinute
  where
    getHourMinute = do
      localTime <- utcToLocalTime <$> getCurrentTimeZone <*> getCurrentTime
      let timeOfDay = localTimeOfDay localTime
      return (todHour timeOfDay, todMin timeOfDay)
    showTime (hour, minute) = show hour <> "." <> printf "%02d" minute

layoutRenamer :: String -> String
layoutRenamer x = case x of
  "ResizableTall"          -> "left"
  "Mirror ResizableTall"   -> "top"
  "Full"                   -> "max"
  "BSP"                    -> "bsp"
  "Tabbed Bottom Simplest" -> "tabbed"
  x                        -> x

red = "#cc241d"
darkGray = "#665c54"
fg = "#ebdbb2"

colorSpan :: String -> String -> String
colorSpan s color ="<span foreground=\"" <> color <> "\">" <> s <> "</span>"

formatWorkSpaces :: [Workspace] -> String
formatWorkSpaces = unwords . map formatter
  where formatter :: Workspace -> String
        formatter (Focused s) = colorSpan s fg
        formatter (Unfocused s) = colorSpan s darkGray

report :: Statuses -> DunstId -> IO DunstId
report r i = do
  time <- getTime
  let x = r ^. sway . _2
      x_u = workSpaces x
      x_f = focused x
      --x_l = layoutRenamer $ layout x
      x_string = formatWorkSpaces x_u
      b_s = r ^. battery . _2 & displayBattery
      v_s = "<b>vol:</b> " ++ (r ^. volume . _2 & show)
  --dunstify $ stdMsg (intercalate "  •  " [x_string, v_s, b_s, time]) i
  dunstify $ stdMsg (intercalate "\n" [x_string, v_s, b_s, time]) i

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
handle s i x =
  case parseStatus s of
    Nothing -> hPutStrLn stderr ("Parse error: " ++ s) >> return (i, x)
    Just parsed -> case diff x parsed of
      Nothing -> return (i, x)
      Just diff -> do
        id <- dunstify $ (diff i)
        return (id, parsed)

parseLine :: Statuses -> String -> IO Statuses
parseLine r ('?':_) = r & reportage %%~ report r
parseLine r (label:';':s) =
  let go x = r & x %%~ uncurry (handle s)
  in case label of
    'S' -> go sway
    'v' -> go volume
    'b' -> go battery
    'B' -> go brightness
    _   -> parseLine r s
parseLine r s = dunstify (stdMsg ("Parse error: " ++ s) 1) >> return r

pipestatus :: IO ()
pipestatus = do
  removeIfExists fifo
  makePipe fifo
  h <- openFile fifo ReadWriteMode
  statusFifo <- hGetContents h
  foldM_ parseLine emptyStatuses $ lines statusFifo
