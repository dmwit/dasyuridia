module Main where

import Control.Concurrent
import Control.Monad
import Data.Bits
import Data.Word
import Paths_dasyuridia
import System.Environment
import System.Exit
import System.IO
import System.Process
import Text.Printf
import Text.Read

main :: IO ()
main = do
	args <- getArgs
	case args of
		"--fceux":fceuxArgs -> fceux fceuxArgs
		["--help"] -> usage stdout ExitSuccess
		["-h"] -> usage stdout ExitSuccess
		_ -> usage stderr (ExitFailure 1)

usage :: Handle -> ExitCode -> IO a
usage h exitCode = do
	nm <- getProgName
	hPutStr h . unlines $ drop 1 [undefined
		, "USAGE: " ++ nm ++ " (--fceux [ARGS]) | --help | -h"
		, ""
		, "This is a tool for interacting with an implementation of NES Dr. Mario that"
		, "supports the Crowd Control communication protocol. The CC protocol is game"
		, "agnostic; this tool bakes in knowledge about Dr. Mario to ease interaction."
		, "It prints information about the current game state on stdout, and accepts"
		, "commands on stdin. The exact format is not yet documented. Whoops."
		, ""
		, "    --fceux       Launch and communicate with fceux, passing any extra ARGS"
		, "                  along verbatim"
		, "    --help, -h    Print this message"
		]
	exitWith exitCode

fceux :: [String] -> IO ()
fceux args = do
	script <- getDataFileName "dasyuridia.lua"
	(i, o, e, _p) <- runInteractiveProcess "fceux" ("--loadlua":script:args) Nothing Nothing
	hSetBuffering i LineBuffering
	hSetBuffering o LineBuffering
	hSetBuffering e LineBuffering
	_ <- forkIO . forever $ do
		line <- hGetLine e
		hPrintf stderr "fceux err: %s\n" line
	topLoop (renderForFceux i) (parseFromFceux o)

renderForFceux :: Handle -> Identified Request -> IO ()
renderForFceux h ireq = hPrintf h "%d %s\n" (identity ireq) case value ireq of
	QVersion -> "version"
	QRead addr -> "read " ++ show addr

parseFromFceux :: Handle -> IO (Identified Response)
parseFromFceux h = go where
	go = hGetLine h >>= \case
		'd':'a':'s':'y':'u':'r':'i':'d':'i':'a':':':' ':line -> pIdentified line
		line -> hPrintf stderr "fceux out: %s\n" line >> go

	pIdentified s = case break (' '==) s of
		(readMaybe -> Just n, ' ':rest) -> pResponse n rest
		_ -> complain $ "Ignoring malformed response from dasyuridia.lua: " ++ s
	pResponse n s = case break (' '==) s of
		("version", ' ':rest) -> pVersion n rest
		("read", ' ':(readMaybe -> Just val)) -> success n $ SRead val
		(responseTy, rest) -> complain $ printf "Ignoring response with unknown type %s from dasyuridia.lua, or with unknown format for this type: %s" responseTy rest
	pVersion n s = case words s of
		[readMaybe -> Just v, readMaybe -> Just ar, readMaybe -> Just aw, readMaybe -> Just fc]
			-> success n $ SVersion v ar aw fc
		_ -> complain $ "Ignoring version response with malformed arguments: " ++ s

	success n = pure . Identified n
	complain s = hPutStrLn stderr s >> go

data Identified a = Identified
	{ identity :: Word8
	, value :: a
	} deriving (Eq, Ord, Read, Show)
data Request
	= QVersion
	| QRead Word16
	deriving (Eq, Ord, Read, Show)
data Response
	= SVersion { protocolVersion, maxArrayReadSize, maxArrayWriteSize, maxFreezeCount :: Word8 }
	| SRead Word8
	deriving (Eq, Ord, Read, Show)

topLoop :: (Identified Request -> IO ()) -> IO (Identified Response) -> IO ()
topLoop q s = do
	q (Identified 42 QVersion)
	v <- s
	unless (v == Identified 42 expectedVersion) $ hPrintf stderr "WARNING: continuing despite unexpected version information %s\n" (show v)
	-- 2 million years ought to be enough for anybody
	go 0 (0 :: Word64)
	where
	go i clock = do
		print clock
		q (Identified i (QRead addrClock))
		response <- s
		case response of
			Identified ((i==) -> True) (SRead clock') -> go (i+1) $ clock + clock8' - clock8 + if clock8' < clock8 then 0x100 else 0
				where
				clock8 = clock .&. 0xff
				clock8' = fromIntegral clock'
			_ -> hPrintf stderr "FATAL: surprising response %s to query %s\n"
				(show response)
				(show (Identified i (QRead addrClock)))

expectedVersion :: Response
expectedVersion = SVersion
	{ protocolVersion = 0
	, maxArrayReadSize = 16
	, maxArrayWriteSize = 16
	, maxFreezeCount = 5
	}

addrClock :: Word16
addrClock = 0x43
