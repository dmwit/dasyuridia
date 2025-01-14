module Main where

import Control.Concurrent
import Control.Concurrent.STM
import Control.Monad
import Data.Word
import Paths_dasyuridia
import System.Environment
import System.Exit
import System.IO
import System.Process
import Text.Printf
import Text.Read

import qualified Data.Array as A
import qualified Data.Array.IO as MA

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
	rpc <- launchRPCThreads q s
	let rpcSync = join . rpc
	stateThread rpc

launchRPCThreads :: (Identified Request -> IO ()) -> IO (Identified Response) -> IO (Request -> IO (IO Response))
launchRPCThreads q s = do
	chan <- newTChanIO
	reservations <- MA.newGenArray @MA.IOArray (minBound, maxBound) (const newEmptyMVar) >>= MA.freeze
	let reqLoop i = do
	    	(req, mresp) <- atomically (readTChan chan)
	    	success <- tryPutMVar (reservations A.! i) (putMVar mresp)
	    	if success
	    		then q (Identified i req)
	    		else do
	    			hPrintf stderr "Tried to reuse outstanding ID %d; perhaps the game has stopped responding? Will try again with another ID after waiting one frame.\n" i
	    			atomically (unGetTChan chan (req, mresp))
	    			threadDelay (1000000`quot`60)
	    	reqLoop (i+1)
	    respLoop = do
	    	Identified i resp <- s
	    	tryTakeMVar (reservations A.! i) >>= \case
	    		Just act -> act resp
	    		Nothing -> hPrintf stderr "Ignoring response with unexpected ID %d\n" i
	    	respLoop
	_ <- forkIO (reqLoop 0)
	_ <- forkIO respLoop
	return \req -> do
		mresp <- newEmptyMVar
		atomically (writeTChan chan (req, mresp))
		return (takeMVar mresp)

clockThread :: (Request -> IO Response) -> IO a
-- 2 million years ought to be enough for anybody
clockThread q = go (0 :: Word64) where
	go clock = print clock >> q req >>= \case
		SRead clock' -> go (clock + fromIntegral (clock' - fromIntegral clock))
		resp -> do
			hPrintf stderr "Ignoring unexpected response %s to request %s\n" (show resp) (show req)
			go clock
	req = QRead addrClock

data State = Unknown | Err | Don'tCare | Pregame | Play deriving (Eq, Ord, Read, Show)

stateThread :: (Request -> IO (IO Response)) -> IO a
stateThread q = getState >>= new where
	new st = print st >> go st
	go st = do
		st' <- getState
		if st == st' then go st else new st'
	
	getState = do
		mresps <- traverse (q . QRead) [addrState, addrNumPlayers, addrP1VirusesToAdd, addrP2VirusesToAdd]
		[respState, respNumPlayers, respP1VirusesToAdd, respP2VirusesToAdd] <- sequence mresps
		pure case respState of
			SRead 8 -> case respP1VirusesToAdd of
				SRead 0 -> case (respNumPlayers, respP2VirusesToAdd) of
					(SRead 1, SRead _) -> Pregame
					(SRead 2, SRead 0) -> Pregame
					(SRead _, SRead _) -> Don'tCare
					_ -> Unknown
				SRead _ -> Don'tCare
				_ -> Unknown
			SRead 4 -> Play
			SRead _ -> Don'tCare
			_ -> Unknown

expectedVersion :: Response
expectedVersion = SVersion
	{ protocolVersion = 0
	, maxArrayReadSize = 16
	, maxArrayWriteSize = 16
	, maxFreezeCount = 5
	}

addrClock :: Word16
addrClock = 0x43

addrState :: Word16
addrState = 0x46

addrNumPlayers :: Word16
addrNumPlayers = 0x727

addrP1State :: Word16
addrP1State = 0x300

addrP2State :: Word16
addrP2State = 0x380

offsetVirusesToAdd :: Word16
offsetVirusesToAdd = 0x28

addrP1VirusesToAdd :: Word16
addrP1VirusesToAdd = addrP1State + offsetVirusesToAdd

addrP2VirusesToAdd :: Word16
addrP2VirusesToAdd = addrP2State + offsetVirusesToAdd
