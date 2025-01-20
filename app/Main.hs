module Main where

import Control.Concurrent
import Control.Monad
import Data.Bits
import Data.Word
import Dr.Mario.Model
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
	QRead addr -> printf "read %d" addr
	QArrayRead size addr -> printf "array_read %d %d" size addr

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
		("array_read", ' ':(traverse readMaybe . words -> Just vals)) -> success n $ SArrayRead vals
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
	| QArrayRead Word8 Word16
	deriving (Eq, Ord, Read, Show)
data Response
	= SVersion { protocolVersion, maxArrayReadSize, maxArrayWriteSize, maxFreezeCount :: Word8 }
	| SRead Word8
	| SArrayRead [Word8]
	deriving (Eq, Ord, Read, Show)

-- 2 million years ought to be enough for anybody
type FrameCount = Word64
data State
	= Bootstrapping
	| Unknown FrameCount
	| WaitingForVirusesOrFirstControl FrameCount
	| ReadingLevel (CoarseSpeed -> [Cell] -> FrameCount -> State) Word16 [Cell] FrameCount
	| HaveLevelBeforeUnknownState CoarseSpeed [Cell] FrameCount
	| WaitingForViruses FrameCount
	| WaitingForFirstControl CoarseSpeed [Cell] FrameCount
	| InLevel FrameCount
	| NotWon FrameCount
	| Cleanup FrameCount
	| Throwing FrameCount
	| ThrowingOrControl FrameCount
	| Relax FrameCount
	| Sync (FrameCount -> State) FrameCount

topLoop :: (Identified Request -> IO ()) -> IO (Identified Response) -> IO ()
topLoop q_ s_ = go Bootstrapping where
	q req = do
		q_ (Identified 0 req)
		Identified 0 resp <- s_
		pure resp

	go = \case
		-- miscellaneous
		Bootstrapping -> do
			v <- q QVersion
			unless (v == expectedVersion) $ hPrintf stderr "WARNING: continuing despite unexpected version information %s\n" (show v)
			clk <- qRead addrClock
			go (Unknown (fromIntegral clk))
		Unknown clk -> readUIState WaitingForVirusesOrFirstControl clk
		Relax clk -> printf "relax %d\n" clk >> go (Sync Unknown clk)
		Sync f clk -> do
			clk' <- qRead addrClock
			let diff = clk' - fromIntegral clk
			when (diff /= 1) (printf "Clock mismatch: %d vs. %d\n" (clk+1) clk')
			go . f $ clk + fromIntegral diff

		-- some event just fired that makes now a good time to read the board state
		ReadingLevel k len cs clk | len < boardLength -> do
			SArrayRead ws <- q (QArrayRead (maxArrayReadSize expectedVersion) (addrP1Board + len))
			cs' <- traverse decodeCell ws
			-- assumes maxArrayReadSize divides boardLength
			tick clk $ ReadingLevel k (len+fromIntegral (maxArrayReadSize expectedVersion)) (cs ++ cs')
		ReadingLevel k _len cs clk -> do
			w <- qRead addrP1CoarseSpeed
			spd <- decodeSpeed w
			print spd
			ppIO $ unsafeGenerateBoard boardWidth boardHeight \(Position x y) -> cs !! (boardWidth*(boardHeight-y-1) + x)
			tick clk $ k spd cs

		-- level has just started, we haven't started controlling the first pill yet
		WaitingForVirusesOrFirstControl clk -> do
			n <- qRead addrP1VirusesToAdd
			tick clk case n of
				0 -> ReadingLevel HaveLevelBeforeUnknownState 0 []
				_ -> WaitingForViruses
		HaveLevelBeforeUnknownState spd cs clk -> readUIState (WaitingForFirstControl spd cs) clk
		WaitingForViruses clk -> qRead addrP1VirusesToAdd >>= tick clk . \case
			0 -> ReadingLevel WaitingForFirstControl 0 []
			_ -> WaitingForViruses
		WaitingForFirstControl spd cs clk -> readUIState (WaitingForFirstControl spd cs) clk

		-- we are playing a level; the bulk of the time should be spent in these states
		-- TODO: if we just started the emulator and are dropped into an InLevel state, we should report the board and pill status and whatever we know about control
		InLevel clk -> do
			n <- qRead addrP1VirusCount
			tick clk case n of
				0 -> Relax
				_ -> NotWon
		-- game states:
		-- 0 control
		-- 1 cleanup
		-- 2 attack
		-- 3 delay
		-- 4 impossible
		-- 5 prethrow
		-- 6 throw
		NotWon clk -> do
			st <- qRead addrP1GameState
			case st of
				0 -> tick clk NotWon
				1 -> putStrLn ("lock " ++ show clk) >> tick clk Cleanup -- TODO: make a second report with the pill position/orientation before transitioning to cleanup
				-- TODO: handle Low speed correctly
				2 -> putStrLn ("next control " ++ show (clk+25)) >> tick clk (ReadingLevel (\_ _ -> Throwing) 0 [])
				3 -> putStrLn ("next control " ++ show (clk+1)) >> tick clk NotWon
				4 -> tick clk Unknown
				5 -> tick clk (ReadingLevel (\_ _ -> ThrowingOrControl) 0 [])
				6 -> tick clk Throwing -- TODO: report next control
				_ -> fail $ "unknown game state " ++ show st
		Cleanup clk -> do
			st <- qRead addrP1GameState
			case st of
				0 -> tick clk Relax -- console reset
				1 -> tick clk Cleanup
				-- TODO: handle Low speed correctly
				2 -> putStrLn ("next control " ++ show (clk+25)) >> tick clk (ReadingLevel (\_ _ -> Throwing) 0 [])
				_ -> fail $ "unexpected transition from cleanup game state to " ++ show st
		Throwing clk -> do
			st <- qRead addrP1GameState
			case st of
				000 -> tick clk Relax -- console reset, not Rev A
				003 -> putStrLn ("next control " ++ show (clk+1))
				    >> tick clk Unknown -- finished throwing; could do something fancy here to get the right next state, but why bother?
				006 -> tick clk Throwing
				255 -> tick clk Relax -- console reset, Rev A
				_   -> hPutStrLn stderr ("WARNING: unexpected transition from throwing to " ++ show st)
				    >> tick clk Unknown -- probably a console reset?
		ThrowingOrControl clk -> go (Unknown clk) -- TODO: check whether we're still throwing; if we are, report when the next control phase is, and if not, report the current pill situation and that control is happening now

	tick clk f = go (f (clk+1))
	readUIState f clk = do
		st <- qRead addrUIState
		tick clk case st of
			8 -> f
			4 -> InLevel
			_ -> Relax
	qRead addr = q (QRead addr) >>= \case
		SRead resp -> pure resp
		resp -> fail $ "Unexpected response " ++ show resp ++ " to request " ++ show (QRead addr)

decodeCell :: Word8 -> IO Cell
decodeCell 0xff = pure Empty
decodeCell w = do
	c <- case w .&. 0x0f of
		0 -> pure Yellow
		1 -> pure Red
		2 -> pure Blue
		_ -> fail $ "failed to decode cell byte " ++ show w
	case w `shiftR` 4 of
		0x4 -> pure $ Occupied c North
		0x5 -> pure $ Occupied c South
		0x6 -> pure $ Occupied c West
		0x7 -> pure $ Occupied c East
		0x8 -> pure $ Occupied c Disconnected
		0xb -> pure $ Empty -- clearing (don't know how it chooses between this and the 0xf case)
		0xd -> pure $ Occupied c Virus
		0xf -> pure $ Empty -- clearing
		_ -> fail $ "failed to decode cell byte " ++ show w

decodeSpeed :: Word8 -> IO CoarseSpeed
decodeSpeed = \case
	0 -> pure Low
	1 -> pure Med
	2 -> pure Hi
	w -> fail $ "failed to decode speed byte " ++ show w

expectedVersion :: Response
expectedVersion = SVersion
	{ protocolVersion = 0
	, maxArrayReadSize = 16
	, maxArrayWriteSize = 16
	, maxFreezeCount = 5
	}

boardWidth, boardHeight :: Int
boardWidth = 8
boardHeight = 16

boardLength :: Word16
boardLength = fromIntegral (boardWidth * boardHeight)

addrClock :: Word16
addrClock = 0x43

addrUIState :: Word16
addrUIState = 0x46

addrNumPlayers :: Word16
addrNumPlayers = 0x727

addrP1State, addrP2State :: Word16
addrP1State = 0x300
addrP2State = 0x380

offsetVirusesToAdd, addrP1VirusesToAdd, addrP2VirusesToAdd :: Word16
offsetVirusesToAdd = 0x28

offsetCoarseSpeed, addrP1CoarseSpeed, addrP2CoarseSpeed :: Word16
offsetCoarseSpeed = 0xb

offsetGameState, addrP1GameState, addrP2GameState :: Word16
offsetGameState = 0x17

offsetVirusCount, addrP1VirusCount, addrP2VirusCount :: Word16
offsetVirusCount = 0x24

offsetsPlayerState :: [Word16]
offsetsPlayerState = [
 	offsetVirusesToAdd, offsetCoarseSpeed, offsetGameState, offsetVirusCount]
[	addrP1VirusesToAdd, addrP1CoarseSpeed, addrP1GameState, addrP1VirusCount] = map (addrP1State+) offsetsPlayerState
[	addrP2VirusesToAdd, addrP2CoarseSpeed, addrP2GameState, addrP2VirusCount] = map (addrP2State+) offsetsPlayerState

addrP1Board, addrP2Board :: Word16
addrP1Board = 0x400
addrP2Board = 0x500
