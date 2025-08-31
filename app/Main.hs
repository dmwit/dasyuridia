module Main where

import Control.Applicative
import Control.Concurrent
import Control.Concurrent.STM
import Control.Monad
import Data.Bits
import Data.Char (toLower)
import Data.IORef
import Data.List
import Data.Word
import Dr.Mario.Model hiding (decodeColor)
import Paths_dasyuridia
import System.Environment
import System.Exit
import System.Hardware.N8Pro
import System.IO
import System.Process
import Text.Printf
import Text.Read

import qualified Data.ByteString as BS
import qualified Data.ByteString.Builder as BS

main :: IO ()
main = do
	args <- getArgs
	env <- newEnvironment
	_ <- forkIO (aiLoop env)
	case args of
		"--fceux":fceuxArgs -> fceux fceuxArgs env
		["--everdrive", dev] -> everdrive dev env
		["--help"] -> usage stdout ExitSuccess
		["-h"] -> usage stdout ExitSuccess
		_ -> usage stderr (ExitFailure 1)

usage :: Handle -> ExitCode -> IO a
usage h exitCode = do
	nm <- getProgName
	hPutStr h . unlines $ drop 1 [undefined
		, "USAGE: " ++ nm ++ " (--fceux [ARGS]) | (--everdrive FILE) | --help | -h"
		, ""
		, "This is a tool for interacting with an implementation of NES Dr. Mario that"
		, "supports a custom communication protocol. The comms protocol is game"
		, "agnostic; this tool bakes in knowledge about Dr. Mario to ease interaction."
		, "It prints information about the current game state on stdout, and accepts"
		, "commands on stdin. The exact format is not yet documented. Whoops."
		, ""
		, "    --fceux       Launch and communicate with fceux, passing any extra ARGS"
		, "                  along verbatim"
		, "    --everdrive   Communicate with an Everdrive N8 Pro via the given com"
		, "                  port (e.g. /dev/ttyACM0)"
		, "    --help, -h    Print this message"
		]
	exitWith exitCode

fceux :: [String] -> Environment -> IO ()
fceux args env = do
	script <- getDataFileName "dasyuridia.lua"
	(i, o, e, _p) <- runInteractiveProcess "fceux" ("--loadlua":script:args) Nothing Nothing
	hSetBuffering i LineBuffering
	hSetBuffering o LineBuffering
	hSetBuffering e LineBuffering
	_ <- forkIO . forever $ do
		line <- hGetLine e
		hPrintf stderr "fceux err: %s\n" line
	nesLoop (renderForFceux i) (parseFromFceux o) env

renderForFceux :: Handle -> NESRequest -> IO ()
renderForFceux h req = hPutStrLn h case req of
	NESUnpause -> "unpause"
	NESPause wr -> "pause " ++ renderWriteRead wr
	NESNormal wr -> renderWriteRead wr
	where
	renderWriteRead wr = unwords $ tail [undefined
		, renderWrite (write0 wr)
		, renderWrite (write1 wr)
		, renderRead (read0 wr)
		, renderRead (read1 wr)
		]
	renderWrite w = printf "%d %d" (wAddress w) (wValue w)
	renderRead r = printf "%d %d" (rAddress r) (rSize r)

parseFromFceux :: Handle -> IO NESResponse
parseFromFceux h = go where
	go = hGetLine h >>= \case
		'd':'a':'s':'y':'u':'r':'i':'d':'i':'a':':':line -> case traverse readMaybe (words line) of
			Just ws -> pure ws
			Nothing -> hPrintf stderr "WARNING: Ignoring malformed response %s from dasyuridia.lua\n" line
			        >> go
		line -> hPrintf stderr "fceux out: %s\n" line >> go

everdrive :: FilePath -> Environment -> IO ()
everdrive dev env = do
	wrs <- newIORef mempty
	h <- openPort dev
	hCommand h GetStatus >>= \case
		Left err -> fail $ "Could not get EverDrive status: " ++ show err
		Right ExitSuccess -> pure ()
		Right other -> fail $ "Bad EverDrive status: " ++ show other
	hCommand h GetMode >>= \case
		Left err -> fail $ "Could not get EverDrive mode: " ++ show err
		Right Application -> pure ()
		Right Service -> fail $ "EverDrive still in service mode (perhaps the NES isn't on yet?)"
	nesLoop (renderForEverdrive h wrs) (parseFromEverdrive h wrs) env

renderForEverdrive :: Handle -> IORef (Q WriteRead) -> NESRequest -> IO ()
renderForEverdrive h wrs = \case
	NESUnpause -> hPutStrLn stderr "Pause/unpause not supported by EverDrive backend"
	NESPause wr -> hPutStrLn stderr "Pause/unpause not supported by EverDrive backend" >> go wr
	NESNormal wr -> go wr
	where
	go wr = do
		modifyIORef wrs (pushQ wr)
		result <- hCommand h . WriteMemory addrFIFO . BS.toStrict . BS.toLazyByteString $ n8Encode wr
		case result of
			Left err -> fail $ "Error when writing to EverDrive FIFO: " ++ show err
			Right () -> pure ()

parseFromEverdrive :: Handle -> IORef (Q WriteRead) -> IO NESResponse
parseFromEverdrive h wrsRef = do
	wrs <- readIORef wrsRef
	case popQ wrs of
		Nothing -> fail $ "tried to read more responses than there have been requests"
		Just (wrs', wr) -> do
			writeIORef wrsRef wrs'
			let len0 = fi (rSize (read0 wr)); len1 = fi (rSize (read1 wr))
			bs <- BS.hGet h (len0 + len1)
			return $  (BS.unpack . BS.reverse . BS.take len0) bs
			       ++ (BS.unpack . BS.reverse . BS.drop len0) bs

data Write = Write { wAddress :: Word16, wValue :: Word8 } deriving (Eq, Ord, Read, Show)
data ArrayRead = ArrayRead { rAddress :: Word16, rSize :: Word8 } deriving (Eq, Ord, Read, Show)
data WriteRead = WriteRead
	{ write0, write1 :: Write
	, read0, read1 :: ArrayRead
	} deriving (Eq, Ord, Read, Show)
data NESRequest
	= NESNormal WriteRead
	| NESPause WriteRead
	| NESUnpause
	deriving (Eq, Ord, Read, Show)
type NESResponse = [Word8]

-- a million years ought to be enough for anybody
type FrameCount = Int
newtype Controls = Controls { graphControls :: [(FrameCount, Word8)] } deriving (Eq, Ord, Read, Show)
data ThreadComms = ThreadComms
	{ currentFrame :: FrameCount
	, controlRequests :: Controls
	, paused :: Bool
	} deriving (Eq, Ord, Read, Show)
data Environment = Environment
	{ commsRef :: TVar ThreadComms
	, aiChan :: Chan String
	}

type Q a = ([a], [a])

nesLoop :: (NESRequest -> IO ()) -> IO NESResponse -> Environment -> IO ()
nesLoop qAsync sAsync env = do
	[clk] <- qSync False noWrite0 noWrite1 (ArrayRead addrClock 1) noRead
	clkRef <- newIORef (fi clk)
	go clkRef
	where
	reportBoard rowsw = do
		rows <- traverse (traverse decodeCell) rowsw
		report ("board " ++ map ppCell (concat rows))
	reportLookahead csw = do
		[c0, c1] <- traverse decodeColor csw
		report (printf "lookahead %s%s" (ppColor c0) (ppColor c1))
	reportSpeed = report . ("speed "++) . map toLower . show <=< decodeSpeed
	report = writeChan (aiChan env)

	-- TODO: when opening the emulator to an inLevel state, it would be nice to
	-- get an initial readout of the level situation
	go clkRef = unknown where
		-- miscellaneous states
		unknown = debug "unknown" >> qByte addrUIState >>= \case
			8 -> waitingForVirusesOrFirstControl
			4 -> inLevel
			_ -> relax
		relax = debug "relax" >> readIORef clkRef >>= report . printf "relax %d" >> unknown

		-- level has just started, we haven't started controlling the first pill yet
		waitingForVirusesOrFirstControl = debug "waitingForVirusesOrFirstControl" >> do
			n:row <- q addrP1VirusesToAdd 1 addrP1Board (fi boardWidth)
			case n of
				0 -> readingLevelWaitingForFirstControl boardWidth [row]
				_ -> waitingForViruses
		readingLevelWaitingForFirstControl len rows = debug "readingLevelWaitingForFirstControl" >>
			if len < boardLength
			then do
				st:row <- q addrUIState 1 (addrP1Board + fi len) (fi boardWidth)
				case st of
					8 -> readingLevelWaitingForFirstControl (len+boardWidth) (row:rows)
					4 -> do
						clk <- readIORef clkRef
						report (printf "next control %d" (clk+uiState4TransitionPillTossDelay))
						-- TODO: we could cram the readLookahead and readSpeed
						-- queries into the other half of the board requests
						rows' <- traverse (\offset -> qClk (addrP1Board + fi offset) (fi boardWidth)) [len+boardWidth, len+2*boardWidth .. boardLength-1]
						reportBoard (reverse rows' ++ row:rows)
						readLookahead
						readSpeed
						inLevel
					_ -> relax
			else do
				reportBoard rows
				st0:lk <- q addrUIState 1 addrP1Lookahead 2
				reportLookahead lk
				[st1, spdw] <- qBytes addrUIState addrP1CoarseSpeed
				reportSpeed spdw
				case (st0, st1) of
					(4, _) -> beginPillToss (uiState4TransitionPillTossDelay-1)
					(_, 4) -> beginPillToss uiState4TransitionPillTossDelay
					(_, 8) -> waitingForFirstControl
					_ -> relax
		waitingForViruses = debug "waitingForViruses" >> qByte addrP1VirusesToAdd >>= \case
			0 -> readLevel >> waitingForFirstControl
			_ -> waitingForViruses
		waitingForFirstControl = debug "waitingForFirstControl" >> qByte addrUIState >>= \case
			8 -> waitingForFirstControl
			4 -> beginPillToss uiState4TransitionPillTossDelay
			_ -> relax

		-- we are playing a level; the bulk of the time should be spent in these states
		-- game states:
		-- 0 control
		-- 1 cleanup
		-- 2 attack
		-- 3 delay
		-- 4 impossible
		-- 5 prethrow
		-- 6 throw

		-- in normal flow, inLevel means we're in the control phase, but we can
		-- also get dumped here in an unknown phase at emulator startup
		inLevel = debug "inLevel" >> qBytes addrP1VirusCount addrP1GameState >>= \case
			[0, _] -> relax
			[_, st] -> case st of
				0 -> qByte addrUIState >>= \case 4 -> inLevel; _ -> unknown
				1 -> do
					-- we have 8 frames until we need to be watching for the
					-- next pill toss
					clk <- readIORef clkRef
					-- direction is number of counterclockwise rotations
					d:csw <- q addrP1PillDirection 1 addrP1PillColors 2
					let orientation = if d .&. 1 == 0 then "horizontal" else "vertical"
					[x, y] <- qClk addrP1PillPosition 2
					cs <- (if d > 1 then reverse else id) <$> traverse decodeColor csw
					report (printf "lock %d %s %s (%d, %d)" (clk-1) orientation (cs >>= ppColor) x y)
					cleanup
				2 -> beginPillToss gameState2TransitionPillTossDelay
				3 -> beginControl
				4 -> hPutStrLn stderr "WARNING: entered game state 4, which should be impossible" >> unknown
				5 -> readLevel >> throwing
				6 -> readLevel >> throwing
				_ -> fail $ "unknown game state " ++ show st
		beginPillToss delay = debug (printf "beginPillToss %d" delay) >> do
			clk <- readIORef clkRef
			report ("next control " ++ show (clk+delay))
			readLevel
			throwing
		-- TODO: this does not notice an (ab*#) reset
		throwing = debug "throwing" >> qByte addrP1GameState >>= \case
			000 -> relax -- console reset, not Rev A
			003 -> beginControl
			004 -> relax -- lost, now in UI state 5 (see 0x9ba7)
			006 -> throwing
			255 -> relax -- console reset, Rev A
			st  -> hPutStrLn stderr ("WARNING: unexpected transition from throwing to " ++ show st) >> unknown
		-- we only ever enter this state when we know for sure that this is the
		-- first frame of control
		beginControl = debug "beginControl" >> do
			clk <- readIORef clkRef
			report ("control " ++ show clk)
			readLookahead
			inLevel

		-- we ping-pong between syncing the frame counter, checking whether
		-- we've won, and checking for a reset
		cleanup = cleanupClk
		cleanupClk = debug "cleanupClk" >> qByte addrP1GameState >>= cleanupDispatch cleanupWon
		cleanupWon = debug "cleanupWon" >> qBytes addrP1VirusCount addrP1GameState >>= \case
			[0, _] -> relax
			[_, s] -> cleanupDispatch cleanupUI s
		cleanupUI = debug "cleanupUI" >> qBytes addrUIState addrP1GameState >>= \case
			[4, s] -> cleanupDispatch cleanupClk s
			_ -> unknown
		cleanupDispatch k = \case
			0 -> relax -- console reset
			1 -> k -- cycle to the next thing to check, but stay in cleanup mode
			2 -> beginPillToss gameState2TransitionPillTossDelay
			s -> fail $ "unexpected transition from cleanup game state to " ++ show s

		-- helpers
		readBoard = debug "readBoard" >> do
			doubleRows <- traverse (\offset -> q (addrP1Board + fi offset) (2*fi boardWidth) 0 0) [0,2*boardWidth..boardLength-1]
			let splitDoubleRow dr = let (row, row') = splitAt boardWidth dr in [row, row']
			reportBoard (reverse (doubleRows >>= splitDoubleRow))
		readLookahead = debug "readLookahead" >> qClk addrP1Lookahead 2 >>= reportLookahead
		readSpeed = debug "readSpeed" >> qByte addrP1CoarseSpeed >>= reportSpeed
		readLevel = debug "readLevel" >> readBoard >> readLookahead >> readSpeed

		debug :: String -> IO ()
		debug msg = pure () -- -} readIORef clkRef >>= \clk -> hPrintf stderr "DEBUG (frame %d): %s\n" clk msg

		-- requests
		qByte addr = do
			[val] <- qClk addr 1
			pure val
		qBytes addr0 addr1 = q addr0 1 addr1 1
		qClk addr sz = do
			clkw:rest <- q addrClock 1 addr sz
			clk <- readIORef clkRef
			let diff = clkw - fi clk
			when (diff /= 0) (hPrintf stderr "WARNING: Clock mismatch: predicted %d (%d) vs. actual %d\n" clk (clk .&. 0xff) clkw)
			rest <$ writeIORef clkRef (clk + fi diff)
		q raddr0 rsz0 raddr1 rsz1 = do
			clk <- readIORef clkRef
			writeIORef clkRef (clk+1)
			(reqPause, ctrl) <- atomically do
				comms <- readTVar (commsRef env)
				let (ctrl, ctrls') = popControl clk (controlRequests comms)
				(paused comms, ctrl) <$ writeTVar (commsRef env) comms { currentFrame = clk, controlRequests = ctrls' }
			resp <- qSync reqPause (maybe noWrite0 (Write addrP1Input) ctrl) noWrite1 (ArrayRead raddr0 rsz0) (ArrayRead raddr1 rsz1)
			when reqPause do
				atomically $ readTVar (commsRef env) >>= \case
					ThreadComms { paused = False } -> pure ()
					_ -> retry
				qAsync NESUnpause
			pure resp
		-- end clkRef scope
	qSync pause w0 w1 r0 r1 = qAsync ((if pause then NESPause else NESNormal) (WriteRead w0 w1 r0 r1)) >> sAsync
	uiState4TransitionPillTossDelay = 22
	gameState2TransitionPillTossDelay = 24

decodeCell :: Word8 -> IO Cell
decodeCell 0xff = pure Empty
decodeCell w = do
	occ <- (pure .) . Occupied <$> decodeColor (w .&. 0x0f)
	case w `shiftR` 4 of
		0x4 -> occ North
		0x5 -> occ South
		0x6 -> occ West
		0x7 -> occ East
		0x8 -> occ Disconnected
		0xb -> pure Empty -- clearing (don't know how it chooses between this and the 0xf case)
		0xd -> occ Virus
		0xf -> pure Empty -- clearing
		_ -> fail $ "failed to decode cell byte " ++ show w

decodeSpeed :: Word8 -> IO CoarseSpeed
decodeSpeed = \case
	0 -> pure Low
	1 -> pure Med
	2 -> pure Hi
	w -> fail $ "failed to decode speed byte " ++ show w

decodeColor :: Word8 -> IO Color
decodeColor = \case
	0 -> pure Yellow
	1 -> pure Red
	2 -> pure Blue
	w -> fail $ "failed to decode color " ++ show w

ppCell :: Cell -> Char
ppCell Empty = 'd'
ppCell (Occupied c s) = toEnum (0x60 .|. (1 + fromEnum c) .|. (shiftL (fromEnum s) 2))

ppColor :: Color -> String
ppColor = \case
	Blue -> "b"
	Red -> "r"
	Yellow -> "y"

aiLoop :: Environment -> IO a
aiLoop env = do
	hSetBuffering stdout LineBuffering
	hSetBuffering stdin LineBuffering
	_ <- forkIO . forever $ readChan (aiChan env) >>= putStrLn
	forever do
		s <- getLine
		case parseAIRequest s of
			Nothing -> writeChan (aiChan env) "malformed"
			Just (Debug cmd) -> atomically $ modifyTVar (commsRef env) \comms -> comms { paused = cmd == AIPause }
			Just (Control clk' ctrls' infty) -> do
				problem <- atomically do
					comms@ThreadComms { currentFrame = clk, controlRequests = ctrls } <- readTVar (commsRef env)
					let ctrlsNext = if infty
					    	then ctrls `mergeControlsUntil` pushControls clk' ctrls' emptyControls
					    	else pushControls clk' ctrls' ctrls
					-- minBound check ensures nesLoop has started
					if clk+1 > clk' || clk == minBound
						then pure (Just (clk+1))
						else Nothing <$ writeTVar (commsRef env) comms { controlRequests = ctrlsNext }
				writeChan (aiChan env) case problem of
					Nothing -> "accepted"
					Just clk | clk == minBound+1 -> "unprepared"
					         | otherwise -> printf "rejected %d" clk

data AIRequest
	= Debug DebugRequest
	| Control FrameCount [Maybe Word8] Bool
	deriving (Eq, Ord, Read, Show)

data DebugRequest = AIPause | AIUnpause deriving (Bounded, Enum, Eq, Ord, Read, Show)

parseAIRequest :: String -> Maybe AIRequest
parseAIRequest s = parseControlRequest s <|> parseDebugRequest s

parseDebugRequest :: String -> Maybe AIRequest
parseDebugRequest = \case
	"pause" -> Just (Debug AIPause)
	"unpause" -> Just (Debug AIUnpause)
	_ -> Nothing

-- a number, a space, a control sequence, and an optional q
-- control sequence: many repetitions of a single control
-- control: - for do not override, e for press no buttons, a single button (see singleButton), or parentheses with any number of buttons inside (including 0 or 1)
parseControlRequest :: String -> Maybe AIRequest
parseControlRequest s = case reads s of
	[(clk, ' ':s')] -> go [] s'
		where
		go ws = \case
			[] -> success ws False
			"q" -> success ws True
			c:cs -> case singleButton c of
				w@Just{} -> go (w:ws) cs
				Nothing -> case c of
					'-' -> go (Nothing:ws) cs
					'e' -> go (Just controlNone:ws) cs
					'(' -> multiButton controlNone cs >>= \(w, cs') -> go (Just w:ws) cs'
					_ -> Nothing

		singleButton = \case
			'<' -> Just controlLeft
			'>' -> Just controlRight
			'^' -> Just controlUp
			'v' -> Just controlDown
			'V' -> Just controlDown
			'a' -> Just controlA
			'b' -> Just controlB
			'*' -> Just controlStart -- STARt
			'#' -> Just controlSelect -- used to change the # of players
			_ -> Nothing

		multiButton w = \case
			[] -> Nothing
			')':cs -> Just (w, cs)
			c:cs -> singleButton c >>= \w' -> multiButton (w .|. w') cs

		success ws b = Just (Control clk (reverse ws) b)
	_ -> Nothing

emptyControls :: Controls
emptyControls = Controls []

controls :: [(FrameCount, Word8)] -> Controls
controls = Controls . uniqOn fst . sortOn fst

popControl :: FrameCount -> Controls -> (Maybe Word8, Controls)
popControl clk (Controls overrides) = go overrides where
	go os@((clk', o):ot) = case compare clk clk' of
		LT -> (Nothing, Controls os)
		EQ -> (Just o, Controls ot)
		GT -> go ot
	go os@[] = (Nothing, Controls os)

pushControls :: FrameCount -> [Maybe Word8] -> Controls -> Controls
pushControls clk0 os0 (Controls ps) = Controls (prefix ++ go clk0 os0) where
	(prefix, suffix) = span (earlierThan clk0) ps
	go clk [] = dropWhile (earlierThan clk) suffix
	go clk (Nothing:os) = go (clk+1) os
	go clk (Just w:os) = (clk, w) : go (clk+1) os

mergeControlsUntil :: Controls -> Controls -> Controls
mergeControlsUntil (Controls ps) (Controls ps') = Controls case ps' of
	[] -> ps
	(clk, _):_ -> takeWhile (earlierThan clk) ps ++ ps'

earlierThan :: FrameCount -> (FrameCount, Word8) -> Bool
earlierThan clk (clk', _) = clk' < clk

newEnvironment :: IO Environment
newEnvironment = pure Environment <*> newTVarIO newThreadComms <*> newChan

newThreadComms :: ThreadComms
newThreadComms = ThreadComms
	{ currentFrame = minBound
	, controlRequests = emptyControls
	, paused = False
	}

instance N8Encode Write where n8Encode = (n8Encode . wAddress) <> (n8Encode . wValue)
instance N8Encode ArrayRead where n8Encode = (n8Encode . rAddress) <> (n8Encode . rSize)
instance N8Encode WriteRead where
	n8Encode = (n8Encode . write0) <> (n8Encode . read0) <> (n8Encode . write1) <> (n8Encode . read1)

pushQ :: a -> Q a -> Q a
pushQ a (front, back) = (front, a:back)

popQ :: Q a -> Maybe (Q a, a)
popQ (front, back) = case front of
	a:as -> Just ((as, back), a)
	[] -> case reverse back of
		a:as -> Just ((as, []), a)
		[] -> Nothing

{-# inline fi #-}
fi :: (Integral a, Num b) => a -> b
fi = fromIntegral

uniqOn :: Eq b => (a -> b) -> [a] -> [a]
uniqOn _ [] = []
uniqOn f (a0:as0) = a0 : go (f a0) as0 where
	go _ [] = []
	go b (a:as)
		| b' == b = go b as
		| otherwise = a : go b' as
		where b' = f a

boardWidth, boardHeight :: Int
boardWidth = 8
boardHeight = 16

boardLength :: Int
boardLength = boardWidth * boardHeight

controlNone :: Word8
controlNone = 0

controlRight, controlLeft, controlDown, controlUp, controlStart, controlSelect, controlA, controlB :: Word8
controlRight: controlLeft: controlDown: controlUp: controlStart: controlSelect: controlB: controlA :_ = iterate (`shiftL` 1) 1

noWrite0, noWrite1 :: Write
noWrite0 = Write 0xfffe 0
noWrite1 = Write 0xffff 0

noRead :: ArrayRead
noRead = ArrayRead 0 0

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

offsetLookahead, addrP1Lookahead, addrP2Lookahead :: Word16
offsetLookahead = 0x1a

offsetPillPosition, addrP1PillPosition, addrP2PillPosition :: Word16
offsetPillPosition = 0x5

offsetPillDirection, addrP1PillDirection, addrP2PillDirection :: Word16
offsetPillDirection = 0x25

offsetPillColors, addrP1PillColors, addrP2PillColors :: Word16
offsetPillColors = 0x1

offsetsPlayerState :: [Word16]
offsetsPlayerState = [
 	offsetVirusesToAdd, offsetCoarseSpeed, offsetGameState, offsetVirusCount, offsetLookahead, offsetPillPosition, offsetPillDirection, offsetPillColors]
[	addrP1VirusesToAdd, addrP1CoarseSpeed, addrP1GameState, addrP1VirusCount, addrP1Lookahead, addrP1PillPosition, addrP1PillDirection, addrP1PillColors] = map (addrP1State+) offsetsPlayerState
[	addrP2VirusesToAdd, addrP2CoarseSpeed, addrP2GameState, addrP2VirusCount, addrP2Lookahead, addrP2PillPosition, addrP2PillDirection, addrP2PillColors] = map (addrP2State+) offsetsPlayerState

addrP1Board, addrP2Board :: Word16
addrP1Board = 0x400
addrP2Board = 0x500

addrP1Input, addrP2Input :: Word16
addrP1Input = 0xf5
addrP2Input = 0xf6
