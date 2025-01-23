module Main where

import Control.Applicative
import Control.Concurrent
import Control.Concurrent.STM
import Control.Monad
import Data.Bits
import Data.IORef
import Data.List
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
	env <- newEnvironment
	_ <- forkIO (aiLoop env)
	case args of
		"--fceux":fceuxArgs -> fceux fceuxArgs env
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

renderForFceux :: Handle -> Request -> IO ()
renderForFceux h req = hPutStrLn h . unwords $ tail [undefined
	, renderWrite (write0 req)
	, renderWrite (write1 req)
	, renderRead (read0 req)
	, renderRead (read1 req)
	] where
	renderWrite w = printf "%d %d" (wAddress w) (wValue w)
	renderRead r = printf "%d %d" (rAddress r) (rSize r)

parseFromFceux :: Handle -> IO Response
parseFromFceux h = go where
	go = hGetLine h >>= \case
		'd':'a':'s':'y':'u':'r':'i':'d':'i':'a':':':line -> case traverse readMaybe (words line) of
			Just ws -> pure ws
			Nothing -> hPrintf stderr "WARNING: Ignoring malformed response %s from dasyuridia.lua\n" line
			        >> go
		line -> hPrintf stderr "fceux out: %s\n" line >> go

data Write = Write { wAddress :: Word16, wValue :: Word8 } deriving (Eq, Ord, Read, Show)
data ArrayRead = ArrayRead { rAddress :: Word16, rSize :: Word8 } deriving (Eq, Ord, Read, Show)
data Request = Request
	{ write0, write1 :: Write
	, read0, read1 :: ArrayRead
	} deriving (Eq, Ord, Read, Show)
type Response = [Word8]

-- a million years ought to be enough for anybody
type FrameCount = Int
newtype Controls = Controls { graphControls :: [(FrameCount, Word8)] } deriving (Eq, Ord, Read, Show)
data Environment = Environment
	{ controlsRef :: TVar (FrameCount, Controls)
	, aiChan :: Chan String
	}

nesLoop :: (Request -> IO ()) -> IO Response -> Environment -> IO ()
nesLoop qAsync sAsync env = do
	[clk] <- qSync noWrite0 noWrite1 (ArrayRead addrClock 1) noRead
	clkRef <- newIORef (fi clk)
	go clkRef
	where
	reportLevel spdw rowsw = do
		spd <- decodeSpeed spdw
		rows <- traverse (traverse decodeCell) rowsw
		report (show spd)
		report . pp $ unsafeGenerateBoard boardWidth boardHeight \pos -> rows !! y pos !! x pos
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
				0 -> readingLevelWaitingForFirstControl boardWidth [row] -- TODO: add an intermediate state to report when control is available next
				_ -> waitingForViruses
		readingLevelWaitingForFirstControl len rows = debug "readingLevelWaitingForFirstControl" >>
			if len < boardLength
			then do
				st:row <- q addrUIState 1 (addrP1Board + fi len) (fi boardWidth)
				case st of
					8 -> readingLevelWaitingForFirstControl (len+boardWidth) (row:rows)
					-- TODO: no, next control is about 25 frames away
					4 -> readIORef clkRef >>= report . printf "control %d" >> readingLevelInLevel (len+boardWidth) (row:rows)
					_ -> relax
			else do
				[st, spdw] <- qBytes addrUIState addrP1CoarseSpeed
				reportLevel spdw rows
				case st of
					8 -> waitingForFirstControl
					4 -> beginPillToss uiState4TransitionPillTossDelay
					_ -> relax
		waitingForViruses = debug "waitingForViruses" >> qByte addrP1VirusesToAdd >>= \case
			0 -> readLevel >> waitingForFirstControl
			_ -> waitingForViruses
		waitingForFirstControl = debug "waitingForFirstControl" >> qByte addrUIState >>= \case
			8 -> waitingForFirstControl
			4 -> beginPillToss uiState4TransitionPillTossDelay
			_ -> relax

		-- uncomfortable limbo: we've just started with the first pill of the
		-- level, but haven't finished reading the game state yet, so we're sort of
		-- in the previous group and sort of in the next group
		readingLevelInLevel len rows = debug "readingLevelInLevel" >> do
			-- TODO: we can advance our knowledge of the game state more cleverly
			-- than this using the other half of each request
			rows' <- traverse (\offset -> qClk (addrP1Board + fi offset) (fi boardWidth)) [len, len+boardWidth .. boardLength-1]
			spdw <- qByte addrP1CoarseSpeed
			reportLevel spdw (reverse rows' ++ rows)
			inLevel

		-- we are playing a level; the bulk of the time should be spent in these states
		-- game states:
		-- 0 control
		-- 1 cleanup
		-- 2 attack
		-- 3 delay
		-- 4 impossible
		-- 5 prethrow
		-- 6 throw
		inLevel = debug "inLevel" >> qBytes addrP1VirusCount addrP1GameState >>= \case
			[0, _] -> relax
			[_, st] -> case st of
				0 -> qByte addrUIState >>= \case 4 -> inLevel; _ -> unknown
				1 -> readIORef clkRef >>= \clk -> report ("lock " ++ show (clk-1)) >> cleanupClk
				2 -> beginPillToss gameState2TransitionPillTossDelay
				3 -> beginControl
				4 -> unknown
				5 -> readLevel >> throwing
				6 -> throwing
				_ -> fail $ "unknown game state " ++ show st
		beginPillToss delay = debug (printf "beginPillToss %d" delay) >> do
			clk <- readIORef clkRef
			report ("next control " ++ show (clk+delay))
			readLevel
			throwing
		throwing = debug "throwing" >> qByte addrP1GameState >>= \case
			000 -> relax -- console reset, not Rev A
			003 -> beginControl
			006 -> throwing
			255 -> relax -- console reset, Rev A
			st  -> hPutStrLn stderr ("WARNING: unexpected transition from throwing to " ++ show st) >> unknown
		beginControl = debug "beginControl" >> do
			clk <- readIORef clkRef
			report ("next control " ++ show clk)
			inLevel

		-- we ping-pong between syncing the frame counter and checking whether
		-- we've won
		cleanupClk = debug "cleanupClk" >> qByte addrP1GameState >>= cleanupDispatch cleanupWon
		cleanupWon = debug "cleanupWon" >> qBytes addrP1VirusCount addrP1GameState >>= \case
			[0, _] -> relax
			[_, s] -> cleanupDispatch cleanupClk s
		cleanupDispatch k = \case
			0 -> relax -- console reset
			1 -> k
			2 -> beginPillToss gameState2TransitionPillTossDelay
			s -> fail $ "unexpected transition from cleanup game state to " ++ show s

		-- helpers
		readLevel = debug "readLevel" >> do
			doubleRows <- traverse (\offset -> q (addrP1Board + fi offset) (2*fi boardWidth) 0 0) [0,2*boardWidth..boardLength-1]
			spdw <- qByte addrP1CoarseSpeed
			let splitDoubleRow dr = let (row, row') = splitAt boardWidth dr in [row, row']
			reportLevel spdw $ reverse (doubleRows >>= splitDoubleRow)

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
			ctrl <- atomically do
				(_, ctrls) <- readTVar (controlsRef env)
				let (ctrl, ctrls') = popControl clk ctrls
				ctrl <$ writeTVar (controlsRef env) (clk, ctrls')
			qSync (maybe noWrite0 (Write addrP1Input) ctrl) noWrite1 (ArrayRead raddr0 rsz0) (ArrayRead raddr1 rsz1)
		-- end clkRef scope

	debug :: String -> IO ()
	debug = const (pure ()) -- hPrintf stderr "DEBUG: %s\n"
	qSync w0 w1 r0 r1 = qAsync (Request w0 w1 r0 r1) >> sAsync
	uiState4TransitionPillTossDelay = 22
	gameState2TransitionPillTossDelay = 24

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

aiLoop :: Environment -> IO a
aiLoop env = do
	hSetBuffering stdout LineBuffering
	hSetBuffering stdin LineBuffering
	_ <- forkIO . forever $ readChan (aiChan env) >>= putStrLn
	forever do
		s <- getLine
		case parseControlRequest s of
			Nothing -> writeChan (aiChan env) "malformed"
			Just (clk', ctrls', infty) -> do
				problem <- atomically do
					(clk, ctrls) <- readTVar (controlsRef env)
					let ctrlsNext = if infty
					    	then ctrls `mergeControlsUntil` pushControls clk' ctrls' emptyControls
					    	else pushControls clk' ctrls' ctrls
					-- minBound check ensures nesLoop has started
					if clk+1 > clk' || clk == minBound
						then pure (Just (clk+1))
						else Nothing <$ writeTVar (controlsRef env) (clk, ctrlsNext)
				writeChan (aiChan env) case problem of
					Nothing -> "accepted"
					Just clk | clk == minBound -> "unprepared"
					         | otherwise -> printf "rejected %d" clk

-- a number, a space, a control sequence, and an optional q
-- control sequence: many repetitions of a single control
-- control: - for do not override, e for press no buttons, a single button (see singleButton), or parentheses with any number of buttons inside (including 0 or 1)
parseControlRequest :: String -> Maybe (FrameCount, [Maybe Word8], Bool)
parseControlRequest s = case reads s of
	[(clk, ' ':s')] -> go [] s'
		where
		go ws = \case
			[] -> Just (clk, reverse ws, False)
			"q" -> Just (clk, reverse ws, True)
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
newEnvironment = liftA2 Environment
	(newTVarIO (minBound, emptyControls))
	newChan

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

offsetsPlayerState :: [Word16]
offsetsPlayerState = [
 	offsetVirusesToAdd, offsetCoarseSpeed, offsetGameState, offsetVirusCount]
[	addrP1VirusesToAdd, addrP1CoarseSpeed, addrP1GameState, addrP1VirusCount] = map (addrP1State+) offsetsPlayerState
[	addrP2VirusesToAdd, addrP2CoarseSpeed, addrP2GameState, addrP2VirusCount] = map (addrP2State+) offsetsPlayerState

addrP1Board, addrP2Board :: Word16
addrP1Board = 0x400
addrP2Board = 0x500

addrP1Input, addrP2Input :: Word16
addrP1Input = 0xf5
addrP2Input = 0xf6
