{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}

{-# HLINT ignore "Use uncurry" #-}

import Control.Concurrent (threadDelay)
import Control.Exception (Exception, throw)
import Data.Char (chr, ord)
import Data.List (elemIndex)
import Data.Maybe (isNothing)
import System.Directory.Internal.Prelude (getArgs)
import System.Posix (sleep)
import Text.Printf (printf)

-- Invalid Args
newtype InvalidArgs = InvalidArgs [String]
  deriving (Show)

instance Exception InvalidArgs

-- RuntimeException
data RuntimeException = RuntimeException String Int State

instance Show RuntimeException where
  show :: RuntimeException -> String
  show (RuntimeException msg pos state) = "RuntimeException Exception: " ++ msg ++ "\n Error at: " ++ show pos ++ "\n" ++ show state

instance Exception RuntimeException

-- State
-- prev is reversed
data State = State Int [Int] [Int] [Int] -- curr_cell, prev_cells, next_cells, loop_stack

instance Show State where
  show :: State -> String
  show (State curr prev next loop_stack) =
    let all = reverse prev ++ (curr : next)
     in if length all < 32
          then
            "State:\n"
              ++ replicate (length prev * 4) ' '
              ++ "VVV\n"
              ++ foldl (\aux x -> aux ++ printf "%3d " x) "" all
              ++ "\n"
              ++ replicate (length prev) ' '
              ++ "V\n"
              ++ foldr (\x aux -> charf x : aux) "" all
              ++ "\n"
              ++ "loop stack: "
              ++ show loop_stack
              ++ "\n"
          else foldl (\aux x -> aux ++ printf "%02x\n") "" all ++ "\n at " ++ show (length prev) ++ "\n loops: " ++ show loop_stack
    where
      charf :: Int -> Char
      charf x = if x > 31 && x < 127 then chr x else ' '

-- Main
main :: IO ()
main = do
  args <- getArgs
  source <- getSource args
  let _ = validateSource source
  putStrLn "Starting..."
  executeSource source
  putStrLn "\nDone..."

getSource :: [String] -> IO String
getSource [] = throw $ InvalidArgs ["missing arguments"]
getSource ["-c"] = throw $ InvalidArgs ["missing argument for -c"]
getSource [file] = readFile file
getSource ["-c", source] = pure source
getSource args = throw $ InvalidArgs ("invalid args" : args)

validateSource :: String -> ()
validateSource source = aux source [] 0
  where
    aux :: String -> [Int] -> Int -> ()
    aux "" stack _ =
      if null stack
        then ()
        else throw $ InvalidArgs ["Invalid source: loop has no closing bracket at " ++ show (head stack)]
    aux (char : xs) stack pos =
      case char of
        '[' -> aux xs (pos : stack) (pos + 1)
        ']' ->
          if null stack
            then throw $ InvalidArgs ["Invalid source: unexpected end of loop at " ++ show pos ++ ": no opening bracket"]
            else aux xs (tail stack) (pos + 1)
        _ -> aux xs stack (pos + 1)

executeSource :: String -> IO ()
executeSource source = exec (State 0 [] (replicate 16 0) []) 0
  where
    exec :: State -> Int -> IO ()
    exec state pos =
      if length source == pos
        then return ()
        else do
          res <- act state (source !! pos)
          -- print $ fst res
          -- sleep 1
          let newRes = if snd res == -1 then (fst res, pos + 1) else res
          exec (fst newRes) (snd newRes)
      where
        act :: State -> Char -> IO (State, Int)
        -- > = increases memory pointer, or moves the pointer to the right 1 block.
        -- < = decreases memory pointer, or moves the pointer to the left 1 block.
        -- + = increases value stored at the block pointed to by the memory pointer
        -- - = decreases value stored at the block pointed to by the memory pointer
        -- [ = like c while(cur_block_value != 0) loop.
        -- ] = if block currently pointed to's value is not zero, jump back to [
        -- , = like c getchar(). input 1 character.
        -- . = like c putchar(). print 1 character to the console
        act state '>' = rshift state
        act state '<' = lshift state
        act state '+' = increment state
        act state '-' = decrement state
        act state '[' = loopStart state
        act state ']' = loopEnd state
        act state ',' = inputChar state
        act state '.' = outputChar state
        act state action = pure (state, -1) -- skip unknown characters
        --
        rshift :: State -> IO (State, Int)
        rshift (State curr prev next loop_stack) =
          if null next
            then
              if length prev > 10000
                then throw $ RuntimeException "out of memory" pos state
                else pure (State 0 (curr : prev) (replicate 100 0) loop_stack, -1)
            else pure (State (head next) (curr : prev) (tail next) loop_stack, -1)

        lshift :: State -> IO (State, Int)
        lshift (State curr prev next loop_stack) =
          if null prev
            then throw $ RuntimeException "going into negative memory" pos state
            else pure (State (head prev) (tail prev) (curr : next) loop_stack, -1)

        increment :: State -> IO (State, Int)
        increment (State curr prev next loop_stack) =
          pure (State ((curr + 1) `mod` 256) prev next loop_stack, -1)

        decrement :: State -> IO (State, Int)
        decrement (State curr prev next loop_stack) =
          pure (State ((curr + 255) `mod` 256) prev next loop_stack, -1)

        loopStart :: State -> IO (State, Int)
        loopStart (State curr prev next loop_stack) =
          let endLoopPos = elemIndex ']' (drop pos source)
           in case endLoopPos of
                Nothing -> throw $ RuntimeException "missing loop end" pos state
                Just endPos ->
                  if curr > 0
                    then pure (State curr prev next (pos : loop_stack), -1)
                    else pure (State curr prev next loop_stack, endPos + pos + 1)

        loopEnd :: State -> IO (State, Int)
        loopEnd (State curr prev next loop_stack) =
          if null loop_stack
            then throw $ RuntimeException "unexpected loop end" pos state
            else pure (State curr prev next (tail loop_stack), head loop_stack)

        inputChar :: State -> IO (State, Int)
        inputChar (State curr prev next loop_stack) = do
          input <- getChar
          return (State (ord input `mod` 256) prev next loop_stack, -1)

        outputChar :: State -> IO (State, Int)
        outputChar (State curr prev next loop_stack) = do
          putChar $ chr curr
          -- print $ chr curr
          return (State curr prev next loop_stack, -1)
