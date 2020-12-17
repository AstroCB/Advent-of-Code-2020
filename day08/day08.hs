import qualified Data.Set as Set
import qualified Data.Text as Text
import qualified Data.Text.IO as Text

-- --- Day 8: Handheld Halting ---

-- Your flight to the major airline hub reaches cruising altitude without incident. While you consider checking the in-flight menu for one of those drinks that come with a little umbrella, you are interrupted by the kid sitting next to you.

-- Their handheld game console won't turn on! They ask if you can take a look.

-- You narrow the problem down to a strange infinite loop in the boot code (your puzzle input) of the device. You should be able to fix it, but first you need to be able to run the code in isolation.

-- The boot code is represented as a text file with one instruction per line of text. Each instruction consists of an operation (acc, jmp, or nop) and an argument (a signed number like +4 or -20).

-- acc increases or decreases a single global value called the accumulator by the value given in the argument. For example, acc +7 would increase the accumulator by 7. The accumulator starts at 0. After an acc instruction, the instruction immediately below it is executed next.
-- jmp jumps to a new instruction relative to itself. The next instruction to execute is found using the argument as an offset from the jmp instruction; for example, jmp +2 would skip the next instruction, jmp +1 would continue to the instruction immediately below it, and jmp -20 would cause the instruction 20 lines above to be executed next.
-- nop stands for No OPeration - it does nothing. The instruction immediately below it is executed next.
-- For example, consider the following program:

-- nop +0
-- acc +1
-- jmp +4
-- acc +3
-- jmp -3
-- acc -99
-- acc +1
-- jmp -4
-- acc +6
-- These instructions are visited in this order:

-- nop +0  | 1
-- acc +1  | 2, 8(!)
-- jmp +4  | 3
-- acc +3  | 6
-- jmp -3  | 7
-- acc -99 |
-- acc +1  | 4
-- jmp -4  | 5
-- acc +6  |
-- First, the nop +0 does nothing. Then, the accumulator is increased from 0 to 1 (acc +1) and jmp +4 sets the next instruction to the other acc +1 near the bottom. After it increases the accumulator from 1 to 2, jmp -4 executes, setting the next instruction to the only acc +3. It sets the accumulator to 5, and jmp -3 causes the program to continue back at the first acc +1.

-- This is an infinite loop: with this sequence of jumps, the program will run forever. The moment the program tries to run any instruction a second time, you know it will never terminate.

-- Immediately before the program would run an instruction a second time, the value in the accumulator is 5.

-- Run your copy of the boot code. Immediately before any instruction is executed a second time, what value is in the accumulator?

data Instr = Nop Int | Acc Int | Jmp Int
    deriving Show
type Prog = [Instr]

-- Execute the program and determine a final value
exec :: Prog -> Int -> Int -> Set.Set Int -> Int
exec [] _ count _ = count
exec prog line count visited =
    if Set.member newLine visited then count
    else if newLine == length prog then newCount
    else exec prog newLine newCount newVisited
    where
        instr = prog !! line
        newLine = case instr of
            Nop _ -> line + 1
            Acc _ -> line + 1
            Jmp n -> line + n
        newCount = case instr of
            Nop _ -> count
            Acc n -> count + n
            Jmp _ -> count
        newVisited = Set.insert line visited

execProg :: Prog -> Int
execProg prog = exec prog 0 0 Set.empty

-- --- Part Two ---

-- After some careful analysis, you believe that exactly one instruction is corrupted.

-- Somewhere in the program, either a jmp is supposed to be a nop, or a nop is supposed to be a jmp. (No acc instructions were harmed in the corruption of this boot code.)

-- The program is supposed to terminate by attempting to execute an instruction immediately after the last instruction in the file. By changing exactly one jmp or nop, you can repair the boot code and make it terminate correctly.

-- For example, consider the same program from above:

-- nop +0
-- acc +1
-- jmp +4
-- acc +3
-- jmp -3
-- acc -99
-- acc +1
-- jmp -4
-- acc +6
-- If you change the first instruction from nop +0 to jmp +0, it would create a single-instruction infinite loop, never leaving that instruction. If you change almost any of the jmp instructions, the program will still eventually find another jmp instruction and loop forever.

-- However, if you change the second-to-last instruction (from jmp -4 to nop -4), the program terminates! The instructions are visited in this order:

-- nop +0  | 1
-- acc +1  | 2
-- jmp +4  | 3
-- acc +3  |
-- jmp -3  |
-- acc -99 |
-- acc +1  | 4
-- nop -4  | 5
-- acc +6  | 6
-- After the last instruction (acc +6), the program terminates by attempting to run the instruction below the last instruction in the file. With this change, after the program terminates, the accumulator contains the value 8 (acc +1, acc +1, acc +6).

-- Fix the program so that it terminates normally by changing exactly one jmp (to nop) or nop (to jmp). What is the value of the accumulator after the program terminates?

-- Determine whether a program infinite loops by checking whether any lines are
-- visited more than once
infiniteLoops :: Prog -> Int -> Set.Set Int -> Bool
infiniteLoops [] _ _ = False
infiniteLoops prog line visited =
    if Set.member newLine visited then True
    else if newLine < 0 then True
    else if newLine == length prog then False
    else infiniteLoops prog newLine newVisited
    where
        instr = prog !! line
        newLine = case instr of
            Nop _ -> line + 1
            Acc _ -> line + 1
            Jmp n -> line + n
        newVisited = Set.insert line visited

isFinite :: Prog -> Bool
isFinite prog = not (infiniteLoops prog 0 Set.empty)

-- Change the given line's instruction to generate a new, possibly valid, program
modifyInstr :: Int -> Prog -> Prog
modifyInstr line prog =
    foldl (\newProg cur -> newProg ++ [if cur == line then newInstr else (prog !! cur)]) [] [0..(length prog - 1)]
    where
        instr = prog !! line
        newInstr = case instr of
            Nop n -> Jmp n
            Acc n -> Acc n
            Jmp n -> Nop n

-- Find a version of the given program that is finite
makeFinite :: Prog -> Prog
makeFinite prog = 
    head (filter isFinite progOptions)
    where
        len = length prog
        instrList = take len (repeat prog)
        progOptions = foldl (\opts ind -> opts ++ [modifyInstr ind (instrList !! ind)]) [] [0..(len - 1)]

-- Haskell doesn't like a leading "+" when parsing an int (and frankly neither do I)
clean :: String -> String
clean xs = [x | x <- xs, x /= '+']

parseProg :: [String] -> Prog
parseProg [] = []
parseProg (x:xs) =
    instr:(parseProg xs)
    where
        instr = case cmd of
            "nop" -> Nop val
            "acc" -> Acc val
            "jmp" -> Jmp val
        items = words x
        cmd:strVal:[] = items
        val = read (clean strVal)

main :: IO ()
main = do
    ls <- fmap Text.lines (Text.readFile "input.txt")
    let lines = map (\ln -> Text.unpack ln) ls
    let prog = parseProg lines
    let p1 = execProg prog
    let p2 = execProg (makeFinite prog)
    putStrLn $ "Part 1: " ++ show (p1)
    putStrLn $ "Part 2: " ++ show (p2)