module Day17 (testAll) where
-- module Day17 (testAll, answer, answer2) where

import GHC.Internal.Bits (xor)
import qualified Data.Maybe as DM
import qualified Data.List as DL

import TestLib (Comparation(Comparation))

newtype ThreeBit = ThreeBit Int
  deriving (Eq, Show)

data Instruction = Adv | Bxl | Bst | Jnz | Bxc | Out_ | Bdv | Cdv 
newtype Operand = Operand ThreeBit

newtype Registers = Registers (Integer, Integer, Integer)
  deriving (Eq, Show)

readOperand :: Operand -> Registers -> Either String Integer
readOperand (Operand (ThreeBit n)) (Registers (regA, regB, regC))
  | n >= 0 && n <= 3 = Right $ fromIntegral n
  | n == 4 = Right regA
  | n == 5 = Right regB
  | n == 6 = Right regC
  | otherwise = Left $ "Attempt to read the invalid operand [" ++ show n ++ "]" 

returnRegisters :: (Integer, Integer, Integer) -> Either String (Registers, Maybe ThreeBit, Maybe ThreeBit)
returnRegisters x = Right (Registers x, Nothing, Nothing)

executeInstruction :: Instruction -> Operand -> Registers -> Either String (Registers, Maybe ThreeBit, Maybe ThreeBit)
executeInstruction Adv opc regs@(Registers (regA, regB, regC)) = do
  denominatorPower <- readOperand opc regs
  let newA = (regA `div` (2 ^ denominatorPower))
    in returnRegisters (newA, regB, regC)
executeInstruction Bxl (Operand (ThreeBit opcLiteral)) (Registers (regA, regB, regC)) =
  let newB = regB `xor` (fromIntegral opcLiteral)
  in returnRegisters (regA, newB, regC)
executeInstruction Bst opc regs@(Registers (regA, _, regC)) = do
  newB <- readOperand opc regs
  returnRegisters (regA, newB `mod` 8, regC)
executeInstruction Jnz (Operand (ThreeBit opcLiteral)) regs@(Registers (regA, regB, regC))
  | regA == 0 = returnRegisters (regA, regB, regC)
  | otherwise = Right (regs, Nothing, Just $ ThreeBit opcLiteral)
executeInstruction Bxc _ (Registers (regA, regB, regC)) =
  let newB = regB `xor` regC
  in returnRegisters (regA, newB, regC)
executeInstruction Out_ opc regs = do
  comboValue <- readOperand opc regs
  Right (regs, Just $ ThreeBit $ fromIntegral (comboValue `mod` 8), Nothing)
executeInstruction Bdv opc regs@(Registers (regA, _, regC)) = do
  denominatorPower <- readOperand opc regs
  let newB = (regA `div` (2 ^ denominatorPower))
    in returnRegisters (regA, newB, regC)
executeInstruction Cdv opc regs@(Registers (regA, regB, _)) = do
  denominatorPower <- readOperand opc regs
  let newC = (regA `div` (2 ^ denominatorPower))
    in returnRegisters (regA, regB, newC)

testReadOperand :: IO ()
testReadOperand =
  let regs :: Registers
      regs = Registers (60, 802, 0 - 9)
      comps :: [Comparation (Either String Integer)]
      comps =
        [ Comparation (101, Right 0, readOperand (Operand (ThreeBit 0)) regs)
        , Comparation (102, Right 1, readOperand (Operand (ThreeBit 1)) regs)
        , Comparation (103, Right 3, readOperand (Operand (ThreeBit 3)) regs)
        , Comparation (104, Right 60, readOperand (Operand (ThreeBit 4)) regs)
        , Comparation (105, Right 802, readOperand (Operand (ThreeBit 5)) regs)
        , Comparation (106, Right (0 - 9), readOperand (Operand (ThreeBit 6)) regs)
        , Comparation (107, Left "Attempt to read the invalid operand [7]", readOperand (Operand (ThreeBit 7)) regs)
        ]
  in do
    (putStrLn . unlines . map show) comps

testInstructions :: IO ()
testInstructions =
  let advComps :: [Comparation ( Either String (Registers, Maybe ThreeBit, Maybe ThreeBit) )]
      advComps =
        [ Comparation (201, returnRegisters (5120, 44, 901), macroExecute Adv 0 $ Registers (5120, 44, 901))
        , Comparation (202, returnRegisters (1280, 4, 901), macroExecute Adv 2 $ Registers (5120, 4, 901))
        , Comparation (203, returnRegisters (10, 9, 550), macroExecute Adv 5 $ Registers (5120, 9, 550))
        ]
      bxlComps :: [Comparation ( Either String (Registers, Maybe ThreeBit, Maybe ThreeBit) )]
      bxlComps =
        [ Comparation (211, returnRegisters (231, 44, 10), macroExecute Bxl 0 $ Registers (231, 44, 10))
        , Comparation (212, returnRegisters (231, 45, 10), macroExecute Bxl 1 $ Registers (231, 44, 10))
        , Comparation (213, returnRegisters (90, 41, 10), macroExecute Bxl 5 $ Registers (90, 44, 10))
        ]
      bstComps :: [Comparation ( Either String (Registers, Maybe ThreeBit, Maybe ThreeBit) )]
      bstComps =
        [ Comparation (221, returnRegisters (5000, 0, 875), macroExecute Bst 0 $ Registers (5000, 44, 875))
        , Comparation (222, returnRegisters (5000, 2, 875), macroExecute Bst 2 $ Registers (5000, 44, 875))
        , Comparation (223, returnRegisters (5000, 0, 875), macroExecute Bst 4 $ Registers (5000, 44, 875))
        , Comparation (224, returnRegisters (5000, 3, 875), macroExecute Bst 6 $ Registers (5000, 44, 875))
        ]
      jnzComps :: [Comparation ( Either String (Registers, Maybe ThreeBit, Maybe ThreeBit) )]
      jnzComps =
        [ Comparation (231, returnRegisters (0, 27, 388), macroExecute Jnz 6 $ Registers (0, 27, 388))
        , Comparation (232, Right (Registers (5, 27, 388), Nothing, Just (ThreeBit 2)), macroExecute Jnz 2 $ Registers (5, 27, 388))
        , Comparation (233, Right (Registers (5, 4, 3), Nothing, Just (ThreeBit 6)), macroExecute Jnz 6 $ Registers (5, 4, 3))
        ]
      bxcComps :: [Comparation ( Either String (Registers, Maybe ThreeBit, Maybe ThreeBit) )]
      bxcComps =
        [ Comparation (241, returnRegisters (9, 8, 4), macroExecute Bxc 0 $ Registers (9, 12, 4))
        , Comparation (242, returnRegisters (9, 8, 4), macroExecute Bxc 5 $ Registers (9, 12, 4))
        , Comparation (243, returnRegisters (9, 23, 0), macroExecute Bxc 5 $ Registers (9, 23, 0))
        ]
      outComps :: [Comparation ( Either String (Registers, Maybe ThreeBit, Maybe ThreeBit) )]
      outComps =
        [ Comparation (251, Right (Registers (0, 1, 27), Just (ThreeBit 0), Nothing), macroExecute Out_ 0 $ Registers (0, 1, 27))
        , Comparation (252, Right (Registers (0, 1, 27), Just (ThreeBit 1), Nothing), macroExecute Out_ 1 $ Registers (0, 1, 27))
        , Comparation (253, Right (Registers (0, 1, 27), Just (ThreeBit 2), Nothing), macroExecute Out_ 2 $ Registers (0, 1, 27))
        , Comparation (254, Right (Registers (0, 1, 27), Just (ThreeBit 1), Nothing), macroExecute Out_ 5 $ Registers (0, 1, 27))
        , Comparation (255, Right (Registers (0, 1, 27), Just (ThreeBit 3), Nothing), macroExecute Out_ 6 $ Registers (0, 1, 27))
        , Comparation (256, Right (Registers (506, 12, 14), Just (ThreeBit 6), Nothing), macroExecute Out_ 6 $ Registers (506, 12, 14))
        ]
      bdvComps :: [Comparation ( Either String (Registers, Maybe ThreeBit, Maybe ThreeBit) )]
      bdvComps =
        [ Comparation (261, returnRegisters (5120, 5120, 901), macroExecute Bdv 0 $ Registers (5120, 44, 901))
        , Comparation (262, returnRegisters (5120, 1280, 901), macroExecute Bdv 2 $ Registers (5120, 4, 901))
        , Comparation (263, returnRegisters (5120, 10, 550), macroExecute Bdv 5 $ Registers (5120, 9, 550))
        ]
      cdvComps :: [Comparation ( Either String (Registers, Maybe ThreeBit, Maybe ThreeBit) )]
      cdvComps =
        [ Comparation (271, returnRegisters (5120, 44, 5120), macroExecute Cdv 0 $ Registers (5120, 44, 901))
        , Comparation (272, returnRegisters (5120, 4, 1280), macroExecute Cdv 2 $ Registers (5120, 4, 901))
        , Comparation (273, returnRegisters (5120, 9, 10), macroExecute Cdv 5 $ Registers (5120, 9, 550))
        ]
  in do
    (putStrLn . unlines . map show) advComps
    (putStrLn . unlines . map show) bxlComps
    (putStrLn . unlines . map show) bstComps
    (putStrLn . unlines . map show) jnzComps
    (putStrLn . unlines . map show) bxcComps
    (putStrLn . unlines . map show) outComps
    (putStrLn . unlines . map show) bdvComps
    (putStrLn . unlines . map show) cdvComps
  where macroExecute :: Instruction -> Int -> Registers -> Either String (Registers, Maybe ThreeBit, Maybe ThreeBit)
        macroExecute instV operandV = executeInstruction instV (Operand (ThreeBit operandV))

newtype ProgramOutput = ProgramOutput [ThreeBit]
  deriving (Eq, Show)

executeScript :: [(Instruction, Operand)] -> Int -> (ProgramOutput, Registers) -> Either String (ProgramOutput, Registers)
executeScript instructions nextExecLine (progOutput, regs)
  | nextExecLine >= ((length instructions) * 2) = Right (progOutput, regs)
  | otherwise = do
      nextLineResult <-
        (uncurry executeInstruction)
          (instructions !! (nextExecLine `div` 2))
          regs
      let (newRegs, newOutput, newExecLine) = nextLineResult
          yExecLine :: Int
          yExecLine = ((DM.fromMaybe (nextExecLine + 2)) . fmap (\(ThreeBit x) -> x)) newExecLine
          xOutput :: [ThreeBit]
          xOutput = (\(ProgramOutput x) -> x) progOutput
          yOutput :: ProgramOutput
          yOutput = ProgramOutput ( xOutput ++ DM.maybeToList newOutput )
        in executeScript instructions yExecLine (yOutput, newRegs)

testScript :: IO ()
testScript =
  let programU :: (Registers, [(Instruction, Operand)])
      programU =
        (Registers (72, 5, 4),
          [ programLine Adv 1
          , programLine Bxc 2
          , programLine Adv 5
          ])
      noOutput :: ProgramOutput
      noOutput = ProgramOutput []
      orderComp :: Comparation (Either String (ProgramOutput, Registers))
      orderComp =
        Comparation (301, Right (noOutput, Registers (18, 1, 4)), executeScript (snd programU) 0 (noOutput, fst programU))
      programV :: (Registers, [(Instruction, Operand)])
      programV =
        (Registers (72, 5, 4),
          [ programLine Jnz 4
          , programLine Bxc 2
          , programLine Adv 5
          ])
      jumpComp :: Comparation (Either String (ProgramOutput, Registers))
      jumpComp =
        Comparation (302, Right (noOutput, Registers (2, 5, 4)), executeScript (snd programV) 0 (noOutput, fst programV))
      programW :: (Registers, [(Instruction, Operand)])
      programW = 
        (fst programU,
          DL.intercalate [programLine Out_ 4, programLine Out_ 5, programLine Out_ 6]
              $ map (:[])
              $ snd programU)
      outputComps :: [Comparation (Either String (ProgramOutput, Registers))]
      outputComps =
        [ Comparation (303, Right (fromOutput [4, 5, 4, 4, 1, 4], Registers (18, 1, 4)), executeScript (snd programW) 0 (noOutput, fst programW))
        , Comparation (304, Right (fromOutput [7, 4, 5, 4, 4, 1, 4], Registers (18, 1, 4)), executeScript (snd programW) 0 (fromOutput [7], fst programW))
        ]
  in do
    (putStrLn . show) orderComp
    (putStrLn . show) jumpComp
    (putStrLn . unlines . map show) outputComps
  where programLine :: Instruction -> Int -> (Instruction, Operand)
        programLine i o = (i, Operand (ThreeBit o))
        fromOutput :: [Int] -> ProgramOutput
        fromOutput = ProgramOutput . map ThreeBit

testAll :: IO ()
testAll = do
  testReadOperand
  testInstructions
  testScript 

part1Input :: (Registers, [(Instruction, Operand)])
part1Input =
  (Registers (11, 0, 0)
  , [ programLine Bst 4
    , programLine Bxl 1
    , programLine Cdv 5
    , programLine Bxl 5
    , programLine Bxc 3
    , programLine Adv 3
    , programLine Out_ 5
    , programLine Jnz 0
    ])
  where programLine :: Instruction -> Int -> (Instruction, Operand)
        programLine i o = (i, Operand (ThreeBit o))

{-
answer :: IO ()
answer =
  print (executeScript (snd part1Input) 0 (ProgramOutput [], fst part1Input))
  -}

{-
 - The program only outputs register B mod 8.
 - > B mod 8 needs to be 2,4,1,1,7,5,1,5,4,3,0,3,5,5,3,0
 -
 - Program in easier language (lua):
 -             do
 -   Bst 4   |    b = a % 8
 -   Bxl 1   |    b = xor(b, 1)
 -   Cdv 5   |    c = a / (2 ^ b)
 -   Bxl 5   |    b = xor(b, 5)
 -   Bxc 3   |    b = xor(b, c)
 -   Adv 3   |    a = a / 8
 -   Out_ 5  |    print(b % 8)
 -   Jnz 0   | while a > 0
 -             end
 -
 - Merging first 5 lines:
 -    b = 
 -      xor(
 -        xor(xor(a % 8, 1), 5),
 -        a / 2 ^(  xor(a % 8, 1)  )
 -      )
 -    a = a / 8
 -    print(b)
 -
 -
 - -}

findAFromPrint :: Int -> Integer -> [Integer]
findAFromPrint outP a =
  map (+ (a * 8))
    $ filter (\x ->
                  modOfBPasses
                    outP
                    (fromInteger (a `mod` 512))
                    (fromInteger x)
              )
              [0..7]

  where modOfBPasses :: Int -> Int -> Int -> Bool
        modOfBPasses outP' a' x' =
          let previousA = (8 * a') + x'
              b2 = (previousA `mod` 8) `xor` 1
              c = (previousA `div` (2 ^ b2))
              finalB = (b2 `xor` 5) `xor` c
          in finalB `mod` 8 == outP'
        
possibleAs :: Integer -> [Int] -> [Integer]
possibleAs start outPs = foldr
  (\outP xsAcc -> concatMap (\x -> findAFromPrint outP x) xsAcc)
  [start]
  outPs

{-
answer2 :: IO ()
answer2 = do
  print
    $ DL.sort
    -- $ DL.minimum $ ((-1):)
    $ possibleAs 0 [2,4,1,1,7,5,1,5,4,3,0,3,5,5,3,0]
  print
    $ DL.sort
    -- $ DL.minimum $ ((-1):)
    $ possibleAs 0 [2,4,1,1,7,5]
  print $ runProgramWithA 37
  print $ runProgramWithA 56
  print $ runProgramWithA 164541017976509
  print
    ( Comparation
      ( 1001
      , Right $ ProgramOutput $ map ThreeBit [2,4,1,1,7,5,1,5,4,3,0,3,5,5,3,0]
      , fmap (\(e, _) -> e) $ runProgramWithA 164541017976509
      ))
  where runProgramWithA :: Integer -> Either String (ProgramOutput, Registers)
        runProgramWithA a' =
          executeScript
            (snd part1Input)
            0
            (ProgramOutput [], Registers (a', 0, 0))
            -}
