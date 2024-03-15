module Assembly where

import Data.Set (Set, empty, insert, toList)
import IR (IRVar, Instruction (..))
import SymTab (SymTab (..), lookup)
import Tokenizer (Location)

getAllIRVars :: [(Instruction, Location)] -> [IRVar]
getAllIRVars instr = toList $ getAllIRVars' instr empty
  where
    getAllIRVars' :: [(Instruction, Location)] -> Set IRVar -> Set IRVar
    getAllIRVars' [] vars = vars
    getAllIRVars' ((instr, _) : rest) vars = case instr of
      LoadIntConst _ var -> insert var restVars
      LoadBoolConst _ var -> insert var restVars
      Copy var1 var2 -> insert var1 $ insert var2 restVars
      Call var1 vars var2 -> insert var1 $ insert var2 $ foldr insert restVars vars
      CondJump var _ _ -> insert var restVars
      _ -> restVars
      where
        restVars = getAllIRVars' rest vars

locals :: [IRVar] -> SymTab String
locals vars = SymTab {parent = Nothing, symbols = symbols}
  where
    symbols = map (\(i, var) -> (var, show (-8 * (i + 1)) ++ "(%rbp)")) (zip [0 ..] vars)

stackUsed :: SymTab String -> Int
stackUsed (SymTab _ symbols) = 8 * (length symbols + 1)

genAssembly' :: Instruction -> SymTab String -> [String]
genAssembly' ins vars = case ins of
  Label label -> [".L" ++ label ++ ":"]
  LoadIntConst val var -> ["movq $" ++ show val ++ ", " ++ getRef var]
  LoadBoolConst val var -> ["movq $" ++ (if val then "1" else "0") ++ ", " ++ getRef var]
  Copy src dest ->
    [ "movq " ++ getRef src ++ ", %rax",
      "movq %rax, " ++ getRef dest
    ]
  CondJump cond thenLabel elseLabel ->
    [ "cmpq $0, " ++ getRef cond,
      "jne .L" ++ thenLabel,
      "jmp .L" ++ elseLabel
    ]
  Jump label -> ["jmp .L" ++ label]
  Call "+" [a, b] result ->
    [ "movq " ++ getRef a ++ ", %rax",
      "addq " ++ getRef b ++ ", %rax",
      "movq %rax, " ++ getRef result
    ]
  Call fun vars result ->
    let varRegisters = zip vars ["%rdi", "%rsi", "%rdx", "%rcx", "%r8", "%r9"]
        varInstructions = map (\(var, reg) -> "movq " ++ getRef var ++ ", " ++ reg) varRegisters
     in varInstructions
          ++ [ "callq " ++ fun,
               "movq %rax, " ++ getRef result
             ]
  where
    getRef :: String -> String
    getRef var = case SymTab.lookup var vars of
      Just ref -> ref
      Nothing -> error "Variable not found"

header :: SymTab String -> [String]
header locals =
  [ ".extern print_int",
    ".extern print_bool",
    ".extern read_int",
    ".global main",
    ".type main, @function",
    ".section .text",
    "main:",
    "pushq %rbp",
    "movq %rsp, %rbp",
    "subq $" ++ show (stackUsed locals) ++ ", %rsp"
  ]

footer :: [String]
footer =
  [ "movq $0, %rax",
    "movq %rbp, %rsp",
    "popq %rbp",
    "ret"
  ]

genAssembly :: [(Instruction, Location)] -> String
genAssembly instr = program
  where
    vars = locals $ getAllIRVars instr
    h = unlines $ header vars
    f = unlines footer
    source = unlines $ ".Lstart:" : concatMap (\(ins, _) -> genAssembly' ins vars) instr
    program = h ++ source ++ f