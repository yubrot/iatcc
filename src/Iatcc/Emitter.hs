{-# LANGUAGE GeneralizedNewtypeDeriving #-}
module Iatcc.Emitter where

import Control.Monad
import Control.Monad.RWS (RWS, execRWS)
import Control.Monad.Reader (MonadReader, ask, local)
import Control.Monad.Writer (MonadWriter, tell)
import Control.Monad.State (MonadState, get, put)
import Data.Monoid ((<>))
import Data.Text (Text, pack)
import Data.Char (isAscii, ord)
import Data.Bits (shiftL, xor)

import Iatcc.AST

type StackIndex = Int
type Env = [(Text, Ref)]
type LabelIndex = Integer

data Ref
  = VarRef Text
  | FunRef Text

newtype Emitter a = Emitter { runEmitter :: RWS Env Text LabelIndex a }
  deriving (Monad, Functor, Applicative, MonadReader Env, MonadWriter Text, MonadState LabelIndex)

tshow :: Show a => a -> Text
tshow = pack . show

emitToText :: Program -> Text
emitToText p = snd $ execRWS (runEmitter $ emitProgram p) [] 0

emitProgram :: Program -> Emitter ()
emitProgram (PLetRec funs expr) = do
  labels <- forM funs $ \Fun{..} -> do
    l <- uniqueLabel
    return $ l <> "_" <> funName

  let refs = map funName funs `zip` map FunRef labels
  local (refs <>) $ do
    forM_ (funs `zip` labels) $ \(Fun{..}, label) -> do
      emitFunctionHeader label
      emitArgBinds (-wordSize) funParams $ \nextStackIndex -> do
        emitExpr nextStackIndex True funBody
        emit "ret"
    emitProgram $ PExpr expr

emitProgram (PExpr expr) = do
  emitFunctionHeader "scheme_entry"
  emitRegisterSave "%rdi" "%rcx"  -- args[0]: context* ctx
  emit "movq %rdx, %rbp"          -- args[2]: char* heap
  emit "movq %rsi, %rsp"          -- args[1]: char* stack
  emit "call L_scheme_entry"
  emitRegisterRestore "%rcx"
  emit "ret"
  emitFunctionHeader "L_scheme_entry"
  emitExpr (-wordSize) True expr
  emit "ret"

emitRegisterSave :: Text -> Text -> Emitter ()
emitRegisterSave arg reg = do
  emit $ "movq " <> arg <> ", " <> reg
  emit $ "movq %rbx, 0(" <> reg <> ")"
  emit $ "movq %rbp, 8(" <> reg <> ")"
  emit $ "movq %rsp, 16(" <> reg <> ")"

emitRegisterRestore :: Text -> Emitter ()
emitRegisterRestore reg = do
  emit $ "movq 0(" <> reg <> "), %rbx"
  emit $ "movq 8(" <> reg <> "), %rbp"
  emit $ "movq 16(" <> reg <> "), %rsp"

emitArgBinds :: StackIndex -> [Text] -> (StackIndex -> Emitter a) -> Emitter a
emitArgBinds stackIndex params body = do
  let refs = zipWith (\i param -> (param, VarRef $ tshow i <> "(%rsp)")) [-wordSize, -wordSize*2..] params
  local (refs <>) (body (stackIndex - length params * wordSize))

emitExpr :: StackIndex -> Bool -> Expr -> Emitter ()
emitExpr stackIndex tailPos = \case
  EConst c ->
    emit $ "movq $" <> immediateRep c <> ", %rax"
  EUnary op e -> do
    emitExpr stackIndex False e
    emitUnary op
  EIf test conseq altern -> do
    alt <- uniqueLabel
    end <- uniqueLabel
    do
      emitExpr stackIndex False test
      emit $ "cmpb $" <> tshow constFalse <> ", %al"
      emit $ "je " <> alt
      emitExpr stackIndex tailPos conseq
      emit $ "jmp " <> end
    do
      emitLabel alt
      emitExpr stackIndex tailPos altern
    emitLabel end
  EBinary BinaryAnd a b -> do
    end <- uniqueLabel
    do
      emitExpr stackIndex False a
      emit $ "cmpb $" <> tshow constFalse <> ", %al"
      emit $ "je " <> end
      emitExpr stackIndex tailPos b
    emitLabel end
  EBinary BinaryOr a b -> do
    end <- uniqueLabel
    do
      emitExpr stackIndex False a
      emit $ "cmpb $" <> tshow constFalse <> ", %al"
      emit $ "jne " <> end
      emitExpr stackIndex tailPos b
    emitLabel end
  EBinary BinarySeq a b -> do
    emitExpr stackIndex False a
    emitExpr stackIndex tailPos b
  EBinary op a b -> do
    emitExpr stackIndex False a
    (lhs, nextStackIndex) <- emitStackSave stackIndex
    emitExpr nextStackIndex False b
    emitBinary op lhs
  ELet var init body -> do
    emitExpr stackIndex False init
    (ref, nextStackIndex) <- emitStackSave stackIndex
    local ((var, VarRef ref) :) $ emitExpr nextStackIndex tailPos body
  EVar var -> do
    refs <- ask
    case lookup var refs of
      Just (VarRef ref) -> emit $ "movq " <> ref <> ", %rax"
      _ -> error $ "Unbound variable: " <> show var
  EApp fun args -> do
    refs <- ask
    case lookup fun refs of
      Just (FunRef ref) -> do
        forM_ ([-wordSize, -wordSize*2..] `zip` args) $ \(i, arg) -> do
          emitExpr (stackIndex + i) False arg
          void $ emitStackSave (stackIndex + i)
        if tailPos then do
          forM_ (take (length args) [-wordSize, -wordSize*2..]) $ \i -> do
            emit $ "movq " <> tshow (stackIndex + i) <> "(%rsp), %rax"
            emit $ "movq %rax, " <> tshow i <> "(%rsp)"
          emit $ "jmp " <> ref
        else do
          emit $ "addq $" <> tshow (stackIndex + wordSize) <> ", %rsp"
          emit $ "call " <> ref
          emit $ "subq $" <> tshow (stackIndex + wordSize) <> ", %rsp"
      _ -> error $ "Unknown function: " <> show fun

emitUnary :: UnaryOp -> Emitter ()
emitUnary = \case
  UnaryFxAdd1 ->
    emit $ "addq $" <> immediateRep (CFixnum 1) <> ", %rax"
  UnaryFxSub1 ->
    emit $ "subq $" <> immediateRep (CFixnum 1) <> ", %rax"
  UnaryCharToFx ->
    emit $ "shrq $" <> tshow (charShift - fxShift) <> ", %rax"
  UnaryFxToChar -> do
    emit $ "shlq $" <> tshow (charShift - fxShift) <> ", %rax"
    emit $ "orq $" <> tshow charTag <> ", %rax"
  UnaryIsFixnum -> do
    emit $ "andb $" <> tshow fxTagMask <> ", %al"
    emit $ "cmpb $" <> tshow fxTag <> ", %al"
    emitToBool "sete"
  UnaryIsBoolean -> do
    emit $ "andb $" <> tshow booleanTagMask <> ", %al"
    emit $ "cmpb $" <> tshow booleanTag <> ", %al"
    emitToBool "sete"
  UnaryIsChar -> do
    emit $ "andb $" <> tshow charTagMask <> ", %al"
    emit $ "cmpb $" <> tshow charTag <> ", %al"
    emitToBool "sete"
  UnaryIsFxZero -> do
    emit $ "cmpq $" <> immediateRep (CFixnum 0) <> ", %rax"
    emitToBool "sete"
  UnaryIsNull -> do
    emit $ "cmpb $" <> tshow constNull <> ", %al"
    emitToBool "sete"
  UnaryIsPair -> do
    emit $ "andb $" <> tshow pairTagMask <> ", %al"
    emit $ "cmpb $" <> tshow pairTag <> ", %al"
    emitToBool "sete"
  UnaryNot -> do
    emit $ "cmpb $" <> tshow constFalse <> ", %al"
    emitToBool "sete"
  UnaryLogicalNot -> do
    emit $ "xorq $" <> tshow ((-1) `xor` fxTagMask) <> ", %rax"
  UnaryCar ->
    emit $ "movq " <> tshow (0 - pairTag) <> "(%rax), %rax"
  UnaryCdr ->
    emit $ "movq " <> tshow (toInteger wordSize - pairTag) <> "(%rax), %rax"

emitBinary :: BinaryOp -> Text -> Emitter ()
emitBinary op lhs = case op of
  BinaryAnd ->
    error "emitBinary BinaryAnd"
  BinaryOr ->
    error "emitBinary BinaryOr"
  BinarySeq ->
    error "emitBinary BinarySeq"
  BinaryAdd ->
    emit $ "addq " <> lhs <> ", %rax"
  BinarySub -> do
    emit $ "subq " <> lhs <> ", %rax"
    emit $ "neg %rax"
    emit $ "andb $" <> tshow ((-1) `xor` fxTagMask) <> ", %al"
  BinaryMul -> do
    emit $ "sarq $2, %rax"
    emit $ "imulq " <> lhs <> ", %rax"
  BinaryCons -> do
    -- cdr
    emit $ "movq %rax, " <> tshow wordSize <> "(%rbp)"
    emit $ "movq " <> lhs <> ", %rax"
    -- car
    emit $ "movq %rax, (%rbp)"

    emit $ "movq %rbp, %rax"
    emit $ "orq $" <> tshow pairTag <> ", %rax"
    emit $ "addq $" <> tshow (wordSize * 2) <> ", %rbp"
  BinaryLogicalAnd -> do
    emit $ "andq " <> lhs <> ", %rax"
  BinaryLogicalOr -> do
    emit $ "orq " <> lhs <> ", %rax"
  BinaryEqual -> do
    emit $ "cmpq " <> lhs <> ", %rax"
    emitToBool "sete"
  BinaryNotEqual -> do
    emit $ "cmpq " <> lhs <> ", %rax"
    emitToBool "setne"
  BinaryLesser -> do
    emit $ "cmpq " <> lhs <> ", %rax"
    emitToBool "setg"
  BinaryLesserEqual -> do
    emit $ "cmpq " <> lhs <> ", %rax"
    emitToBool "setge"
  BinaryGreater -> do
    emit $ "cmpq " <> lhs <> ", %rax"
    emitToBool "setl"
  BinaryGreaterEqual -> do
    emit $ "cmpq " <> lhs <> ", %rax"
    emitToBool "setle"
  BinarySetCar -> do
    emit $ "movq " <> lhs <> ", %rdx"
    emit $ "movq %rax, " <> tshow (0 - pairTag) <> "(%rdx)"
  BinarySetCdr -> do
    emit $ "movq " <> lhs <> ", %rdx"
    emit $ "movq %rax, " <> tshow (toInteger wordSize - pairTag) <> "(%rdx)"

emitFunctionHeader :: Text -> Emitter ()
emitFunctionHeader name = do
  emit ".text"
  emit $ ".globl " <> name
  emit $ ".type " <> name <> ", @function"
  emitLabel name

emitToBool :: Text -> Emitter ()
emitToBool cond = do
  emit $ cond <> " %al"
  emit $ "movzbq %al, %rax"
  emit $ "shlq $" <> tshow booleanShift <> ", %rax"
  emit $ "orq $" <> tshow booleanTag <> ", %rax"

emitLabel :: Text -> Emitter ()
emitLabel = tell . (<> ":\n")

emitStackSave :: StackIndex -> Emitter (Text, StackIndex)
emitStackSave stackIndex = do
  emit $ "movq %rax, " <> tshow stackIndex <> "(%rsp)"
  return (tshow stackIndex <> "(%rsp)", stackIndex - wordSize)

emit :: Text -> Emitter ()
emit = tell . (\s -> "    " <> s <> "\n")

uniqueLabel :: Emitter Text
uniqueLabel = do
  count <- get
  put $! count + 1
  return $ "L_" <> tshow count

immediateRep :: Constant -> Text
immediateRep = \case
  CFixnum n ->
    if negate (2 ^ 61) <= n && n <= (2 ^ 61) - 1
      then tshow $ n `shiftL` fxShift
      else error ("Fixnum overflow: " <> show n)
  CBoolean b ->
    tshow $ if b then constTrue else constFalse
  CChar c ->
    if isAscii c
      then tshow (toInteger (ord c) `shiftL` charShift + charTag)
      else error ("Non-ascii characters are not supported: " <> show c)
  CNull ->
    tshow constNull

wordSize :: Int
wordSize = 8

otherShift, charShift, booleanShift, fxShift :: Int
otherShift = 4
charShift = otherShift + 4
booleanShift = otherShift + 2
fxShift = 2

charTag, charTagMask, booleanTag, booleanTagMask, pairTag, pairTagMask, fxTag, fxTagMask :: Integer
charTag        = 0b00001111
charTagMask    = 0b11111111
booleanTag     =   0b101111
booleanTagMask =   0b111111
pairTag        =      0b001
pairTagMask    =      0b111
fxTag          =       0b00
fxTagMask      =       0b11

constFalse, constTrue, constNull :: Integer
constFalse = 0b00101111
constTrue  = 0b01101111
constNull  = 0b00111111
