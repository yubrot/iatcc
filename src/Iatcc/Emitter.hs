{-# LANGUAGE GeneralizedNewtypeDeriving #-}
module Iatcc.Emitter where

import Control.Monad.RWS (RWS, execRWS)
import Control.Monad.Reader (MonadReader, ask, local)
import Control.Monad.Writer (MonadWriter, tell)
import Control.Monad.State (MonadState, get, put)
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

emitToText :: Program -> Text
emitToText p = snd $ execRWS (runEmitter $ emitProgram p) [] 0

emitProgram :: Program -> Emitter ()
emitProgram (PLetRec funs expr) = do
  labels <- forM funs $ \Fun{..} -> do
    l <- uniqueLabel
    return $ l ++ "_" ++ funName

  let refs = map funName funs `zip` map FunRef labels
  local (refs ++) $ do
    forM_ (funs `zip` labels) $ \(Fun{..}, label) -> do
      emitFunctionHeader label
      emitArgBinds (-wordSize) funParams $ \nextStackIndex -> do
        emitExpr nextStackIndex True funBody
        emit "ret"
    emitProgram $ PExpr expr

emitProgram (PExpr expr) = do
  emitFunctionHeader "scheme_entry"
  emitRegisterSave "4(%esp)" "%ecx" -- args[0]: context* ctx
  emit "movl 12(%esp), %ebp"        -- args[2]: char* heap
  emit "movl 8(%esp), %esp"         -- args[1]: char* stack
  emit "call L_scheme_entry"
  emitRegisterRestore "%ecx"
  emit "ret"
  emitFunctionHeader "L_scheme_entry"
  emitExpr (-wordSize) True expr
  emit "ret"

emitRegisterSave :: Text -> Text -> Emitter ()
emitRegisterSave arg reg = do
  emit $ "movl " ++ arg ++ ", " ++ reg
  emit $ "movl %ebx, 4(" ++ reg ++ ")"
  emit $ "movl %esi, 16(" ++ reg ++ ")"
  emit $ "movl %edi, 20(" ++ reg ++ ")"
  emit $ "movl %ebp, 24(" ++ reg ++ ")"
  emit $ "movl %esp, 28(" ++ reg ++ ")"

emitRegisterRestore :: Text -> Emitter ()
emitRegisterRestore reg = do
  emit $ "movl 4(" ++ reg ++ "), %ebx"
  emit $ "movl 16(" ++ reg ++ "), %esi"
  emit $ "movl 20(" ++ reg ++ "), %edi"
  emit $ "movl 24(" ++ reg ++ "), %ebp"
  emit $ "movl 28(" ++ reg ++ "), %esp"

emitArgBinds :: StackIndex -> [Text] -> (StackIndex -> Emitter a) -> Emitter a
emitArgBinds stackIndex params body = do
  let refs = zipWith (\i param -> (param, VarRef $ tshow i ++ "(%esp)")) [-wordSize, -wordSize*2..] params
  local (refs ++) (body (stackIndex - length params * wordSize))

emitExpr :: StackIndex -> Bool -> Expr -> Emitter ()
emitExpr stackIndex tailPos = \case
  EConst c ->
    emit $ "movl $" ++ immediateRep c ++ ", %eax"
  EUnary op e -> do
    emitExpr stackIndex False e
    emitUnary op
  EIf test conseq altern -> do
    alt <- uniqueLabel
    end <- uniqueLabel
    do
      emitExpr stackIndex False test
      emit $ "cmpb $" ++ tshow constFalse ++ ", %al"
      emit $ "je " ++ alt
      emitExpr stackIndex tailPos conseq
      emit $ "jmp " ++ end
    do
      emitLabel alt
      emitExpr stackIndex tailPos altern
    emitLabel end
  EBinary BinaryAnd a b -> do
    end <- uniqueLabel
    do
      emitExpr stackIndex False a
      emit $ "cmpb $" ++ tshow constFalse ++ ", %al"
      emit $ "je " ++ end
      emitExpr stackIndex tailPos b
    emitLabel end
  EBinary BinaryOr a b -> do
    end <- uniqueLabel
    do
      emitExpr stackIndex False a
      emit $ "cmpb $" ++ tshow constFalse ++ ", %al"
      emit $ "jne " ++ end
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
      Just (VarRef ref) -> emit $ "movl " ++ ref ++ ", %eax"
      _ -> error $ "Unbound variable: " ++ show var
  EApp fun args -> do
    refs <- ask
    case lookup fun refs of
      Just (FunRef ref) -> do
        forM_ ([-wordSize, -wordSize*2..] `zip` args) $ \(i, arg) -> do
          emitExpr (stackIndex + i) False arg
          void $ emitStackSave (stackIndex + i)
        if tailPos then do
          forM_ (take (length args) [-wordSize, -wordSize*2..]) $ \i -> do
            emit $ "movl " ++ tshow (stackIndex + i) ++ "(%esp), %eax"
            emit $ "movl %eax, " ++ tshow i ++ "(%esp)"
          emit $ "jmp " ++ ref
        else do
          emit $ "addl $" ++ tshow (stackIndex + wordSize) ++ ", %esp"
          emit $ "call " ++ ref
          emit $ "subl $" ++ tshow (stackIndex + wordSize) ++ ", %esp"
      _ -> error $ "Unknown function: " ++ show fun

emitUnary :: UnaryOp -> Emitter ()
emitUnary = \case
  UnaryFxAdd1 ->
    emit $ "addl $" ++ immediateRep (CFixnum 1) ++ ", %eax"
  UnaryFxSub1 ->
    emit $ "subl $" ++ immediateRep (CFixnum 1) ++ ", %eax"
  UnaryCharToFx ->
    emit $ "shrl $" ++ tshow (charShift - fxShift) ++ ", %eax"
  UnaryFxToChar -> do
    emit $ "shll $" ++ tshow (charShift - fxShift) ++ ", %eax"
    emit $ "orl $" ++ tshow charTag ++ ", %eax"
  UnaryIsFixnum -> do
    emit $ "andb $" ++ tshow fxTagMask ++ ", %al"
    emit $ "cmpb $" ++ tshow fxTag ++ ", %al"
    emitToBool "sete"
  UnaryIsBoolean -> do
    emit $ "andb $" ++ tshow booleanTagMask ++ ", %al"
    emit $ "cmpb $" ++ tshow booleanTag ++ ", %al"
    emitToBool "sete"
  UnaryIsChar -> do
    emit $ "andb $" ++ tshow charTagMask ++ ", %al"
    emit $ "cmpb $" ++ tshow charTag ++ ", %al"
    emitToBool "sete"
  UnaryIsFxZero -> do
    emit $ "cmpl $" ++ immediateRep (CFixnum 0) ++ ", %eax"
    emitToBool "sete"
  UnaryIsNull -> do
    emit $ "cmpb $" ++ tshow constNull ++ ", %al"
    emitToBool "sete"
  UnaryIsPair -> do
    emit $ "andb $" ++ tshow pairTagMask ++ ", %al"
    emit $ "cmpb $" ++ tshow pairTag ++ ", %al"
    emitToBool "sete"
  UnaryNot -> do
    emit $ "cmpb $" ++ tshow constFalse ++ ", %al"
    emitToBool "sete"
  UnaryLogicalNot -> do
    emit $ "xorl $" ++ tshow ((-1) `xor` fxTagMask) ++ ", %eax"
  UnaryCar ->
    emit $ "movl " ++ tshow (0 - pairTag) ++ "(%eax), %eax"
  UnaryCdr ->
    emit $ "movl " ++ tshow (toInteger wordSize - pairTag) ++ "(%eax), %eax"

emitBinary :: BinaryOp -> Text -> Emitter ()
emitBinary op lhs = case op of
  BinaryAnd ->
    error "emitBinary BinaryAnd"
  BinaryOr ->
    error "emitBinary BinaryOr"
  BinarySeq ->
    error "emitBinary BinarySeq"
  BinaryAdd ->
    emit $ "addl " ++ lhs ++ ", %eax"
  BinarySub -> do
    emit $ "subl " ++ lhs ++ ", %eax"
    emit $ "neg %eax"
    emit $ "andb $" ++ tshow ((-1) `xor` fxTagMask) ++ ", %al"
  BinaryMul -> do
    emit $ "sarl $2, %eax"
    emit $ "imull " ++ lhs ++ ", %eax"
  BinaryCons -> do
    -- cdr
    emit $ "movl %eax, " ++ tshow wordSize ++ "(%ebp)"
    emit $ "movl " ++ lhs ++ ", %eax"
    -- car
    emit $ "movl %eax, (%ebp)"

    emit $ "movl %ebp, %eax"
    emit $ "orl $" ++ tshow pairTag ++ ", %eax"
    emit $ "addl $8, %ebp"
  BinaryLogicalAnd -> do
    emit $ "andl " ++ lhs ++ ", %eax"
  BinaryLogicalOr -> do
    emit $ "orl " ++ lhs ++ ", %eax"
  BinaryEqual -> do
    emit $ "cmpl " ++ lhs ++ ", %eax"
    emitToBool "sete"
  BinaryNotEqual -> do
    emit $ "cmpl " ++ lhs ++ ", %eax"
    emitToBool "setne"
  BinaryLesser -> do
    emit $ "cmpl " ++ lhs ++ ", %eax"
    emitToBool "setg"
  BinaryLesserEqual -> do
    emit $ "cmpl " ++ lhs ++ ", %eax"
    emitToBool "setge"
  BinaryGreater -> do
    emit $ "cmpl " ++ lhs ++ ", %eax"
    emitToBool "setl"
  BinaryGreaterEqual -> do
    emit $ "cmpl " ++ lhs ++ ", %eax"
    emitToBool "setle"
  BinarySetCar -> do
    emit $ "movl " ++ lhs ++ ", %edx"
    emit $ "movl %eax, " ++ tshow (0 - pairTag) ++ "(%edx)"
  BinarySetCdr -> do
    emit $ "movl " ++ lhs ++ ", %edx"
    emit $ "movl %eax, " ++ tshow (toInteger wordSize - pairTag) ++ "(%edx)"

emitFunctionHeader :: Text -> Emitter ()
emitFunctionHeader name = do
  emit ".text"
  emit $ ".globl " ++ name
  emit $ ".type " ++ name ++ ", @function"
  emitLabel name

emitToBool :: Text -> Emitter ()
emitToBool cond = do
  emit $ cond ++ " %al"
  emit $ "movzbl %al, %eax"
  emit $ "shll $" ++ tshow booleanShift ++ ", %eax"
  emit $ "orl $" ++ tshow booleanTag ++ ", %eax"

emitLabel :: Text -> Emitter ()
emitLabel = tell . (<> ":\n")

emitStackSave :: StackIndex -> Emitter (Text, StackIndex)
emitStackSave stackIndex = do
  emit $ "movl %eax, " ++ tshow stackIndex ++ "(%esp)"
  return (tshow stackIndex ++ "(%esp)", stackIndex - wordSize)

emit :: Text -> Emitter ()
emit = tell . (\s -> "    " <> s <> "\n")

uniqueLabel :: Emitter Text
uniqueLabel = do
  count <- get
  put $! count + 1
  return $ "L_" ++ tshow count

immediateRep :: Constant -> Text
immediateRep = \case
  CFixnum n ->
    if negate (2 ^ 29) <= n && n <= (2 ^ 29) - 1
      then tshow $ n `shiftL` fxShift
      else error ("Fixnum overflow: " ++ show n)
  CBoolean b ->
    tshow $ bool constFalse constTrue b
  CChar c ->
    if isAscii c
      then tshow (toInteger (ord c) `shiftL` charShift + charTag)
      else error ("Non-ascii characters are not supported: " ++ show c)
  CNull ->
    tshow constNull

wordSize :: Int
wordSize = 4

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
