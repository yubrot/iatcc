{-# LANGUAGE FlexibleInstances, TypeSynonymInstances #-}
{-# OPTIONS_GHC -fno-warn-unused-top-binds #-}

import Iatcc.AST
import Iatcc.Parser (Parser, parse, constant, expr, program)
import Iatcc.Compiler (exec)
import Test.HUnit

class TestResult a where
  (~>>) :: String -> a -> Test

instance (Eq a, Show a) => TestResult (Parser a, a) where
  (~>>) src (p, r) = src ~: parse p "" (fromString src) ~?= Right r

instance TestResult String where
  (~>>) src dest = src ~: do
    dest' <- try (exec (fromString src))
    case dest' of
      Right dest' -> assertEqual "" (fromString dest) dest'
      Left e -> assertFailure (show (e :: SomeException))

o :: String -> String
o = id

main :: IO ()
main = void $ runTestTT $ TestList [parseTests, execTests]

parseTests :: Test
parseTests = "parser" ~: TestList [parseConstantTests, parseExprTests, parseProgramTests]

parseConstantTests :: Test
parseConstantTests = "constants" ~: TestList
  [ "123" ~>> (constant, CFixnum 123)
  , "-45" ~>> (constant, CFixnum (-45))
  , "+67" ~>> (constant, CFixnum 67)
  ]

parseExprTests :: Test
parseExprTests = "expressions" ~: TestList
  [ "123" ~>> (expr, EConst (CFixnum 123))
  , "inc 123" ~>> (expr, EUnary UnaryFxAdd1 (EConst (CFixnum 123)))
  , "inc (dec +123)" ~>> (expr, EUnary UnaryFxAdd1 (EUnary UnaryFxSub1 (EConst (CFixnum 123))))
  , "1 + 2 * 3" ~>> (expr, EBinary BinaryAdd (EConst (CFixnum 1)) (EBinary BinaryMul (EConst (CFixnum 2)) (EConst (CFixnum 3))))
  , "foo" ~>> (expr, EVar "foo")
  , "if a then b else c" ~>> (expr, EIf (EVar "a") (EVar "b") (EVar "c"))
  , "let a = 123 in b" ~>> (expr, ELet "a" (EConst (CFixnum 123)) (EVar "b"))
  , "foo(bar, baz)" ~>> (expr, EApp "foo" [EVar "bar", EVar "baz"])
  ]

parseProgramTests :: Test
parseProgramTests = "programs" ~: TestList
  [ "x" ~>> (program, PExpr (EVar "x"))
  , "letrec foo() = x in y" ~>> (program, PLetRec [Fun "foo" [] (EVar "x")] (EVar "y"))
  , "letrec foo() = x and bar(baz) = y in z" ~>> (program, PLetRec [Fun "foo" [] (EVar "x"), Fun "bar" ["baz"] (EVar "y")] (EVar "z"))
  , "letrec f(x, y) = z in w" ~>> (program, PLetRec [Fun "f" ["x", "y"] (EVar "z")] (EVar "w"))
  ]

execTests :: Test
execTests = "exec" ~: TestList
  [ exec1_9_1Tests
  , exec1_8Tests
  , exec1_7Tests
  , exec1_6Tests
  , exec1_5Tests
  , exec1_4Tests
  , exec1_3Tests
  , exec1_2Tests
  , exec1_1Tests
  ]

constantTest :: String -> Test
constantTest c = c ~>> c

exec1_1Tests :: Test
exec1_1Tests = "1-1" ~: TestList (map constantTest nums)
 where
  nums = ["0", "1", "-1", "10", "-10", "2736", "-2736", "536870911", "-536870912"]

exec1_2Tests :: Test
exec1_2Tests = "1-2" ~: TestList (map constantTest (bools ++ objs ++ chars))
 where
  bools = ["true", "false"]
  objs = ["()"]
  chars = map show "\t\n\r\"#$%&'()*+,-./0123456789:;<=>?@ABCDEFGHIJKLMNOPQRSTUVWXYZ[\\]^_`abcdefghijklmnopqrstuvwxyz{|}~"

exec1_3Tests :: Test
exec1_3Tests = "1-3" ~: TestList
  [ "inc 0" ~>> o "1"
  , "inc -1" ~>> o "0"
  , "inc 1" ~>> o "2"
  , "inc -100" ~>> o "-99"
  , "inc 1000" ~>> o "1001"
  , "inc 536870910" ~>> o "536870911"
  , "inc -536870912" ~>> o "-536870911"
  , "inc (inc 0)" ~>> o "2"
  , "inc (inc (inc (inc (inc (inc 12)))))" ~>> o "18"

  , "dec 0" ~>> o "-1"
  , "dec -1" ~>> o "-2"
  , "dec 1" ~>> o "0"
  , "dec -100" ~>> o "-101"
  , "dec 1000" ~>> o "999"
  , "dec 536870911" ~>> o "536870910"
  , "dec -536870911" ~>> o "-536870912"
  , "dec (dec 0)" ~>> o "-2"
  , "dec (dec (dec (dec (dec (dec 12)))))" ~>> o "6"
  , "dec (inc 0)" ~>> o "0"

  , "fixnum->char 65" ~>> o "'A'"
  , "fixnum->char 97" ~>> o "'a'"
  , "fixnum->char 122" ~>> o "'z'"
  , "fixnum->char 90" ~>> o "'Z'"
  , "fixnum->char 48" ~>> o "'0'"
  , "fixnum->char 57" ~>> o "'9'"
  , "char->fixnum 'A'" ~>> o "65"
  , "char->fixnum 'a'" ~>> o "97"
  , "char->fixnum 'z'" ~>> o "122"
  , "char->fixnum 'Z'" ~>> o "90"
  , "char->fixnum '0'" ~>> o "48"
  , "char->fixnum '9'" ~>> o "57"
  , "char->fixnum (fixnum->char 12)" ~>> o "12"
  , "fixnum->char (char->fixnum 'x')" ~>> o "'x'"

  , "fixnum? 0" ~>> o "true"
  , "fixnum? 1" ~>> o "true"
  , "fixnum? -1" ~>> o "true"
  , "fixnum? 37287" ~>> o "true"
  , "fixnum? -23873" ~>> o "true"
  , "fixnum? 536870911" ~>> o "true"
  , "fixnum? -536870912" ~>> o "true"
  , "fixnum? true" ~>> o "false"
  , "fixnum? false" ~>> o "false"
  , "fixnum? ()" ~>> o "false"
  , "fixnum? 'Q'" ~>> o "false"
  , "fixnum? (fixnum? 12)" ~>> o "false"
  , "fixnum? (fixnum? false)" ~>> o "false"
  , "fixnum? (fixnum? 'A')" ~>> o "false"
  , "fixnum? (char->fixnum 'r')" ~>> o "true"
  , "fixnum? (fixnum->char 12)" ~>> o "false"

  , "boolean? true" ~>> o "true"
  , "boolean? false" ~>> o "true"
  , "boolean? 0" ~>> o "false"
  , "boolean? 1" ~>> o "false"
  , "boolean? -1" ~>> o "false"
  , "boolean? ()" ~>> o "false"
  , "boolean? 'a'" ~>> o "false"
  , "boolean? (boolean? 0)" ~>> o "true"
  , "boolean? (fixnum? (boolean? 0))" ~>> o "true"

  , "char? 'a'" ~>> o "true"
  , "char? 'Z'" ~>> o "true"
  , "char? '\n'" ~>> o "true"
  , "char? true" ~>> o "false"
  , "char? false" ~>> o "false"
  , "char? ()" ~>> o "false"
  , "char? (char? true)" ~>> o "false"
  , "char? 0" ~>> o "false"
  , "char? 23870" ~>> o "false"
  , "char? -23789" ~>> o "false"

  , "zero? 0" ~>> o "true"
  , "zero? 1" ~>> o "false"
  , "zero? -1" ~>> o "false"
  , "zero? 64" ~>> o "false"
  , "zero? 960" ~>> o "false"

  , "null? ()" ~>> o "true"
  , "null? false" ~>> o "false"
  , "null? true" ~>> o "false"
  , "null? (null? ())" ~>> o "false"
  , "null? 'a'" ~>> o "false"
  , "null? 0" ~>> o "false"
  , "null? -10" ~>> o "false"
  , "null? 10" ~>> o "false"

  , "! true" ~>> o "false"
  , "! false" ~>> o "true"
  , "! 15" ~>> o "false"
  , "! ()" ~>> o "false"
  , "! 'A'" ~>> o "false"
  , "! (! true)" ~>> o "true"
  , "! (! false)" ~>> o "false"
  , "! (! 15)" ~>> o "true"
  , "! (fixnum? 15)" ~>> o "false"
  , "! (fixnum? false)" ~>> o "true"

  , "~ 0" ~>> o "-1"
  , "~ -1" ~>> o "0"
  , "~ 1" ~>> o "-2"
  , "~ -2" ~>> o "1"
  , "~ 536870911" ~>> o "-536870912"
  , "~ -536870912" ~>> o "536870911"
  , "~ (~ 237463)" ~>> o "237463"
  ]

exec1_4Tests :: Test
exec1_4Tests = "1-4" ~: TestList
  [ "if true then 12 else 13" ~>> o "12"
  , "if false then 12 else 13" ~>> o "13"
  , "if 0 then 12 else 13" ~>> o "12"
  , "if () then 43 else ()" ~>> o "43"
  , "if true then if 12 then 13 else 4 else 17" ~>> o "13"
  , "if false then 12 else if false then 13 else 4" ~>> o "4"
  , "if 'X' then if 1 then 2 else 3 else if 4 then 5 else 6" ~>> o "2"
  , "if !(boolean? true) then 15 else boolean? false" ~>> o "true"
  , "if if char? 'a' then boolean? 'b' else fixnum? 'c' then 119 else -23" ~>> o "-23"
  , "if if if !1 then !2 else !3 then 4 else 5 then 6 else 7" ~>> o "6"
  , "if !(if if !1 then !2 else !3 then 4 else 5) then 6 else 7" ~>> o "7"
  , "!(if !(if if !1 then !2 else !3 then 4 else 5) then 6 else 7)" ~>> o "false"
  , "if char? 12 then 13 else 14" ~>> o "14"
  , "if char? 'a' then 13 else 14" ~>> o "13"
  , "inc (if dec 1 then dec 13 else 14)" ~>> o "13"

  , "true && true" ~>> o "true"
  , "true && false" ~>> o "false"
  , "false && true" ~>> o "false"
  , "false && false" ~>> o "false"
  , "false && 34" ~>> o "false"
  , "12 && 34" ~>> o "34"
  , "12 && 34 && 56" ~>> o "56"
  , "12 && false && 56" ~>> o "false"
  , "!false && !false && !false" ~>> o "true"

  , "true || true" ~>> o "true"
  , "true || false" ~>> o "true"
  , "false || true" ~>> o "true"
  , "false || false" ~>> o "false"
  , "false || 34" ~>> o "34"
  , "12 || 34" ~>> o "12"
  , "false || 34 || 56" ~>> o "34"
  , "false || 34 || false" ~>> o "34"
  , "!true || !true || !true" ~>> o "false"
  , "1 && 2 || 3 && 4" ~>> o "2"
  , "1 && false || 3 && 4" ~>> o "4"
  , "1 && false || false && 4" ~>> o "false"
  , "(1 || false) && (3 || false)" ~>> o "3"
  , "(1 || false) && (3 || 4)" ~>> o "3"
  ]

exec1_5Tests :: Test
exec1_5Tests = "1-5" ~: TestList
  [ "1 + 2" ~>> o "3"
  , "1 + 2" ~>> o "3"
  , "1 + -2" ~>> o "-1"
  , "-1 + 2" ~>> o "1"
  , "-1 + -2" ~>> o "-3"
  , "536870911 + -1" ~>> o "536870910"
  , "536870910 + 1" ~>> o "536870911"
  , "-536870912 + 1" ~>> o "-536870911"
  , "-536870911 + -1" ~>> o "-536870912"
  , "536870911 + -536870912" ~>> o "-1"
  , "1 + (2 + 3)" ~>> o "6"
  , "1 + (2 + -3)" ~>> o "0"
  , "1 + (-2 + 3)" ~>> o "2"
  , "1 + (-2 + -3)" ~>> o "-4"
  , "-1 + (2 + 3)" ~>> o "4"
  , "-1 + (2 + -3)" ~>> o "-2"
  , "-1 + (-2 + 3)" ~>> o "0"
  , "-1 + (-2 + -3)" ~>> o "-6"
  , "(1 + 2) + 3" ~>> o "6"
  , "(1 + 2) + -3" ~>> o "0"
  , "(1 + -2) + 3" ~>> o "2"
  , "(1 + -2) + -3" ~>> o "-4"
  , "(-1 + 2) + 3" ~>> o "4"
  , "(-1 + 2) + -3" ~>> o "-2"
  , "(-1 + -2) + 3" ~>> o "0"
  , "(-1 + -2) + -3" ~>> o "-6"
  , "(((((((1 + 2) + 3) + 4) + 5) + 6) + 7) + 8) + 9" ~>> o "45"
  , "1 + (2 + (3 + (4 + (5 + (6 + (7 + (8 + 9)))))))" ~>> o "45"

  , "1 - 2" ~>> o "-1"
  , "1 - -2" ~>> o "3"
  , "-1 - 2" ~>> o "-3"
  , "-1 - -2" ~>> o "1"
  , "536870910 - -1" ~>> o "536870911"
  , "536870911 - 1" ~>> o "536870910"
  , "-536870911 - 1" ~>> o "-536870912"
  , "-536870912 - -1" ~>> o "-536870911"
  , "1 - 536870911" ~>> o "-536870910"
  , "-1 - 536870911" ~>> o "-536870912"
  , "1 - -536870910" ~>> o "536870911"
  , "-1 - -536870912" ~>> o "536870911"
  , "536870911 - 536870911" ~>> o "0"
  , " 536870911 - -536870912" ~>> o "-1"
  , "-536870911 - -536870912" ~>> o "1"
  , "1 - (2 - 3)" ~>> o "2"
  , "1 - (2 - -3)" ~>> o "-4"
  , "1 - (-2 - 3)" ~>> o "6"
  , "1 - (-2 - -3)" ~>> o "0"
  , "-1 - (2 - 3)" ~>> o "0"
  , "-1 - (2 - -3)" ~>> o "-6"
  , "-1 - (-2 - 3)" ~>> o "4"
  , "-1 - (-2 - -3)" ~>> o "-2"
  , "0 - (-2 - -3)" ~>> o "-1"
  , "(1 - 2) - 3" ~>> o "-4"
  , "(1 - 2) - -3" ~>> o "2"
  , "(1 - -2) - 3" ~>> o "0"
  , "(1 - -2) - -3" ~>> o "6"
  , "(-1 - 2) - 3" ~>> o "-6"
  , "(-1 - 2) - -3" ~>> o "0"
  , "(-1 - -2) - 3" ~>> o "-2"
  , "(-1 - -2) - -3" ~>> o "4"
  , "(((((((1 - 2) - 3) - 4) - 5) - 6) - 7) - 8) - 9" ~>> o "-43"
  , "1 - (2 - (3 - (4 - (5 - (6 - (7 - (8 - 9)))))))" ~>> o "5"

  , "2 * 3" ~>> o "6"
  , "2 * -3" ~>> o "-6"
  , "-2 * 3" ~>> o "-6"
  , "-2 * -3" ~>> o "6"
  , "536870911 * 1" ~>> o "536870911"
  , "536870911 * -1" ~>> o "-536870911"
  , "-536870912 * 1" ~>> o "-536870912"
  , "-536870911 * -1" ~>> o "536870911"
  , "2 * (3 * 4)" ~>> o "24"
  , "(2 * 3) * 4" ~>> o "24"
  , "((((2 * 3) * 4) * 5) * 6) * 7" ~>> o "5040"
  , "2 * (3 * (4 * (5 * (6 * 7))))" ~>> o "5040"

  , "3 | 16" ~>> o "19"
  , "3 | 5" ~>> o "7"
  , "3 | 7" ~>> o "7"
  , "~(~7 | 1)" ~>> o "6"
  , "~(1 | ~7)" ~>> o "6"
  , "3 & 7" ~>> o "3"
  , "3 & 5" ~>> o "1"
  , "2346 & ~2346" ~>> o "0"
  , "~2346 & 2346" ~>> o "0"
  , "2376 & 2376" ~>> o "2376"

  , "12 == 13" ~>> o "false"
  , "12 == 12" ~>> o "true"
  , "16 == 13 + 3" ~>> o "true"
  , "16 == 13 + 13" ~>> o "false"
  , "13 + 3 == 16" ~>> o "true"
  , "13 + 13 == 16" ~>> o "false"

  , "12 < 13" ~>> o "true"
  , "12 < 12" ~>> o "false"
  , "13 < 12" ~>> o "false"
  , "16 < 13 + 1" ~>> o "false"
  , "16 < 13 + 3" ~>> o "false"
  , "16 < 13 + 13" ~>> o "true"
  , "13 + 1 < 16" ~>> o "true"
  , "13 + 3 < 16" ~>> o "false"
  , "13 + 13 < 16" ~>> o "false"

  , "12 <= 13" ~>> o "true"
  , "12 <= 12" ~>> o "true"
  , "13 <= 12" ~>> o "false"
  , "16 <= 13 + 1" ~>> o "false"
  , "16 <= 13 + 3" ~>> o "true"
  , "16 <= 13 + 13" ~>> o "true"
  , "13 + 1 <= 16" ~>> o "true"
  , "13 + 3 <= 16" ~>> o "true"
  , "13 + 13 <= 16" ~>> o "false"

  , "12 > 13" ~>> o "false"
  , "12 > 12" ~>> o "false"
  , "13 > 12" ~>> o "true"
  , "16 > 13 + 1" ~>> o "true"
  , "16 > 13 + 3" ~>> o "false"
  , "16 > 13 + 13" ~>> o "false"
  , "13 + 1 > 16" ~>> o "false"
  , "13 + 3 > 16" ~>> o "false"
  , "13 + 13 > 16" ~>> o "true"

  , "12 >= 13" ~>> o "false"
  , "12 >= 12" ~>> o "true"
  , "13 >= 12" ~>> o "true"
  , "16 >= 13 + 1" ~>> o "true"
  , "16 >= 13 + 3" ~>> o "true"
  , "16 >= 13 + 13" ~>> o "false"
  , "13 + 1 >= 16" ~>> o "false"
  , "13 + 3 >= 16" ~>> o "true"
  , "13 + 13 >= 16" ~>> o "true"

  , "if 12 == 13 then 12 else 13" ~>> o "13"
  , "if 12 == 12 then 13 else 14" ~>> o "13"
  , "if 12 <  13 then 12 else 13" ~>> o "12"
  , "if 12 <  12 then 13 else 14" ~>> o "14"
  , "if 13 <  12 then 13 else 14" ~>> o "14"
  , "if 12 <= 13 then 12 else 13" ~>> o "12"
  , "if 12 <= 12 then 12 else 13" ~>> o "12"
  , "if 13 <= 12 then 13 else 14" ~>> o "14"
  , "if 12 >  13 then 12 else 13" ~>> o "13"
  , "if 12 >  12 then 12 else 13" ~>> o "13"
  , "if 13 >  12 then 13 else 14" ~>> o "13"
  , "if 12 >= 13 then 12 else 13" ~>> o "13"
  , "if 12 >= 12 then 12 else 13" ~>> o "12"
  , "if 13 >= 12 then 13 else 14" ~>> o "13"

  , "~-7" ~>> o "6"
  , "~(~7 | 1)" ~>> o "6"
  , "~(~7 | ~2)" ~>> o "2"
  , "~(~12) & ~(~12)" ~>> o "12"
  , "(1 + 2) + (3 + 4)" ~>> o "10"
  , "(1 + 2) + (3 + -4)" ~>> o "2"
  , "(1 + 2) + (-3 + 4)" ~>> o "4"
  , "(1 + 2) + (-3 + -4)" ~>> o "-4"
  , "(1 + -2) + (3 + 4)" ~>> o "6"
  , "(1 + -2) + (3 + -4)" ~>> o "-2"
  , "(1 + -2) + (-3 + 4)" ~>> o "0"
  , "(1 + -2) + (-3 + -4)" ~>> o "-8"
  , "(-1 + 2) + (3 + 4)" ~>> o "8"
  , "(-1 + 2) + (3 + -4)" ~>> o "0"
  , "(-1 + 2) + (-3 + 4)" ~>> o "2"
  , "(-1 + 2) + (-3 + -4)" ~>> o "-6"
  , "(-1 + -2) + (3 + 4)" ~>> o "4"
  , "(-1 + -2) + (3 + -4)" ~>> o "-4"
  , "(-1 + -2) + (-3 + 4)" ~>> o "-2"
  , "(-1 + -2) + (-3 + -4)" ~>> o "-10"
  , "(((((((1 + 2) + 3) + 4) + 5) + 6) + 7) + 8) + 9" ~>> o "45"
  , "1 + (2 + (3 + (4 + (5 + (6 + (7 + (8 + 9)))))))" ~>> o "45"
  , "(((1 + 2) + (3 + 4)) + ((5 + 6) + (7 + 8))) + (((9 + 10) + (11 + 12)) + ((13 + 14) + (15 + 16)))" ~>> o "136"
  , "( 1 -  2) - ( 3 -  4)" ~>> o "0"
  , "( 1 -  2) - ( 3 - -4)" ~>> o "-8"
  , "( 1 -  2) - (-3 -  4)" ~>> o "6"
  , "( 1 -  2) - (-3 - -4)" ~>> o "-2"
  , "( 1 - -2) - ( 3 -  4)" ~>> o "4"
  , "( 1 - -2) - ( 3 - -4)" ~>> o "-4"
  , "( 1 - -2) - (-3 -  4)" ~>> o "10"
  , "( 1 - -2) - (-3 - -4)" ~>> o "2"
  , "(-1 -  2) - ( 3 -  4)" ~>> o "-2"
  , "(-1 -  2) - ( 3 - -4)" ~>> o "-10"
  , "(-1 -  2) - (-3 -  4)" ~>> o "4"
  , "(-1 -  2) - (-3 - -4)" ~>> o "-4"
  , "(-1 - -2) - ( 3 -  4)" ~>> o "2"
  , "(-1 - -2) - ( 3 - -4)" ~>> o "-6"
  , "(-1 - -2) - (-3 -  4)" ~>> o "8"
  , "(-1 - -2) - (-3 - -4)" ~>> o "0"
  , "(((((((1 - 2) - 3) - 4) - 5) - 6) - 7) - 8) - 9" ~>> o "-43"
  , "1 - (2 - (3 - (4 - (5 - (6 - (7 - (8 - 9)))))))" ~>> o "5"
  , "(((1 - 2) - (3 - 4)) - ((5 - 6) - (7 - 8))) - (((9 - 10) - (11 - 12)) - ((13 - 14) - (15 - 16)))" ~>> o "0"
  , "(((2 * 3) * (4 * 5)) * ((6 * 7) * (8 * 9))) * (((2 * 3) * (2 * 3)) * ((2 * 3) * (2 * 3)))" ~>> o "470292480"
  , "~(~7 | 1)" ~>> o "6"
  , "~(~7 | ~2)" ~>> o "2"
  , "~(~12) & ~(~12)" ~>> o "12"
  , "13 + 3 == 10 + 6" ~>> o "true"
  , "13 + 0 == 10 + 6" ~>> o "false"
  , "12 + 1 == -12 + -1" ~>> o "false"
  , "10  +  6 <  13 +  1" ~>> o "false"
  , "10  +  6 <  13 +  3" ~>> o "false"
  , "10  +  6 <  13 + 31" ~>> o "true"
  , "12  +  1 < -12 + -1" ~>> o "false"
  , "-12 + -1 <  12 +  1" ~>> o "true"
  , " 10 +  6 <=  13 + 1" ~>> o "false"
  , " 10 +  6 <=  13 + 3" ~>> o "true"
  , " 10 +  6 <=  13 + 31" ~>> o "true"
  , " 12 +  1 <= -12 + -1" ~>> o "false"
  , "-12 + -1 <=  12 + 1" ~>> o "true"
  , "10 + 6 > 13 + 1" ~>> o "true"
  , "10 + 6 > 13 + 3" ~>> o "false"
  , "10 + 6 > 13 + 31" ~>> o "false"
  , "12 + 1 > -12 + -1" ~>> o "true"
  , "-12 + -1 > 12 + 1" ~>> o "false"
  , "10 + 6 >= 13 + 1" ~>> o "true"
  , "10 + 6 >= 13 + 3" ~>> o "true"
  , "10 + 6 >= 13 + 31" ~>> o "false"
  , "12 + 1 >= -12 + -1" ~>> o "true"
  , "-12 + -1 >= 12 + 1" ~>> o "false"
  ]

exec1_6Tests :: Test
exec1_6Tests = "1-6" ~: TestList
  [ "let x = 5 in x" ~>> o "5"
  , "let x = 1 + 2 in x" ~>> o "3"
  , "let x = 1 + 2 in let y = 3 + 4 in x + y" ~>> o "10"
  , "let x = 1 + 2 in let y = 3 + 4 in y - x" ~>> o "4"
  , "let x = let y = 1 + 2 in y * y in x + x" ~>> o "18"
  , "let x = 1 + 2 in let x = 3 + 4 in x" ~>> o "7"
  , "let x = 1 + 2 in let x = x + 4 in x" ~>> o "7"
  , "let t = let t = let t = let t = 1 + 2 in t in t in t in t" ~>> o "3"
  , "let x = 12 in let x = x + x in let x = x + x in let x = x + x in x + x" ~>> o "192"
  ]

exec1_7Tests :: Test
exec1_7Tests = "1-7" ~: TestList
  [ "letrec f() = 5 in 7" ~>> o "7"
  , "letrec f() = 5 in let x = 12 in x" ~>> o "12"
  , "letrec f() = 5 in f()" ~>> o "5"
  , "letrec f() = 5 in f() + 6" ~>> o "11"
  , "letrec f() = 5 in 6 + f()" ~>> o "11"
  , "letrec f() = 5 in 20 - f()" ~>> o "15"
  , "letrec f() = 5 in f() + f()" ~>> o "10"
  , "letrec f() = 5 + 7 and g() = 13 in f() + g()" ~>> o "25"
  , "letrec f(x) = x + 12 in f(13)" ~>> o "25"
  , "letrec f(x) = x + 12 in f(f(10))" ~>> o "34"
  , "letrec f(x) = x + 12 in f(f(f(0)))" ~>> o "36"
  , "letrec f(x, y) = x + y and g(x) = x + 12 in f(16, f(g(0), 1 + g(0)))" ~>> o "41"
  , "letrec f(x) = g(x, x) and g(x, y) = x + y in f(12)" ~>> o "24"
  , "letrec f(x) = if zero? x then 1 else x * f(dec x) in f(5)" ~>> o "120"
  , "letrec f(x, acc) = if zero? x then acc else f(dec x, acc * x) in f(5, 1)" ~>> o "120"
  , "letrec f(x) = if zero? x then 0 else 1 + f(dec x) in f(500)" ~>> o "500"
  ]

exec1_8Tests :: Test
exec1_8Tests = "1-8" ~: TestList
  [ "letrec even(x) = if zero? x then true else odd(dec x) and odd(x) = if zero? x then false else even(dec x) in even(25)" ~>> o "false"
  , "letrec countdown(n) = if zero? n then n else countdown(dec n) in countdown(50005000)" ~>> o "0"
  , "letrec sum(n, acc) = if zero? n then acc else sum(dec n, n + acc) in sum(10000, 0)" ~>> o "50005000"
  , "letrec even(x) = if zero? x then true else odd(dec x) and odd(x) = if zero? x then false else even(dec x) in even(5000000)" ~>> o "true"
  ]

exec1_9_1Tests :: Test
exec1_9_1Tests = "1-9-1" ~: TestList
  [ "1 . 2" ~>> o "(1 . 2)"
  , "pair? 12" ~>> o "false"
  , "pair? true" ~>> o "false"
  , "pair? false" ~>> o "false"
  , "pair? ()" ~>> o "false"
  , "pair? (1 . 2)" ~>> o "true"
  , "fixnum? (12 . 43)" ~>> o "false"
  , "boolean? (12 . 43)" ~>> o "false"
  , "null? (12 . 43)" ~>> o "false"
  , "! (12 . 43)" ~>> o "false"
  , "if 12 . 43 then 32 else 43" ~>> o "32"
  , "car (1 . 23)" ~>> o "1"
  , "cdr (43 . 123)" ~>> o "123"
  , "pair? ((12 . 3) . false)" ~>> o "true"
  , "pair? ((12 . 3) . (true . false))" ~>> o "true"
  , "car (car ((12 . 3) . (true . false)))" ~>> o "12"
  , "cdr (car ((12 . 3) . (true . false)))" ~>> o "3"
  , "car (cdr ((12 . 3) . (true . false)))" ~>> o "true"
  , "cdr (cdr ((12 . 3) . (true . false)))" ~>> o "false"
  , "pair? ((1 * 1) . 1)" ~>> o "true"
  , "let x = let y = 1 + 2 in y * y in x . (x + x)" ~>> o "(9 . 18)"
  , "let t0 = 1 . 2 and t1 = 3 . 4 in let a0 = car t0 and a1 = car t1 and d0 = cdr t0 and d1 = cdr t1 in let t0 = a0 . d1 and t1 = a1 . d0 in t0 . t1" ~>> o "((1 . 4) . (3 . 2))"
  , "let t = 1 . 2 in let t = t in let t = t in let t = t in t" ~>> o "(1 . 2)"
  , "let t = let t = let t = let t = 1 . 2 in t in t in t in t" ~>> o "(1 . 2)"
  , "let x = () in let x = x . x in x . x" ~>> o "((() . ()) . (() . ()))"
  , "13; 122" ~>> o "122"
  , "123; 2343; true" ~>> o "true"
  , "let x = 1 . 2 in x car= 3; x" ~>> o "(3 . 2)"
  , "let x = 1 . 2 in x cdr= 3; x" ~>> o "(1 . 3)"
  , "let x = 12 . 13 and y = 14 . 15 in x cdr= y; x . y" ~>> o "((12 . (14 . 15)) . (14 . 15))"
  , "let x = 1 . 2 in x car= true; x cdr= false; x" ~>> o "(true . false)"
  ]
