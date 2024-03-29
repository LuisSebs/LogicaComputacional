{-# OPTIONS_GHC -w #-}
module MinMl.Parser where
import MinMl.Lexer as LexMl
import qualified Data.Array as Happy_Data_Array
import qualified Data.Bits as Bits
import Control.Applicative(Applicative(..))
import Control.Monad (ap)

-- parser produced by Happy Version 1.19.11

data HappyAbsSyn t4
	= HappyTerminal (Token)
	| HappyErrorToken Int
	| HappyAbsSyn4 t4

happyExpList :: Happy_Data_Array.Array Int Int
happyExpList = Happy_Data_Array.listArray (0,126) ([33264,8416,0,0,0,64480,0,0,0,0,0,16632,61552,57473,992,49601,33287,3971,1796,65503,48655,8191,65404,63551,32766,32256,57375,49411,1985,33666,1039,7943,3592,4158,31772,14368,16632,61552,57473,992,49601,33287,57347,248,61888,32769,995,50944,7,3982,23552,31,16120,0,0,128,0,1,0,31744,16383,65272,127,28,14336,0,112,57344,0,0
	])

{-# NOINLINE happyExpListPerState #-}
happyExpListPerState st =
    token_strs_expected
  where token_strs = ["error","%dummy","%start_parser","MinMl","boolean","var","digit","fun","'('","')'","'-'","'+'","'*'","minmlor","minmland","minmllet","minmleq","minmlgt","minmllt","minmlgte","minmllte","minmlif","minmlletrec","minmlfunc","%eof"]
        bit_start = st * 25
        bit_end = (st + 1) * 25
        read_bit = readArrayBit happyExpList
        bits = map read_bit [bit_start..bit_end - 1]
        bits_indexed = zip bits [0..24]
        token_strs_expected = concatMap f bits_indexed
        f (False, _) = []
        f (True, nr) = [token_strs !! nr]

action_0 (5) = happyShift action_2
action_0 (6) = happyShift action_4
action_0 (7) = happyShift action_5
action_0 (8) = happyShift action_6
action_0 (9) = happyShift action_7
action_0 (16) = happyShift action_8
action_0 (22) = happyShift action_9
action_0 (23) = happyShift action_10
action_0 (24) = happyShift action_11
action_0 (4) = happyGoto action_3
action_0 _ = happyFail (happyExpListPerState 0)

action_1 (5) = happyShift action_2
action_1 _ = happyFail (happyExpListPerState 1)

action_2 _ = happyReduce_1

action_3 (11) = happyShift action_17
action_3 (12) = happyShift action_18
action_3 (13) = happyShift action_19
action_3 (14) = happyShift action_20
action_3 (15) = happyShift action_21
action_3 (17) = happyShift action_22
action_3 (18) = happyShift action_23
action_3 (19) = happyShift action_24
action_3 (20) = happyShift action_25
action_3 (21) = happyShift action_26
action_3 (25) = happyAccept
action_3 _ = happyFail (happyExpListPerState 3)

action_4 _ = happyReduce_2

action_5 _ = happyReduce_3

action_6 _ = happyReduce_4

action_7 (5) = happyShift action_2
action_7 (6) = happyShift action_4
action_7 (7) = happyShift action_5
action_7 (8) = happyShift action_6
action_7 (9) = happyShift action_7
action_7 (16) = happyShift action_8
action_7 (22) = happyShift action_9
action_7 (23) = happyShift action_10
action_7 (24) = happyShift action_11
action_7 (4) = happyGoto action_16
action_7 _ = happyFail (happyExpListPerState 7)

action_8 (5) = happyShift action_2
action_8 (6) = happyShift action_4
action_8 (7) = happyShift action_5
action_8 (8) = happyShift action_6
action_8 (9) = happyShift action_7
action_8 (16) = happyShift action_8
action_8 (22) = happyShift action_9
action_8 (23) = happyShift action_10
action_8 (24) = happyShift action_11
action_8 (4) = happyGoto action_15
action_8 _ = happyFail (happyExpListPerState 8)

action_9 (5) = happyShift action_2
action_9 (6) = happyShift action_4
action_9 (7) = happyShift action_5
action_9 (8) = happyShift action_6
action_9 (9) = happyShift action_7
action_9 (16) = happyShift action_8
action_9 (22) = happyShift action_9
action_9 (23) = happyShift action_10
action_9 (24) = happyShift action_11
action_9 (4) = happyGoto action_14
action_9 _ = happyFail (happyExpListPerState 9)

action_10 (5) = happyShift action_2
action_10 (6) = happyShift action_4
action_10 (7) = happyShift action_5
action_10 (8) = happyShift action_6
action_10 (9) = happyShift action_7
action_10 (16) = happyShift action_8
action_10 (22) = happyShift action_9
action_10 (23) = happyShift action_10
action_10 (24) = happyShift action_11
action_10 (4) = happyGoto action_13
action_10 _ = happyFail (happyExpListPerState 10)

action_11 (5) = happyShift action_2
action_11 (6) = happyShift action_4
action_11 (7) = happyShift action_5
action_11 (8) = happyShift action_6
action_11 (9) = happyShift action_7
action_11 (16) = happyShift action_8
action_11 (22) = happyShift action_9
action_11 (23) = happyShift action_10
action_11 (24) = happyShift action_11
action_11 (4) = happyGoto action_12
action_11 _ = happyFail (happyExpListPerState 11)

action_12 (5) = happyShift action_2
action_12 (6) = happyShift action_4
action_12 (7) = happyShift action_5
action_12 (8) = happyShift action_6
action_12 (9) = happyShift action_7
action_12 (11) = happyShift action_17
action_12 (12) = happyShift action_18
action_12 (13) = happyShift action_19
action_12 (14) = happyShift action_20
action_12 (15) = happyShift action_21
action_12 (16) = happyShift action_8
action_12 (17) = happyShift action_22
action_12 (18) = happyShift action_23
action_12 (19) = happyShift action_24
action_12 (20) = happyShift action_25
action_12 (21) = happyShift action_26
action_12 (22) = happyShift action_9
action_12 (23) = happyShift action_10
action_12 (24) = happyShift action_11
action_12 (4) = happyGoto action_41
action_12 _ = happyFail (happyExpListPerState 12)

action_13 (5) = happyShift action_2
action_13 (6) = happyShift action_4
action_13 (7) = happyShift action_5
action_13 (8) = happyShift action_6
action_13 (9) = happyShift action_7
action_13 (11) = happyShift action_17
action_13 (12) = happyShift action_18
action_13 (13) = happyShift action_19
action_13 (14) = happyShift action_20
action_13 (15) = happyShift action_21
action_13 (16) = happyShift action_8
action_13 (17) = happyShift action_22
action_13 (18) = happyShift action_23
action_13 (19) = happyShift action_24
action_13 (20) = happyShift action_25
action_13 (21) = happyShift action_26
action_13 (22) = happyShift action_9
action_13 (23) = happyShift action_10
action_13 (24) = happyShift action_11
action_13 (4) = happyGoto action_40
action_13 _ = happyFail (happyExpListPerState 13)

action_14 (5) = happyShift action_2
action_14 (6) = happyShift action_4
action_14 (7) = happyShift action_5
action_14 (8) = happyShift action_6
action_14 (9) = happyShift action_7
action_14 (11) = happyShift action_17
action_14 (12) = happyShift action_18
action_14 (13) = happyShift action_19
action_14 (14) = happyShift action_20
action_14 (15) = happyShift action_21
action_14 (16) = happyShift action_8
action_14 (17) = happyShift action_22
action_14 (18) = happyShift action_23
action_14 (19) = happyShift action_24
action_14 (20) = happyShift action_25
action_14 (21) = happyShift action_26
action_14 (22) = happyShift action_9
action_14 (23) = happyShift action_10
action_14 (24) = happyShift action_11
action_14 (4) = happyGoto action_39
action_14 _ = happyFail (happyExpListPerState 14)

action_15 (5) = happyShift action_2
action_15 (6) = happyShift action_4
action_15 (7) = happyShift action_5
action_15 (8) = happyShift action_6
action_15 (9) = happyShift action_7
action_15 (11) = happyShift action_17
action_15 (12) = happyShift action_18
action_15 (13) = happyShift action_19
action_15 (14) = happyShift action_20
action_15 (15) = happyShift action_21
action_15 (16) = happyShift action_8
action_15 (17) = happyShift action_22
action_15 (18) = happyShift action_23
action_15 (19) = happyShift action_24
action_15 (20) = happyShift action_25
action_15 (21) = happyShift action_26
action_15 (22) = happyShift action_9
action_15 (23) = happyShift action_10
action_15 (24) = happyShift action_11
action_15 (4) = happyGoto action_38
action_15 _ = happyFail (happyExpListPerState 15)

action_16 (10) = happyShift action_37
action_16 (11) = happyShift action_17
action_16 (12) = happyShift action_18
action_16 (13) = happyShift action_19
action_16 (14) = happyShift action_20
action_16 (15) = happyShift action_21
action_16 (17) = happyShift action_22
action_16 (18) = happyShift action_23
action_16 (19) = happyShift action_24
action_16 (20) = happyShift action_25
action_16 (21) = happyShift action_26
action_16 _ = happyFail (happyExpListPerState 16)

action_17 (5) = happyShift action_2
action_17 (6) = happyShift action_4
action_17 (7) = happyShift action_5
action_17 (8) = happyShift action_6
action_17 (9) = happyShift action_7
action_17 (16) = happyShift action_8
action_17 (22) = happyShift action_9
action_17 (23) = happyShift action_10
action_17 (24) = happyShift action_11
action_17 (4) = happyGoto action_36
action_17 _ = happyFail (happyExpListPerState 17)

action_18 (5) = happyShift action_2
action_18 (6) = happyShift action_4
action_18 (7) = happyShift action_5
action_18 (8) = happyShift action_6
action_18 (9) = happyShift action_7
action_18 (16) = happyShift action_8
action_18 (22) = happyShift action_9
action_18 (23) = happyShift action_10
action_18 (24) = happyShift action_11
action_18 (4) = happyGoto action_35
action_18 _ = happyFail (happyExpListPerState 18)

action_19 (5) = happyShift action_2
action_19 (6) = happyShift action_4
action_19 (7) = happyShift action_5
action_19 (8) = happyShift action_6
action_19 (9) = happyShift action_7
action_19 (16) = happyShift action_8
action_19 (22) = happyShift action_9
action_19 (23) = happyShift action_10
action_19 (24) = happyShift action_11
action_19 (4) = happyGoto action_34
action_19 _ = happyFail (happyExpListPerState 19)

action_20 (5) = happyShift action_2
action_20 (6) = happyShift action_4
action_20 (7) = happyShift action_5
action_20 (8) = happyShift action_6
action_20 (9) = happyShift action_7
action_20 (16) = happyShift action_8
action_20 (22) = happyShift action_9
action_20 (23) = happyShift action_10
action_20 (24) = happyShift action_11
action_20 (4) = happyGoto action_33
action_20 _ = happyFail (happyExpListPerState 20)

action_21 (5) = happyShift action_2
action_21 (6) = happyShift action_4
action_21 (7) = happyShift action_5
action_21 (8) = happyShift action_6
action_21 (9) = happyShift action_7
action_21 (16) = happyShift action_8
action_21 (22) = happyShift action_9
action_21 (23) = happyShift action_10
action_21 (24) = happyShift action_11
action_21 (4) = happyGoto action_32
action_21 _ = happyFail (happyExpListPerState 21)

action_22 (5) = happyShift action_2
action_22 (6) = happyShift action_4
action_22 (7) = happyShift action_5
action_22 (8) = happyShift action_6
action_22 (9) = happyShift action_7
action_22 (16) = happyShift action_8
action_22 (22) = happyShift action_9
action_22 (23) = happyShift action_10
action_22 (24) = happyShift action_11
action_22 (4) = happyGoto action_31
action_22 _ = happyFail (happyExpListPerState 22)

action_23 (5) = happyShift action_2
action_23 (6) = happyShift action_4
action_23 (7) = happyShift action_5
action_23 (8) = happyShift action_6
action_23 (9) = happyShift action_7
action_23 (16) = happyShift action_8
action_23 (22) = happyShift action_9
action_23 (23) = happyShift action_10
action_23 (24) = happyShift action_11
action_23 (4) = happyGoto action_30
action_23 _ = happyFail (happyExpListPerState 23)

action_24 (5) = happyShift action_2
action_24 (6) = happyShift action_4
action_24 (7) = happyShift action_5
action_24 (8) = happyShift action_6
action_24 (9) = happyShift action_7
action_24 (16) = happyShift action_8
action_24 (22) = happyShift action_9
action_24 (23) = happyShift action_10
action_24 (24) = happyShift action_11
action_24 (4) = happyGoto action_29
action_24 _ = happyFail (happyExpListPerState 24)

action_25 (5) = happyShift action_2
action_25 (6) = happyShift action_4
action_25 (7) = happyShift action_5
action_25 (8) = happyShift action_6
action_25 (9) = happyShift action_7
action_25 (16) = happyShift action_8
action_25 (22) = happyShift action_9
action_25 (23) = happyShift action_10
action_25 (24) = happyShift action_11
action_25 (4) = happyGoto action_28
action_25 _ = happyFail (happyExpListPerState 25)

action_26 (5) = happyShift action_2
action_26 (6) = happyShift action_4
action_26 (7) = happyShift action_5
action_26 (8) = happyShift action_6
action_26 (9) = happyShift action_7
action_26 (16) = happyShift action_8
action_26 (22) = happyShift action_9
action_26 (23) = happyShift action_10
action_26 (24) = happyShift action_11
action_26 (4) = happyGoto action_27
action_26 _ = happyFail (happyExpListPerState 26)

action_27 (11) = happyShift action_17
action_27 (12) = happyShift action_18
action_27 (13) = happyShift action_19
action_27 (17) = happyShift action_22
action_27 (18) = happyShift action_23
action_27 (19) = happyShift action_24
action_27 (20) = happyShift action_25
action_27 (21) = happyShift action_26
action_27 _ = happyReduce_15

action_28 (11) = happyShift action_17
action_28 (12) = happyShift action_18
action_28 (13) = happyShift action_19
action_28 (17) = happyShift action_22
action_28 (18) = happyShift action_23
action_28 (19) = happyShift action_24
action_28 (20) = happyShift action_25
action_28 (21) = happyShift action_26
action_28 _ = happyReduce_14

action_29 (11) = happyShift action_17
action_29 (12) = happyShift action_18
action_29 (13) = happyShift action_19
action_29 (17) = happyShift action_22
action_29 (18) = happyShift action_23
action_29 (19) = happyShift action_24
action_29 (20) = happyShift action_25
action_29 (21) = happyShift action_26
action_29 _ = happyReduce_13

action_30 (11) = happyShift action_17
action_30 (12) = happyShift action_18
action_30 (13) = happyShift action_19
action_30 (17) = happyShift action_22
action_30 (18) = happyShift action_23
action_30 (19) = happyShift action_24
action_30 (20) = happyShift action_25
action_30 (21) = happyShift action_26
action_30 _ = happyReduce_12

action_31 (11) = happyShift action_17
action_31 (12) = happyShift action_18
action_31 (13) = happyShift action_19
action_31 (17) = happyShift action_22
action_31 (18) = happyShift action_23
action_31 (19) = happyShift action_24
action_31 (20) = happyShift action_25
action_31 (21) = happyShift action_26
action_31 _ = happyReduce_11

action_32 (11) = happyShift action_17
action_32 (12) = happyShift action_18
action_32 (13) = happyShift action_19
action_32 (15) = happyShift action_21
action_32 (17) = happyShift action_22
action_32 (18) = happyShift action_23
action_32 (19) = happyShift action_24
action_32 (20) = happyShift action_25
action_32 (21) = happyShift action_26
action_32 _ = happyReduce_10

action_33 (11) = happyShift action_17
action_33 (12) = happyShift action_18
action_33 (13) = happyShift action_19
action_33 (14) = happyShift action_20
action_33 (15) = happyShift action_21
action_33 (17) = happyShift action_22
action_33 (18) = happyShift action_23
action_33 (19) = happyShift action_24
action_33 (20) = happyShift action_25
action_33 (21) = happyShift action_26
action_33 _ = happyReduce_9

action_34 _ = happyReduce_7

action_35 (13) = happyShift action_19
action_35 _ = happyReduce_6

action_36 (13) = happyShift action_19
action_36 _ = happyReduce_8

action_37 _ = happyReduce_5

action_38 (5) = happyShift action_2
action_38 (6) = happyShift action_4
action_38 (7) = happyShift action_5
action_38 (8) = happyShift action_6
action_38 (9) = happyShift action_7
action_38 (11) = happyShift action_17
action_38 (12) = happyShift action_18
action_38 (13) = happyShift action_19
action_38 (14) = happyShift action_20
action_38 (15) = happyShift action_21
action_38 (16) = happyShift action_8
action_38 (17) = happyShift action_22
action_38 (18) = happyShift action_23
action_38 (19) = happyShift action_24
action_38 (20) = happyShift action_25
action_38 (21) = happyShift action_26
action_38 (22) = happyShift action_9
action_38 (23) = happyShift action_10
action_38 (24) = happyShift action_11
action_38 (4) = happyGoto action_43
action_38 _ = happyFail (happyExpListPerState 38)

action_39 (5) = happyShift action_2
action_39 (6) = happyShift action_4
action_39 (7) = happyShift action_5
action_39 (8) = happyShift action_6
action_39 (9) = happyShift action_7
action_39 (11) = happyShift action_17
action_39 (12) = happyShift action_18
action_39 (13) = happyShift action_19
action_39 (14) = happyShift action_20
action_39 (15) = happyShift action_21
action_39 (16) = happyShift action_8
action_39 (17) = happyShift action_22
action_39 (18) = happyShift action_23
action_39 (19) = happyShift action_24
action_39 (20) = happyShift action_25
action_39 (21) = happyShift action_26
action_39 (22) = happyShift action_9
action_39 (23) = happyShift action_10
action_39 (24) = happyShift action_11
action_39 (4) = happyGoto action_42
action_39 _ = happyFail (happyExpListPerState 39)

action_40 (11) = happyShift action_17
action_40 (12) = happyShift action_18
action_40 (13) = happyShift action_19
action_40 _ = happyReduce_18

action_41 (11) = happyShift action_17
action_41 (12) = happyShift action_18
action_41 (13) = happyShift action_19
action_41 _ = happyReduce_19

action_42 (11) = happyShift action_17
action_42 (12) = happyShift action_18
action_42 (13) = happyShift action_19
action_42 _ = happyReduce_16

action_43 (11) = happyShift action_17
action_43 (12) = happyShift action_18
action_43 (13) = happyShift action_19
action_43 _ = happyReduce_17

happyReduce_1 = happySpecReduce_1  4 happyReduction_1
happyReduction_1 (HappyTerminal (LexMl.TokenBool happy_var_1))
	 =  HappyAbsSyn4
		 (LexMl.MinMlBool happy_var_1
	)
happyReduction_1 _  = notHappyAtAll 

happyReduce_2 = happySpecReduce_1  4 happyReduction_2
happyReduction_2 (HappyTerminal (LexMl.TokenVar happy_var_1))
	 =  HappyAbsSyn4
		 (LexMl.MinMlVar happy_var_1
	)
happyReduction_2 _  = notHappyAtAll 

happyReduce_3 = happySpecReduce_1  4 happyReduction_3
happyReduction_3 (HappyTerminal (LexMl.TokenDigit happy_var_1))
	 =  HappyAbsSyn4
		 (LexMl.MinMlDigit happy_var_1
	)
happyReduction_3 _  = notHappyAtAll 

happyReduce_4 = happySpecReduce_1  4 happyReduction_4
happyReduction_4 (HappyTerminal (LexMl.TokenFun happy_var_1))
	 =  HappyAbsSyn4
		 (LexMl.MinMlFun happy_var_1
	)
happyReduction_4 _  = notHappyAtAll 

happyReduce_5 = happySpecReduce_3  4 happyReduction_5
happyReduction_5 _
	(HappyAbsSyn4  happy_var_2)
	_
	 =  HappyAbsSyn4
		 (happy_var_2
	)
happyReduction_5 _ _ _  = notHappyAtAll 

happyReduce_6 = happySpecReduce_3  4 happyReduction_6
happyReduction_6 (HappyAbsSyn4  happy_var_3)
	_
	(HappyAbsSyn4  happy_var_1)
	 =  HappyAbsSyn4
		 (LexMl.MinMlAdd happy_var_1 happy_var_3
	)
happyReduction_6 _ _ _  = notHappyAtAll 

happyReduce_7 = happySpecReduce_3  4 happyReduction_7
happyReduction_7 (HappyAbsSyn4  happy_var_3)
	_
	(HappyAbsSyn4  happy_var_1)
	 =  HappyAbsSyn4
		 (LexMl.MinMlMul happy_var_1 happy_var_3
	)
happyReduction_7 _ _ _  = notHappyAtAll 

happyReduce_8 = happySpecReduce_3  4 happyReduction_8
happyReduction_8 (HappyAbsSyn4  happy_var_3)
	_
	(HappyAbsSyn4  happy_var_1)
	 =  HappyAbsSyn4
		 (LexMl.MinMlSub happy_var_1 happy_var_3
	)
happyReduction_8 _ _ _  = notHappyAtAll 

happyReduce_9 = happySpecReduce_3  4 happyReduction_9
happyReduction_9 (HappyAbsSyn4  happy_var_3)
	_
	(HappyAbsSyn4  happy_var_1)
	 =  HappyAbsSyn4
		 (LexMl.MinMlOr happy_var_1 happy_var_3
	)
happyReduction_9 _ _ _  = notHappyAtAll 

happyReduce_10 = happySpecReduce_3  4 happyReduction_10
happyReduction_10 (HappyAbsSyn4  happy_var_3)
	_
	(HappyAbsSyn4  happy_var_1)
	 =  HappyAbsSyn4
		 (LexMl.MinMlAnd happy_var_1 happy_var_3
	)
happyReduction_10 _ _ _  = notHappyAtAll 

happyReduce_11 = happySpecReduce_3  4 happyReduction_11
happyReduction_11 (HappyAbsSyn4  happy_var_3)
	_
	(HappyAbsSyn4  happy_var_1)
	 =  HappyAbsSyn4
		 (LexMl.MinMlEq happy_var_1 happy_var_3
	)
happyReduction_11 _ _ _  = notHappyAtAll 

happyReduce_12 = happySpecReduce_3  4 happyReduction_12
happyReduction_12 (HappyAbsSyn4  happy_var_3)
	_
	(HappyAbsSyn4  happy_var_1)
	 =  HappyAbsSyn4
		 (LexMl.MinMlGt happy_var_1 happy_var_3
	)
happyReduction_12 _ _ _  = notHappyAtAll 

happyReduce_13 = happySpecReduce_3  4 happyReduction_13
happyReduction_13 (HappyAbsSyn4  happy_var_3)
	_
	(HappyAbsSyn4  happy_var_1)
	 =  HappyAbsSyn4
		 (LexMl.MinMlLt happy_var_1 happy_var_3
	)
happyReduction_13 _ _ _  = notHappyAtAll 

happyReduce_14 = happySpecReduce_3  4 happyReduction_14
happyReduction_14 (HappyAbsSyn4  happy_var_3)
	_
	(HappyAbsSyn4  happy_var_1)
	 =  HappyAbsSyn4
		 (LexMl.MinMlGtE happy_var_1 happy_var_3
	)
happyReduction_14 _ _ _  = notHappyAtAll 

happyReduce_15 = happySpecReduce_3  4 happyReduction_15
happyReduction_15 (HappyAbsSyn4  happy_var_3)
	_
	(HappyAbsSyn4  happy_var_1)
	 =  HappyAbsSyn4
		 (LexMl.MinMlLtE happy_var_1 happy_var_3
	)
happyReduction_15 _ _ _  = notHappyAtAll 

happyReduce_16 = happyReduce 4 4 happyReduction_16
happyReduction_16 ((HappyAbsSyn4  happy_var_4) `HappyStk`
	(HappyAbsSyn4  happy_var_3) `HappyStk`
	(HappyAbsSyn4  happy_var_2) `HappyStk`
	_ `HappyStk`
	happyRest)
	 = HappyAbsSyn4
		 (LexMl.MinMlIf happy_var_2 happy_var_3 happy_var_4
	) `HappyStk` happyRest

happyReduce_17 = happyReduce 4 4 happyReduction_17
happyReduction_17 ((HappyAbsSyn4  happy_var_4) `HappyStk`
	(HappyAbsSyn4  happy_var_3) `HappyStk`
	(HappyAbsSyn4  happy_var_2) `HappyStk`
	_ `HappyStk`
	happyRest)
	 = HappyAbsSyn4
		 (LexMl.MinMlLet happy_var_2 happy_var_3 happy_var_4
	) `HappyStk` happyRest

happyReduce_18 = happySpecReduce_3  4 happyReduction_18
happyReduction_18 (HappyAbsSyn4  happy_var_3)
	(HappyAbsSyn4  happy_var_2)
	_
	 =  HappyAbsSyn4
		 (LexMl.MinMlLetRec happy_var_2 happy_var_3
	)
happyReduction_18 _ _ _  = notHappyAtAll 

happyReduce_19 = happySpecReduce_3  4 happyReduction_19
happyReduction_19 (HappyAbsSyn4  happy_var_3)
	(HappyAbsSyn4  happy_var_2)
	_
	 =  HappyAbsSyn4
		 (LexMl.MinMlFunc happy_var_2 happy_var_3
	)
happyReduction_19 _ _ _  = notHappyAtAll 

happyNewToken action sts stk [] =
	action 25 25 notHappyAtAll (HappyState action) sts stk []

happyNewToken action sts stk (tk:tks) =
	let cont i = action i i tk (HappyState action) sts stk tks in
	case tk of {
	LexMl.TokenBool happy_dollar_dollar -> cont 5;
	LexMl.TokenVar happy_dollar_dollar -> cont 6;
	LexMl.TokenDigit happy_dollar_dollar -> cont 7;
	LexMl.TokenFun happy_dollar_dollar -> cont 8;
	LexMl.TokenOB -> cont 9;
	LexMl.TokenCB -> cont 10;
	LexMl.TokenSub -> cont 11;
	LexMl.TokenAdd -> cont 12;
	LexMl.TokenMul -> cont 13;
	LexMl.TokenOr -> cont 14;
	LexMl.TokenAnd -> cont 15;
	LexMl.TokenLet -> cont 16;
	LexMl.TokenEq -> cont 17;
	LexMl.TokenGt -> cont 18;
	LexMl.TokenLt -> cont 19;
	LexMl.TokenGtE -> cont 20;
	LexMl.TokenLtE -> cont 21;
	LexMl.TokenIf -> cont 22;
	LexMl.TokenLetRec -> cont 23;
	LexMl.TokenFunc -> cont 24;
	_ -> happyError' ((tk:tks), [])
	}

happyError_ explist 25 tk tks = happyError' (tks, explist)
happyError_ explist _ tk tks = happyError' ((tk:tks), explist)

newtype HappyIdentity a = HappyIdentity a
happyIdentity = HappyIdentity
happyRunIdentity (HappyIdentity a) = a

instance Functor HappyIdentity where
    fmap f (HappyIdentity a) = HappyIdentity (f a)

instance Applicative HappyIdentity where
    pure  = HappyIdentity
    (<*>) = ap
instance Monad HappyIdentity where
    return = pure
    (HappyIdentity p) >>= q = q p

happyThen :: () => HappyIdentity a -> (a -> HappyIdentity b) -> HappyIdentity b
happyThen = (>>=)
happyReturn :: () => a -> HappyIdentity a
happyReturn = (return)
happyThen1 m k tks = (>>=) m (\a -> k a tks)
happyReturn1 :: () => a -> b -> HappyIdentity a
happyReturn1 = \a tks -> (return) a
happyError' :: () => ([(Token)], [String]) -> HappyIdentity a
happyError' = HappyIdentity . (\(tokens, _) -> parseError tokens)
parser tks = happyRunIdentity happySomeParser where
 happySomeParser = happyThen (happyParse action_0 tks) (\x -> case x of {HappyAbsSyn4 z -> happyReturn z; _other -> notHappyAtAll })

happySeq = happyDontSeq


parseError :: [Token] -> a
parseError _ = error "Parse error"

mainInter = getContents >>= print . parser . lexer
{-# LINE 1 "templates/GenericTemplate.hs" #-}
{-# LINE 1 "templates/GenericTemplate.hs" #-}
{-# LINE 1 "<built-in>" #-}
{-# LINE 1 "<command-line>" #-}







# 1 "/usr/include/stdc-predef.h" 1 3 4

# 17 "/usr/include/stdc-predef.h" 3 4











































{-# LINE 7 "<command-line>" #-}
{-# LINE 1 "/usr/lib/ghc/include/ghcversion.h" #-}















{-# LINE 7 "<command-line>" #-}
{-# LINE 1 "/tmp/ghc8336_0/ghc_2.h" #-}
































































































































































































{-# LINE 7 "<command-line>" #-}
{-# LINE 1 "templates/GenericTemplate.hs" #-}
-- Id: GenericTemplate.hs,v 1.26 2005/01/14 14:47:22 simonmar Exp 









{-# LINE 43 "templates/GenericTemplate.hs" #-}

data Happy_IntList = HappyCons Int Happy_IntList







{-# LINE 65 "templates/GenericTemplate.hs" #-}

{-# LINE 75 "templates/GenericTemplate.hs" #-}

{-# LINE 84 "templates/GenericTemplate.hs" #-}

infixr 9 `HappyStk`
data HappyStk a = HappyStk a (HappyStk a)

-----------------------------------------------------------------------------
-- starting the parse

happyParse start_state = happyNewToken start_state notHappyAtAll notHappyAtAll

-----------------------------------------------------------------------------
-- Accepting the parse

-- If the current token is (1), it means we've just accepted a partial
-- parse (a %partial parser).  We must ignore the saved token on the top of
-- the stack in this case.
happyAccept (1) tk st sts (_ `HappyStk` ans `HappyStk` _) =
        happyReturn1 ans
happyAccept j tk st sts (HappyStk ans _) = 
         (happyReturn1 ans)

-----------------------------------------------------------------------------
-- Arrays only: do the next action

{-# LINE 137 "templates/GenericTemplate.hs" #-}

{-# LINE 147 "templates/GenericTemplate.hs" #-}
indexShortOffAddr arr off = arr Happy_Data_Array.! off


{-# INLINE happyLt #-}
happyLt x y = (x < y)






readArrayBit arr bit =
    Bits.testBit (indexShortOffAddr arr (bit `div` 16)) (bit `mod` 16)






-----------------------------------------------------------------------------
-- HappyState data type (not arrays)



newtype HappyState b c = HappyState
        (Int ->                    -- token number
         Int ->                    -- token number (yes, again)
         b ->                           -- token semantic value
         HappyState b c ->              -- current state
         [HappyState b c] ->            -- state stack
         c)



-----------------------------------------------------------------------------
-- Shifting a token

happyShift new_state (1) tk st sts stk@(x `HappyStk` _) =
     let i = (case x of { HappyErrorToken (i) -> i }) in
--     trace "shifting the error token" $
     new_state i i tk (HappyState (new_state)) ((st):(sts)) (stk)

happyShift new_state i tk st sts stk =
     happyNewToken new_state ((st):(sts)) ((HappyTerminal (tk))`HappyStk`stk)

-- happyReduce is specialised for the common cases.

happySpecReduce_0 i fn (1) tk st sts stk
     = happyFail [] (1) tk st sts stk
happySpecReduce_0 nt fn j tk st@((HappyState (action))) sts stk
     = action nt j tk st ((st):(sts)) (fn `HappyStk` stk)

happySpecReduce_1 i fn (1) tk st sts stk
     = happyFail [] (1) tk st sts stk
happySpecReduce_1 nt fn j tk _ sts@(((st@(HappyState (action))):(_))) (v1`HappyStk`stk')
     = let r = fn v1 in
       happySeq r (action nt j tk st sts (r `HappyStk` stk'))

happySpecReduce_2 i fn (1) tk st sts stk
     = happyFail [] (1) tk st sts stk
happySpecReduce_2 nt fn j tk _ ((_):(sts@(((st@(HappyState (action))):(_))))) (v1`HappyStk`v2`HappyStk`stk')
     = let r = fn v1 v2 in
       happySeq r (action nt j tk st sts (r `HappyStk` stk'))

happySpecReduce_3 i fn (1) tk st sts stk
     = happyFail [] (1) tk st sts stk
happySpecReduce_3 nt fn j tk _ ((_):(((_):(sts@(((st@(HappyState (action))):(_))))))) (v1`HappyStk`v2`HappyStk`v3`HappyStk`stk')
     = let r = fn v1 v2 v3 in
       happySeq r (action nt j tk st sts (r `HappyStk` stk'))

happyReduce k i fn (1) tk st sts stk
     = happyFail [] (1) tk st sts stk
happyReduce k nt fn j tk st sts stk
     = case happyDrop (k - ((1) :: Int)) sts of
         sts1@(((st1@(HappyState (action))):(_))) ->
                let r = fn stk in  -- it doesn't hurt to always seq here...
                happyDoSeq r (action nt j tk st1 sts1 r)

happyMonadReduce k nt fn (1) tk st sts stk
     = happyFail [] (1) tk st sts stk
happyMonadReduce k nt fn j tk st sts stk =
      case happyDrop k ((st):(sts)) of
        sts1@(((st1@(HappyState (action))):(_))) ->
          let drop_stk = happyDropStk k stk in
          happyThen1 (fn stk tk) (\r -> action nt j tk st1 sts1 (r `HappyStk` drop_stk))

happyMonad2Reduce k nt fn (1) tk st sts stk
     = happyFail [] (1) tk st sts stk
happyMonad2Reduce k nt fn j tk st sts stk =
      case happyDrop k ((st):(sts)) of
        sts1@(((st1@(HappyState (action))):(_))) ->
         let drop_stk = happyDropStk k stk





             _ = nt :: Int
             new_state = action

          in
          happyThen1 (fn stk tk) (\r -> happyNewToken new_state sts1 (r `HappyStk` drop_stk))

happyDrop (0) l = l
happyDrop n ((_):(t)) = happyDrop (n - ((1) :: Int)) t

happyDropStk (0) l = l
happyDropStk n (x `HappyStk` xs) = happyDropStk (n - ((1)::Int)) xs

-----------------------------------------------------------------------------
-- Moving to a new state after a reduction

{-# LINE 267 "templates/GenericTemplate.hs" #-}
happyGoto action j tk st = action j j tk (HappyState action)


-----------------------------------------------------------------------------
-- Error recovery ((1) is the error token)

-- parse error if we are in recovery and we fail again
happyFail explist (1) tk old_st _ stk@(x `HappyStk` _) =
     let i = (case x of { HappyErrorToken (i) -> i }) in
--      trace "failing" $ 
        happyError_ explist i tk

{-  We don't need state discarding for our restricted implementation of
    "error".  In fact, it can cause some bogus parses, so I've disabled it
    for now --SDM

-- discard a state
happyFail  (1) tk old_st (((HappyState (action))):(sts)) 
                                                (saved_tok `HappyStk` _ `HappyStk` stk) =
--      trace ("discarding state, depth " ++ show (length stk))  $
        action (1) (1) tk (HappyState (action)) sts ((saved_tok`HappyStk`stk))
-}

-- Enter error recovery: generate an error token,
--                       save the old token and carry on.
happyFail explist i tk (HappyState (action)) sts stk =
--      trace "entering error recovery" $
        action (1) (1) tk (HappyState (action)) sts ( (HappyErrorToken (i)) `HappyStk` stk)

-- Internal happy errors:

notHappyAtAll :: a
notHappyAtAll = error "Internal Happy error\n"

-----------------------------------------------------------------------------
-- Hack to get the typechecker to accept our action functions







-----------------------------------------------------------------------------
-- Seq-ing.  If the --strict flag is given, then Happy emits 
--      happySeq = happyDoSeq
-- otherwise it emits
--      happySeq = happyDontSeq

happyDoSeq, happyDontSeq :: a -> b -> b
happyDoSeq   a b = a `seq` b
happyDontSeq a b = b

-----------------------------------------------------------------------------
-- Don't inline any functions from the template.  GHC has a nasty habit
-- of deciding to inline happyGoto everywhere, which increases the size of
-- the generated parser quite a bit.

{-# LINE 333 "templates/GenericTemplate.hs" #-}
{-# NOINLINE happyShift #-}
{-# NOINLINE happySpecReduce_0 #-}
{-# NOINLINE happySpecReduce_1 #-}
{-# NOINLINE happySpecReduce_2 #-}
{-# NOINLINE happySpecReduce_3 #-}
{-# NOINLINE happyReduce #-}
{-# NOINLINE happyMonadReduce #-}
{-# NOINLINE happyGoto #-}
{-# NOINLINE happyFail #-}

-- end of Happy Template.
