{-# OPTIONS_GHC -w #-}
module MinHs.Parser where
import MinHs.Lexer as LexHs
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
happyExpList = Happy_Data_Array.listArray (0,170) ([33264,15328,8,0,0,0,61312,131,0,0,0,0,0,57344,49411,61559,57473,63547,61504,31773,63520,15886,31760,7943,48648,3971,57092,1,0,0,0,0,0,65272,8191,65404,4095,65470,2047,65503,33791,65519,49663,65527,255,16124,61448,57473,63547,61504,31773,63520,15886,31760,7943,48648,3971,57092,1985,61314,992,30657,33264,15328,16632,7664,8316,3832,61312,3,61888,1,63712,0,31856,0,15928,0,7964,0,4014,0,2015,0,0,0,1,32768,0,0,0,64480,32767,65008,16383,65272,8191,1792,0,65470,2047,65503,1023,224,49152,65527,57599,65531,127,28,0,14,31744,65535,48655,65535,57095,65535,57347,0,63424,65535,14336,0,0
	])

{-# NOINLINE happyExpListPerState #-}
happyExpListPerState st =
    token_strs_expected
  where token_strs = ["error","%dummy","%start_parser","Expr","boolean","var","digit","fun","'('","')'","'-'","'+'","'*'","expror","exprand","exprlet","expreq","exprgt","exprlt","exprgte","exprlte","exprif","exprrecfun","exprfunc","exprrecfun2","exprfunc2","arrow","nat","bool","void","%eof"]
        bit_start = st * 31
        bit_end = (st + 1) * 31
        read_bit = readArrayBit happyExpList
        bits = map read_bit [bit_start..bit_end - 1]
        bits_indexed = zip bits [0..30]
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
action_0 (25) = happyShift action_12
action_0 (26) = happyShift action_13
action_0 (28) = happyShift action_14
action_0 (29) = happyShift action_15
action_0 (30) = happyShift action_16
action_0 (4) = happyGoto action_3
action_0 _ = happyFail (happyExpListPerState 0)

action_1 (5) = happyShift action_2
action_1 _ = happyFail (happyExpListPerState 1)

action_2 _ = happyReduce_1

action_3 (11) = happyShift action_24
action_3 (12) = happyShift action_25
action_3 (13) = happyShift action_26
action_3 (14) = happyShift action_27
action_3 (15) = happyShift action_28
action_3 (17) = happyShift action_29
action_3 (18) = happyShift action_30
action_3 (19) = happyShift action_31
action_3 (20) = happyShift action_32
action_3 (21) = happyShift action_33
action_3 (27) = happyShift action_34
action_3 (31) = happyAccept
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
action_7 (25) = happyShift action_12
action_7 (26) = happyShift action_13
action_7 (28) = happyShift action_14
action_7 (29) = happyShift action_15
action_7 (30) = happyShift action_16
action_7 (4) = happyGoto action_23
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
action_8 (25) = happyShift action_12
action_8 (26) = happyShift action_13
action_8 (28) = happyShift action_14
action_8 (29) = happyShift action_15
action_8 (30) = happyShift action_16
action_8 (4) = happyGoto action_22
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
action_9 (25) = happyShift action_12
action_9 (26) = happyShift action_13
action_9 (28) = happyShift action_14
action_9 (29) = happyShift action_15
action_9 (30) = happyShift action_16
action_9 (4) = happyGoto action_21
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
action_10 (25) = happyShift action_12
action_10 (26) = happyShift action_13
action_10 (28) = happyShift action_14
action_10 (29) = happyShift action_15
action_10 (30) = happyShift action_16
action_10 (4) = happyGoto action_20
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
action_11 (25) = happyShift action_12
action_11 (26) = happyShift action_13
action_11 (28) = happyShift action_14
action_11 (29) = happyShift action_15
action_11 (30) = happyShift action_16
action_11 (4) = happyGoto action_19
action_11 _ = happyFail (happyExpListPerState 11)

action_12 (5) = happyShift action_2
action_12 (6) = happyShift action_4
action_12 (7) = happyShift action_5
action_12 (8) = happyShift action_6
action_12 (9) = happyShift action_7
action_12 (16) = happyShift action_8
action_12 (22) = happyShift action_9
action_12 (23) = happyShift action_10
action_12 (24) = happyShift action_11
action_12 (25) = happyShift action_12
action_12 (26) = happyShift action_13
action_12 (28) = happyShift action_14
action_12 (29) = happyShift action_15
action_12 (30) = happyShift action_16
action_12 (4) = happyGoto action_18
action_12 _ = happyFail (happyExpListPerState 12)

action_13 (5) = happyShift action_2
action_13 (6) = happyShift action_4
action_13 (7) = happyShift action_5
action_13 (8) = happyShift action_6
action_13 (9) = happyShift action_7
action_13 (16) = happyShift action_8
action_13 (22) = happyShift action_9
action_13 (23) = happyShift action_10
action_13 (24) = happyShift action_11
action_13 (25) = happyShift action_12
action_13 (26) = happyShift action_13
action_13 (28) = happyShift action_14
action_13 (29) = happyShift action_15
action_13 (30) = happyShift action_16
action_13 (4) = happyGoto action_17
action_13 _ = happyFail (happyExpListPerState 13)

action_14 _ = happyReduce_23

action_15 _ = happyReduce_24

action_16 _ = happyReduce_5

action_17 (5) = happyShift action_2
action_17 (6) = happyShift action_4
action_17 (7) = happyShift action_5
action_17 (8) = happyShift action_6
action_17 (9) = happyShift action_7
action_17 (11) = happyShift action_24
action_17 (12) = happyShift action_25
action_17 (13) = happyShift action_26
action_17 (14) = happyShift action_27
action_17 (15) = happyShift action_28
action_17 (16) = happyShift action_8
action_17 (17) = happyShift action_29
action_17 (18) = happyShift action_30
action_17 (19) = happyShift action_31
action_17 (20) = happyShift action_32
action_17 (21) = happyShift action_33
action_17 (22) = happyShift action_9
action_17 (23) = happyShift action_10
action_17 (24) = happyShift action_11
action_17 (25) = happyShift action_12
action_17 (26) = happyShift action_13
action_17 (27) = happyShift action_34
action_17 (28) = happyShift action_14
action_17 (29) = happyShift action_15
action_17 (30) = happyShift action_16
action_17 (4) = happyGoto action_52
action_17 _ = happyFail (happyExpListPerState 17)

action_18 (5) = happyShift action_2
action_18 (6) = happyShift action_4
action_18 (7) = happyShift action_5
action_18 (8) = happyShift action_6
action_18 (9) = happyShift action_7
action_18 (11) = happyShift action_24
action_18 (12) = happyShift action_25
action_18 (13) = happyShift action_26
action_18 (14) = happyShift action_27
action_18 (15) = happyShift action_28
action_18 (16) = happyShift action_8
action_18 (17) = happyShift action_29
action_18 (18) = happyShift action_30
action_18 (19) = happyShift action_31
action_18 (20) = happyShift action_32
action_18 (21) = happyShift action_33
action_18 (22) = happyShift action_9
action_18 (23) = happyShift action_10
action_18 (24) = happyShift action_11
action_18 (25) = happyShift action_12
action_18 (26) = happyShift action_13
action_18 (27) = happyShift action_34
action_18 (28) = happyShift action_14
action_18 (29) = happyShift action_15
action_18 (30) = happyShift action_16
action_18 (4) = happyGoto action_51
action_18 _ = happyFail (happyExpListPerState 18)

action_19 (5) = happyShift action_2
action_19 (6) = happyShift action_4
action_19 (7) = happyShift action_5
action_19 (8) = happyShift action_6
action_19 (9) = happyShift action_7
action_19 (11) = happyShift action_24
action_19 (12) = happyShift action_25
action_19 (13) = happyShift action_26
action_19 (14) = happyShift action_27
action_19 (15) = happyShift action_28
action_19 (16) = happyShift action_8
action_19 (17) = happyShift action_29
action_19 (18) = happyShift action_30
action_19 (19) = happyShift action_31
action_19 (20) = happyShift action_32
action_19 (21) = happyShift action_33
action_19 (22) = happyShift action_9
action_19 (23) = happyShift action_10
action_19 (24) = happyShift action_11
action_19 (25) = happyShift action_12
action_19 (26) = happyShift action_13
action_19 (27) = happyShift action_34
action_19 (28) = happyShift action_14
action_19 (29) = happyShift action_15
action_19 (30) = happyShift action_16
action_19 (4) = happyGoto action_50
action_19 _ = happyFail (happyExpListPerState 19)

action_20 (5) = happyShift action_2
action_20 (6) = happyShift action_4
action_20 (7) = happyShift action_5
action_20 (8) = happyShift action_6
action_20 (9) = happyShift action_7
action_20 (11) = happyShift action_24
action_20 (12) = happyShift action_25
action_20 (13) = happyShift action_26
action_20 (14) = happyShift action_27
action_20 (15) = happyShift action_28
action_20 (16) = happyShift action_8
action_20 (17) = happyShift action_29
action_20 (18) = happyShift action_30
action_20 (19) = happyShift action_31
action_20 (20) = happyShift action_32
action_20 (21) = happyShift action_33
action_20 (22) = happyShift action_9
action_20 (23) = happyShift action_10
action_20 (24) = happyShift action_11
action_20 (25) = happyShift action_12
action_20 (26) = happyShift action_13
action_20 (27) = happyShift action_34
action_20 (28) = happyShift action_14
action_20 (29) = happyShift action_15
action_20 (30) = happyShift action_16
action_20 (4) = happyGoto action_49
action_20 _ = happyFail (happyExpListPerState 20)

action_21 (5) = happyShift action_2
action_21 (6) = happyShift action_4
action_21 (7) = happyShift action_5
action_21 (8) = happyShift action_6
action_21 (9) = happyShift action_7
action_21 (11) = happyShift action_24
action_21 (12) = happyShift action_25
action_21 (13) = happyShift action_26
action_21 (14) = happyShift action_27
action_21 (15) = happyShift action_28
action_21 (16) = happyShift action_8
action_21 (17) = happyShift action_29
action_21 (18) = happyShift action_30
action_21 (19) = happyShift action_31
action_21 (20) = happyShift action_32
action_21 (21) = happyShift action_33
action_21 (22) = happyShift action_9
action_21 (23) = happyShift action_10
action_21 (24) = happyShift action_11
action_21 (25) = happyShift action_12
action_21 (26) = happyShift action_13
action_21 (27) = happyShift action_34
action_21 (28) = happyShift action_14
action_21 (29) = happyShift action_15
action_21 (30) = happyShift action_16
action_21 (4) = happyGoto action_48
action_21 _ = happyFail (happyExpListPerState 21)

action_22 (5) = happyShift action_2
action_22 (6) = happyShift action_4
action_22 (7) = happyShift action_5
action_22 (8) = happyShift action_6
action_22 (9) = happyShift action_7
action_22 (11) = happyShift action_24
action_22 (12) = happyShift action_25
action_22 (13) = happyShift action_26
action_22 (14) = happyShift action_27
action_22 (15) = happyShift action_28
action_22 (16) = happyShift action_8
action_22 (17) = happyShift action_29
action_22 (18) = happyShift action_30
action_22 (19) = happyShift action_31
action_22 (20) = happyShift action_32
action_22 (21) = happyShift action_33
action_22 (22) = happyShift action_9
action_22 (23) = happyShift action_10
action_22 (24) = happyShift action_11
action_22 (25) = happyShift action_12
action_22 (26) = happyShift action_13
action_22 (27) = happyShift action_34
action_22 (28) = happyShift action_14
action_22 (29) = happyShift action_15
action_22 (30) = happyShift action_16
action_22 (4) = happyGoto action_47
action_22 _ = happyFail (happyExpListPerState 22)

action_23 (10) = happyShift action_46
action_23 (11) = happyShift action_24
action_23 (12) = happyShift action_25
action_23 (13) = happyShift action_26
action_23 (14) = happyShift action_27
action_23 (15) = happyShift action_28
action_23 (17) = happyShift action_29
action_23 (18) = happyShift action_30
action_23 (19) = happyShift action_31
action_23 (20) = happyShift action_32
action_23 (21) = happyShift action_33
action_23 (27) = happyShift action_34
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
action_24 (25) = happyShift action_12
action_24 (26) = happyShift action_13
action_24 (28) = happyShift action_14
action_24 (29) = happyShift action_15
action_24 (30) = happyShift action_16
action_24 (4) = happyGoto action_45
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
action_25 (25) = happyShift action_12
action_25 (26) = happyShift action_13
action_25 (28) = happyShift action_14
action_25 (29) = happyShift action_15
action_25 (30) = happyShift action_16
action_25 (4) = happyGoto action_44
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
action_26 (25) = happyShift action_12
action_26 (26) = happyShift action_13
action_26 (28) = happyShift action_14
action_26 (29) = happyShift action_15
action_26 (30) = happyShift action_16
action_26 (4) = happyGoto action_43
action_26 _ = happyFail (happyExpListPerState 26)

action_27 (5) = happyShift action_2
action_27 (6) = happyShift action_4
action_27 (7) = happyShift action_5
action_27 (8) = happyShift action_6
action_27 (9) = happyShift action_7
action_27 (16) = happyShift action_8
action_27 (22) = happyShift action_9
action_27 (23) = happyShift action_10
action_27 (24) = happyShift action_11
action_27 (25) = happyShift action_12
action_27 (26) = happyShift action_13
action_27 (28) = happyShift action_14
action_27 (29) = happyShift action_15
action_27 (30) = happyShift action_16
action_27 (4) = happyGoto action_42
action_27 _ = happyFail (happyExpListPerState 27)

action_28 (5) = happyShift action_2
action_28 (6) = happyShift action_4
action_28 (7) = happyShift action_5
action_28 (8) = happyShift action_6
action_28 (9) = happyShift action_7
action_28 (16) = happyShift action_8
action_28 (22) = happyShift action_9
action_28 (23) = happyShift action_10
action_28 (24) = happyShift action_11
action_28 (25) = happyShift action_12
action_28 (26) = happyShift action_13
action_28 (28) = happyShift action_14
action_28 (29) = happyShift action_15
action_28 (30) = happyShift action_16
action_28 (4) = happyGoto action_41
action_28 _ = happyFail (happyExpListPerState 28)

action_29 (5) = happyShift action_2
action_29 (6) = happyShift action_4
action_29 (7) = happyShift action_5
action_29 (8) = happyShift action_6
action_29 (9) = happyShift action_7
action_29 (16) = happyShift action_8
action_29 (22) = happyShift action_9
action_29 (23) = happyShift action_10
action_29 (24) = happyShift action_11
action_29 (25) = happyShift action_12
action_29 (26) = happyShift action_13
action_29 (28) = happyShift action_14
action_29 (29) = happyShift action_15
action_29 (30) = happyShift action_16
action_29 (4) = happyGoto action_40
action_29 _ = happyFail (happyExpListPerState 29)

action_30 (5) = happyShift action_2
action_30 (6) = happyShift action_4
action_30 (7) = happyShift action_5
action_30 (8) = happyShift action_6
action_30 (9) = happyShift action_7
action_30 (16) = happyShift action_8
action_30 (22) = happyShift action_9
action_30 (23) = happyShift action_10
action_30 (24) = happyShift action_11
action_30 (25) = happyShift action_12
action_30 (26) = happyShift action_13
action_30 (28) = happyShift action_14
action_30 (29) = happyShift action_15
action_30 (30) = happyShift action_16
action_30 (4) = happyGoto action_39
action_30 _ = happyFail (happyExpListPerState 30)

action_31 (5) = happyShift action_2
action_31 (6) = happyShift action_4
action_31 (7) = happyShift action_5
action_31 (8) = happyShift action_6
action_31 (9) = happyShift action_7
action_31 (16) = happyShift action_8
action_31 (22) = happyShift action_9
action_31 (23) = happyShift action_10
action_31 (24) = happyShift action_11
action_31 (25) = happyShift action_12
action_31 (26) = happyShift action_13
action_31 (28) = happyShift action_14
action_31 (29) = happyShift action_15
action_31 (30) = happyShift action_16
action_31 (4) = happyGoto action_38
action_31 _ = happyFail (happyExpListPerState 31)

action_32 (5) = happyShift action_2
action_32 (6) = happyShift action_4
action_32 (7) = happyShift action_5
action_32 (8) = happyShift action_6
action_32 (9) = happyShift action_7
action_32 (16) = happyShift action_8
action_32 (22) = happyShift action_9
action_32 (23) = happyShift action_10
action_32 (24) = happyShift action_11
action_32 (25) = happyShift action_12
action_32 (26) = happyShift action_13
action_32 (28) = happyShift action_14
action_32 (29) = happyShift action_15
action_32 (30) = happyShift action_16
action_32 (4) = happyGoto action_37
action_32 _ = happyFail (happyExpListPerState 32)

action_33 (5) = happyShift action_2
action_33 (6) = happyShift action_4
action_33 (7) = happyShift action_5
action_33 (8) = happyShift action_6
action_33 (9) = happyShift action_7
action_33 (16) = happyShift action_8
action_33 (22) = happyShift action_9
action_33 (23) = happyShift action_10
action_33 (24) = happyShift action_11
action_33 (25) = happyShift action_12
action_33 (26) = happyShift action_13
action_33 (28) = happyShift action_14
action_33 (29) = happyShift action_15
action_33 (30) = happyShift action_16
action_33 (4) = happyGoto action_36
action_33 _ = happyFail (happyExpListPerState 33)

action_34 (5) = happyShift action_2
action_34 (6) = happyShift action_4
action_34 (7) = happyShift action_5
action_34 (8) = happyShift action_6
action_34 (9) = happyShift action_7
action_34 (16) = happyShift action_8
action_34 (22) = happyShift action_9
action_34 (23) = happyShift action_10
action_34 (24) = happyShift action_11
action_34 (25) = happyShift action_12
action_34 (26) = happyShift action_13
action_34 (28) = happyShift action_14
action_34 (29) = happyShift action_15
action_34 (30) = happyShift action_16
action_34 (4) = happyGoto action_35
action_34 _ = happyFail (happyExpListPerState 34)

action_35 (11) = happyShift action_24
action_35 (12) = happyShift action_25
action_35 (13) = happyShift action_26
action_35 (14) = happyShift action_27
action_35 (15) = happyShift action_28
action_35 (17) = happyShift action_29
action_35 (18) = happyShift action_30
action_35 (19) = happyShift action_31
action_35 (20) = happyShift action_32
action_35 (21) = happyShift action_33
action_35 (27) = happyFail []
action_35 _ = happyReduce_25

action_36 (11) = happyShift action_24
action_36 (12) = happyShift action_25
action_36 (13) = happyShift action_26
action_36 (17) = happyShift action_29
action_36 (18) = happyShift action_30
action_36 (19) = happyShift action_31
action_36 (20) = happyShift action_32
action_36 (21) = happyShift action_33
action_36 _ = happyReduce_16

action_37 (11) = happyShift action_24
action_37 (12) = happyShift action_25
action_37 (13) = happyShift action_26
action_37 (17) = happyShift action_29
action_37 (18) = happyShift action_30
action_37 (19) = happyShift action_31
action_37 (20) = happyShift action_32
action_37 (21) = happyShift action_33
action_37 _ = happyReduce_15

action_38 (11) = happyShift action_24
action_38 (12) = happyShift action_25
action_38 (13) = happyShift action_26
action_38 (17) = happyShift action_29
action_38 (18) = happyShift action_30
action_38 (19) = happyShift action_31
action_38 (20) = happyShift action_32
action_38 (21) = happyShift action_33
action_38 _ = happyReduce_14

action_39 (11) = happyShift action_24
action_39 (12) = happyShift action_25
action_39 (13) = happyShift action_26
action_39 (17) = happyShift action_29
action_39 (18) = happyShift action_30
action_39 (19) = happyShift action_31
action_39 (20) = happyShift action_32
action_39 (21) = happyShift action_33
action_39 _ = happyReduce_13

action_40 (11) = happyShift action_24
action_40 (12) = happyShift action_25
action_40 (13) = happyShift action_26
action_40 (17) = happyShift action_29
action_40 (18) = happyShift action_30
action_40 (19) = happyShift action_31
action_40 (20) = happyShift action_32
action_40 (21) = happyShift action_33
action_40 _ = happyReduce_12

action_41 (11) = happyShift action_24
action_41 (12) = happyShift action_25
action_41 (13) = happyShift action_26
action_41 (15) = happyShift action_28
action_41 (17) = happyShift action_29
action_41 (18) = happyShift action_30
action_41 (19) = happyShift action_31
action_41 (20) = happyShift action_32
action_41 (21) = happyShift action_33
action_41 _ = happyReduce_11

action_42 (11) = happyShift action_24
action_42 (12) = happyShift action_25
action_42 (13) = happyShift action_26
action_42 (14) = happyShift action_27
action_42 (15) = happyShift action_28
action_42 (17) = happyShift action_29
action_42 (18) = happyShift action_30
action_42 (19) = happyShift action_31
action_42 (20) = happyShift action_32
action_42 (21) = happyShift action_33
action_42 _ = happyReduce_10

action_43 _ = happyReduce_8

action_44 (13) = happyShift action_26
action_44 _ = happyReduce_7

action_45 (13) = happyShift action_26
action_45 _ = happyReduce_9

action_46 _ = happyReduce_6

action_47 (5) = happyShift action_2
action_47 (6) = happyShift action_4
action_47 (7) = happyShift action_5
action_47 (8) = happyShift action_6
action_47 (9) = happyShift action_7
action_47 (11) = happyShift action_24
action_47 (12) = happyShift action_25
action_47 (13) = happyShift action_26
action_47 (14) = happyShift action_27
action_47 (15) = happyShift action_28
action_47 (16) = happyShift action_8
action_47 (17) = happyShift action_29
action_47 (18) = happyShift action_30
action_47 (19) = happyShift action_31
action_47 (20) = happyShift action_32
action_47 (21) = happyShift action_33
action_47 (22) = happyShift action_9
action_47 (23) = happyShift action_10
action_47 (24) = happyShift action_11
action_47 (25) = happyShift action_12
action_47 (26) = happyShift action_13
action_47 (27) = happyShift action_34
action_47 (28) = happyShift action_14
action_47 (29) = happyShift action_15
action_47 (30) = happyShift action_16
action_47 (4) = happyGoto action_57
action_47 _ = happyFail (happyExpListPerState 47)

action_48 (5) = happyShift action_2
action_48 (6) = happyShift action_4
action_48 (7) = happyShift action_5
action_48 (8) = happyShift action_6
action_48 (9) = happyShift action_7
action_48 (11) = happyShift action_24
action_48 (12) = happyShift action_25
action_48 (13) = happyShift action_26
action_48 (14) = happyShift action_27
action_48 (15) = happyShift action_28
action_48 (16) = happyShift action_8
action_48 (17) = happyShift action_29
action_48 (18) = happyShift action_30
action_48 (19) = happyShift action_31
action_48 (20) = happyShift action_32
action_48 (21) = happyShift action_33
action_48 (22) = happyShift action_9
action_48 (23) = happyShift action_10
action_48 (24) = happyShift action_11
action_48 (25) = happyShift action_12
action_48 (26) = happyShift action_13
action_48 (27) = happyShift action_34
action_48 (28) = happyShift action_14
action_48 (29) = happyShift action_15
action_48 (30) = happyShift action_16
action_48 (4) = happyGoto action_56
action_48 _ = happyFail (happyExpListPerState 48)

action_49 (5) = happyShift action_2
action_49 (6) = happyShift action_4
action_49 (7) = happyShift action_5
action_49 (8) = happyShift action_6
action_49 (9) = happyShift action_7
action_49 (11) = happyShift action_24
action_49 (12) = happyShift action_25
action_49 (13) = happyShift action_26
action_49 (14) = happyShift action_27
action_49 (15) = happyShift action_28
action_49 (16) = happyShift action_8
action_49 (17) = happyShift action_29
action_49 (18) = happyShift action_30
action_49 (19) = happyShift action_31
action_49 (20) = happyShift action_32
action_49 (21) = happyShift action_33
action_49 (22) = happyShift action_9
action_49 (23) = happyShift action_10
action_49 (24) = happyShift action_11
action_49 (25) = happyShift action_12
action_49 (26) = happyShift action_13
action_49 (27) = happyShift action_34
action_49 (28) = happyShift action_14
action_49 (29) = happyShift action_15
action_49 (30) = happyShift action_16
action_49 (4) = happyGoto action_55
action_49 _ = happyFail (happyExpListPerState 49)

action_50 (11) = happyShift action_24
action_50 (12) = happyShift action_25
action_50 (13) = happyShift action_26
action_50 _ = happyReduce_20

action_51 (5) = happyShift action_2
action_51 (6) = happyShift action_4
action_51 (7) = happyShift action_5
action_51 (8) = happyShift action_6
action_51 (9) = happyShift action_7
action_51 (11) = happyShift action_24
action_51 (12) = happyShift action_25
action_51 (13) = happyShift action_26
action_51 (14) = happyShift action_27
action_51 (15) = happyShift action_28
action_51 (16) = happyShift action_8
action_51 (17) = happyShift action_29
action_51 (18) = happyShift action_30
action_51 (19) = happyShift action_31
action_51 (20) = happyShift action_32
action_51 (21) = happyShift action_33
action_51 (22) = happyShift action_9
action_51 (23) = happyShift action_10
action_51 (24) = happyShift action_11
action_51 (25) = happyShift action_12
action_51 (26) = happyShift action_13
action_51 (27) = happyShift action_34
action_51 (28) = happyShift action_14
action_51 (29) = happyShift action_15
action_51 (30) = happyShift action_16
action_51 (4) = happyGoto action_54
action_51 _ = happyFail (happyExpListPerState 51)

action_52 (5) = happyShift action_2
action_52 (6) = happyShift action_4
action_52 (7) = happyShift action_5
action_52 (8) = happyShift action_6
action_52 (9) = happyShift action_7
action_52 (11) = happyShift action_24
action_52 (12) = happyShift action_25
action_52 (13) = happyShift action_26
action_52 (14) = happyShift action_27
action_52 (15) = happyShift action_28
action_52 (16) = happyShift action_8
action_52 (17) = happyShift action_29
action_52 (18) = happyShift action_30
action_52 (19) = happyShift action_31
action_52 (20) = happyShift action_32
action_52 (21) = happyShift action_33
action_52 (22) = happyShift action_9
action_52 (23) = happyShift action_10
action_52 (24) = happyShift action_11
action_52 (25) = happyShift action_12
action_52 (26) = happyShift action_13
action_52 (27) = happyShift action_34
action_52 (28) = happyShift action_14
action_52 (29) = happyShift action_15
action_52 (30) = happyShift action_16
action_52 (4) = happyGoto action_53
action_52 _ = happyFail (happyExpListPerState 52)

action_53 (11) = happyShift action_24
action_53 (12) = happyShift action_25
action_53 (13) = happyShift action_26
action_53 _ = happyReduce_22

action_54 (5) = happyShift action_2
action_54 (6) = happyShift action_4
action_54 (7) = happyShift action_5
action_54 (8) = happyShift action_6
action_54 (9) = happyShift action_7
action_54 (11) = happyShift action_24
action_54 (12) = happyShift action_25
action_54 (13) = happyShift action_26
action_54 (14) = happyShift action_27
action_54 (15) = happyShift action_28
action_54 (16) = happyShift action_8
action_54 (17) = happyShift action_29
action_54 (18) = happyShift action_30
action_54 (19) = happyShift action_31
action_54 (20) = happyShift action_32
action_54 (21) = happyShift action_33
action_54 (22) = happyShift action_9
action_54 (23) = happyShift action_10
action_54 (24) = happyShift action_11
action_54 (25) = happyShift action_12
action_54 (26) = happyShift action_13
action_54 (27) = happyShift action_34
action_54 (28) = happyShift action_14
action_54 (29) = happyShift action_15
action_54 (30) = happyShift action_16
action_54 (4) = happyGoto action_59
action_54 _ = happyFail (happyExpListPerState 54)

action_55 (5) = happyShift action_2
action_55 (6) = happyShift action_4
action_55 (7) = happyShift action_5
action_55 (8) = happyShift action_6
action_55 (9) = happyShift action_7
action_55 (11) = happyShift action_24
action_55 (12) = happyShift action_25
action_55 (13) = happyShift action_26
action_55 (14) = happyShift action_27
action_55 (15) = happyShift action_28
action_55 (16) = happyShift action_8
action_55 (17) = happyShift action_29
action_55 (18) = happyShift action_30
action_55 (19) = happyShift action_31
action_55 (20) = happyShift action_32
action_55 (21) = happyShift action_33
action_55 (22) = happyShift action_9
action_55 (23) = happyShift action_10
action_55 (24) = happyShift action_11
action_55 (25) = happyShift action_12
action_55 (26) = happyShift action_13
action_55 (27) = happyShift action_34
action_55 (28) = happyShift action_14
action_55 (29) = happyShift action_15
action_55 (30) = happyShift action_16
action_55 (4) = happyGoto action_58
action_55 _ = happyFail (happyExpListPerState 55)

action_56 (11) = happyShift action_24
action_56 (12) = happyShift action_25
action_56 (13) = happyShift action_26
action_56 _ = happyReduce_17

action_57 (11) = happyShift action_24
action_57 (12) = happyShift action_25
action_57 (13) = happyShift action_26
action_57 _ = happyReduce_18

action_58 (5) = happyShift action_2
action_58 (6) = happyShift action_4
action_58 (7) = happyShift action_5
action_58 (8) = happyShift action_6
action_58 (9) = happyShift action_7
action_58 (11) = happyShift action_24
action_58 (12) = happyShift action_25
action_58 (13) = happyShift action_26
action_58 (14) = happyShift action_27
action_58 (15) = happyShift action_28
action_58 (16) = happyShift action_8
action_58 (17) = happyShift action_29
action_58 (18) = happyShift action_30
action_58 (19) = happyShift action_31
action_58 (20) = happyShift action_32
action_58 (21) = happyShift action_33
action_58 (22) = happyShift action_9
action_58 (23) = happyShift action_10
action_58 (24) = happyShift action_11
action_58 (25) = happyShift action_12
action_58 (26) = happyShift action_13
action_58 (27) = happyShift action_34
action_58 (28) = happyShift action_14
action_58 (29) = happyShift action_15
action_58 (30) = happyShift action_16
action_58 (4) = happyGoto action_61
action_58 _ = happyFail (happyExpListPerState 58)

action_59 (5) = happyShift action_2
action_59 (6) = happyShift action_4
action_59 (7) = happyShift action_5
action_59 (8) = happyShift action_6
action_59 (9) = happyShift action_7
action_59 (11) = happyShift action_24
action_59 (12) = happyShift action_25
action_59 (13) = happyShift action_26
action_59 (14) = happyShift action_27
action_59 (15) = happyShift action_28
action_59 (16) = happyShift action_8
action_59 (17) = happyShift action_29
action_59 (18) = happyShift action_30
action_59 (19) = happyShift action_31
action_59 (20) = happyShift action_32
action_59 (21) = happyShift action_33
action_59 (22) = happyShift action_9
action_59 (23) = happyShift action_10
action_59 (24) = happyShift action_11
action_59 (25) = happyShift action_12
action_59 (26) = happyShift action_13
action_59 (27) = happyShift action_34
action_59 (28) = happyShift action_14
action_59 (29) = happyShift action_15
action_59 (30) = happyShift action_16
action_59 (4) = happyGoto action_60
action_59 _ = happyFail (happyExpListPerState 59)

action_60 (5) = happyShift action_2
action_60 (6) = happyShift action_4
action_60 (7) = happyShift action_5
action_60 (8) = happyShift action_6
action_60 (9) = happyShift action_7
action_60 (11) = happyShift action_24
action_60 (12) = happyShift action_25
action_60 (13) = happyShift action_26
action_60 (14) = happyShift action_27
action_60 (15) = happyShift action_28
action_60 (16) = happyShift action_8
action_60 (17) = happyShift action_29
action_60 (18) = happyShift action_30
action_60 (19) = happyShift action_31
action_60 (20) = happyShift action_32
action_60 (21) = happyShift action_33
action_60 (22) = happyShift action_9
action_60 (23) = happyShift action_10
action_60 (24) = happyShift action_11
action_60 (25) = happyShift action_12
action_60 (26) = happyShift action_13
action_60 (27) = happyShift action_34
action_60 (28) = happyShift action_14
action_60 (29) = happyShift action_15
action_60 (30) = happyShift action_16
action_60 (4) = happyGoto action_62
action_60 _ = happyFail (happyExpListPerState 60)

action_61 (11) = happyShift action_24
action_61 (12) = happyShift action_25
action_61 (13) = happyShift action_26
action_61 _ = happyReduce_19

action_62 (5) = happyShift action_2
action_62 (6) = happyShift action_4
action_62 (7) = happyShift action_5
action_62 (8) = happyShift action_6
action_62 (9) = happyShift action_7
action_62 (11) = happyShift action_24
action_62 (12) = happyShift action_25
action_62 (13) = happyShift action_26
action_62 (14) = happyShift action_27
action_62 (15) = happyShift action_28
action_62 (16) = happyShift action_8
action_62 (17) = happyShift action_29
action_62 (18) = happyShift action_30
action_62 (19) = happyShift action_31
action_62 (20) = happyShift action_32
action_62 (21) = happyShift action_33
action_62 (22) = happyShift action_9
action_62 (23) = happyShift action_10
action_62 (24) = happyShift action_11
action_62 (25) = happyShift action_12
action_62 (26) = happyShift action_13
action_62 (27) = happyShift action_34
action_62 (28) = happyShift action_14
action_62 (29) = happyShift action_15
action_62 (30) = happyShift action_16
action_62 (4) = happyGoto action_63
action_62 _ = happyFail (happyExpListPerState 62)

action_63 (11) = happyShift action_24
action_63 (12) = happyShift action_25
action_63 (13) = happyShift action_26
action_63 _ = happyReduce_21

happyReduce_1 = happySpecReduce_1  4 happyReduction_1
happyReduction_1 (HappyTerminal (LexHs.TokenBool happy_var_1))
	 =  HappyAbsSyn4
		 (LexHs.ExprBool happy_var_1
	)
happyReduction_1 _  = notHappyAtAll 

happyReduce_2 = happySpecReduce_1  4 happyReduction_2
happyReduction_2 (HappyTerminal (LexHs.TokenVar happy_var_1))
	 =  HappyAbsSyn4
		 (LexHs.ExprVar happy_var_1
	)
happyReduction_2 _  = notHappyAtAll 

happyReduce_3 = happySpecReduce_1  4 happyReduction_3
happyReduction_3 (HappyTerminal (LexHs.TokenDigit happy_var_1))
	 =  HappyAbsSyn4
		 (LexHs.ExprDigit happy_var_1
	)
happyReduction_3 _  = notHappyAtAll 

happyReduce_4 = happySpecReduce_1  4 happyReduction_4
happyReduction_4 (HappyTerminal (LexHs.TokenFun happy_var_1))
	 =  HappyAbsSyn4
		 (LexHs.ExprFun happy_var_1
	)
happyReduction_4 _  = notHappyAtAll 

happyReduce_5 = happySpecReduce_1  4 happyReduction_5
happyReduction_5 _
	 =  HappyAbsSyn4
		 (LexHs.ExprVoid
	)

happyReduce_6 = happySpecReduce_3  4 happyReduction_6
happyReduction_6 _
	(HappyAbsSyn4  happy_var_2)
	_
	 =  HappyAbsSyn4
		 (happy_var_2
	)
happyReduction_6 _ _ _  = notHappyAtAll 

happyReduce_7 = happySpecReduce_3  4 happyReduction_7
happyReduction_7 (HappyAbsSyn4  happy_var_3)
	_
	(HappyAbsSyn4  happy_var_1)
	 =  HappyAbsSyn4
		 (LexHs.ExprAdd happy_var_1 happy_var_3
	)
happyReduction_7 _ _ _  = notHappyAtAll 

happyReduce_8 = happySpecReduce_3  4 happyReduction_8
happyReduction_8 (HappyAbsSyn4  happy_var_3)
	_
	(HappyAbsSyn4  happy_var_1)
	 =  HappyAbsSyn4
		 (LexHs.ExprMul happy_var_1 happy_var_3
	)
happyReduction_8 _ _ _  = notHappyAtAll 

happyReduce_9 = happySpecReduce_3  4 happyReduction_9
happyReduction_9 (HappyAbsSyn4  happy_var_3)
	_
	(HappyAbsSyn4  happy_var_1)
	 =  HappyAbsSyn4
		 (LexHs.ExprSub happy_var_1 happy_var_3
	)
happyReduction_9 _ _ _  = notHappyAtAll 

happyReduce_10 = happySpecReduce_3  4 happyReduction_10
happyReduction_10 (HappyAbsSyn4  happy_var_3)
	_
	(HappyAbsSyn4  happy_var_1)
	 =  HappyAbsSyn4
		 (LexHs.ExprOr happy_var_1 happy_var_3
	)
happyReduction_10 _ _ _  = notHappyAtAll 

happyReduce_11 = happySpecReduce_3  4 happyReduction_11
happyReduction_11 (HappyAbsSyn4  happy_var_3)
	_
	(HappyAbsSyn4  happy_var_1)
	 =  HappyAbsSyn4
		 (LexHs.ExprAnd happy_var_1 happy_var_3
	)
happyReduction_11 _ _ _  = notHappyAtAll 

happyReduce_12 = happySpecReduce_3  4 happyReduction_12
happyReduction_12 (HappyAbsSyn4  happy_var_3)
	_
	(HappyAbsSyn4  happy_var_1)
	 =  HappyAbsSyn4
		 (LexHs.ExprEq happy_var_1 happy_var_3
	)
happyReduction_12 _ _ _  = notHappyAtAll 

happyReduce_13 = happySpecReduce_3  4 happyReduction_13
happyReduction_13 (HappyAbsSyn4  happy_var_3)
	_
	(HappyAbsSyn4  happy_var_1)
	 =  HappyAbsSyn4
		 (LexHs.ExprGt happy_var_1 happy_var_3
	)
happyReduction_13 _ _ _  = notHappyAtAll 

happyReduce_14 = happySpecReduce_3  4 happyReduction_14
happyReduction_14 (HappyAbsSyn4  happy_var_3)
	_
	(HappyAbsSyn4  happy_var_1)
	 =  HappyAbsSyn4
		 (LexHs.ExprLt happy_var_1 happy_var_3
	)
happyReduction_14 _ _ _  = notHappyAtAll 

happyReduce_15 = happySpecReduce_3  4 happyReduction_15
happyReduction_15 (HappyAbsSyn4  happy_var_3)
	_
	(HappyAbsSyn4  happy_var_1)
	 =  HappyAbsSyn4
		 (LexHs.ExprGtE happy_var_1 happy_var_3
	)
happyReduction_15 _ _ _  = notHappyAtAll 

happyReduce_16 = happySpecReduce_3  4 happyReduction_16
happyReduction_16 (HappyAbsSyn4  happy_var_3)
	_
	(HappyAbsSyn4  happy_var_1)
	 =  HappyAbsSyn4
		 (LexHs.ExprLtE happy_var_1 happy_var_3
	)
happyReduction_16 _ _ _  = notHappyAtAll 

happyReduce_17 = happyReduce 4 4 happyReduction_17
happyReduction_17 ((HappyAbsSyn4  happy_var_4) `HappyStk`
	(HappyAbsSyn4  happy_var_3) `HappyStk`
	(HappyAbsSyn4  happy_var_2) `HappyStk`
	_ `HappyStk`
	happyRest)
	 = HappyAbsSyn4
		 (LexHs.ExprIf happy_var_2 happy_var_3 happy_var_4
	) `HappyStk` happyRest

happyReduce_18 = happyReduce 4 4 happyReduction_18
happyReduction_18 ((HappyAbsSyn4  happy_var_4) `HappyStk`
	(HappyAbsSyn4  happy_var_3) `HappyStk`
	(HappyAbsSyn4  happy_var_2) `HappyStk`
	_ `HappyStk`
	happyRest)
	 = HappyAbsSyn4
		 (LexHs.ExprLet happy_var_2 happy_var_3 happy_var_4
	) `HappyStk` happyRest

happyReduce_19 = happyReduce 6 4 happyReduction_19
happyReduction_19 ((HappyAbsSyn4  happy_var_6) `HappyStk`
	(HappyAbsSyn4  happy_var_5) `HappyStk`
	(HappyAbsSyn4  happy_var_4) `HappyStk`
	(HappyAbsSyn4  happy_var_3) `HappyStk`
	(HappyAbsSyn4  happy_var_2) `HappyStk`
	_ `HappyStk`
	happyRest)
	 = HappyAbsSyn4
		 (LexHs.ExprRecFun happy_var_2 happy_var_3 happy_var_4 happy_var_5 happy_var_6
	) `HappyStk` happyRest

happyReduce_20 = happySpecReduce_3  4 happyReduction_20
happyReduction_20 (HappyAbsSyn4  happy_var_3)
	(HappyAbsSyn4  happy_var_2)
	_
	 =  HappyAbsSyn4
		 (LexHs.ExprFunc happy_var_2 happy_var_3
	)
happyReduction_20 _ _ _  = notHappyAtAll 

happyReduce_21 = happyReduce 8 4 happyReduction_21
happyReduction_21 ((HappyAbsSyn4  happy_var_8) `HappyStk`
	(HappyAbsSyn4  happy_var_7) `HappyStk`
	(HappyAbsSyn4  happy_var_6) `HappyStk`
	(HappyAbsSyn4  happy_var_5) `HappyStk`
	(HappyAbsSyn4  happy_var_4) `HappyStk`
	(HappyAbsSyn4  happy_var_3) `HappyStk`
	(HappyAbsSyn4  happy_var_2) `HappyStk`
	_ `HappyStk`
	happyRest)
	 = HappyAbsSyn4
		 (LexHs.ExprRecFun2 happy_var_2 happy_var_3 happy_var_4 happy_var_5 happy_var_6 happy_var_7 happy_var_8
	) `HappyStk` happyRest

happyReduce_22 = happyReduce 4 4 happyReduction_22
happyReduction_22 ((HappyAbsSyn4  happy_var_4) `HappyStk`
	(HappyAbsSyn4  happy_var_3) `HappyStk`
	(HappyAbsSyn4  happy_var_2) `HappyStk`
	_ `HappyStk`
	happyRest)
	 = HappyAbsSyn4
		 (LexHs.ExprFunc2 happy_var_2 happy_var_3 happy_var_4
	) `HappyStk` happyRest

happyReduce_23 = happySpecReduce_1  4 happyReduction_23
happyReduction_23 _
	 =  HappyAbsSyn4
		 (LexHs.Nat
	)

happyReduce_24 = happySpecReduce_1  4 happyReduction_24
happyReduction_24 _
	 =  HappyAbsSyn4
		 (LexHs.Bool
	)

happyReduce_25 = happySpecReduce_3  4 happyReduction_25
happyReduction_25 (HappyAbsSyn4  happy_var_3)
	_
	(HappyAbsSyn4  happy_var_1)
	 =  HappyAbsSyn4
		 (LexHs.Arrow happy_var_1 happy_var_3
	)
happyReduction_25 _ _ _  = notHappyAtAll 

happyNewToken action sts stk [] =
	action 31 31 notHappyAtAll (HappyState action) sts stk []

happyNewToken action sts stk (tk:tks) =
	let cont i = action i i tk (HappyState action) sts stk tks in
	case tk of {
	LexHs.TokenBool happy_dollar_dollar -> cont 5;
	LexHs.TokenVar happy_dollar_dollar -> cont 6;
	LexHs.TokenDigit happy_dollar_dollar -> cont 7;
	LexHs.TokenFun happy_dollar_dollar -> cont 8;
	LexHs.TokenOB -> cont 9;
	LexHs.TokenCB -> cont 10;
	LexHs.TokenSub -> cont 11;
	LexHs.TokenAdd -> cont 12;
	LexHs.TokenMul -> cont 13;
	LexHs.TokenOr -> cont 14;
	LexHs.TokenAnd -> cont 15;
	LexHs.TokenLet -> cont 16;
	LexHs.TokenEq -> cont 17;
	LexHs.TokenGt -> cont 18;
	LexHs.TokenLt -> cont 19;
	LexHs.TokenGtE -> cont 20;
	LexHs.TokenLtE -> cont 21;
	LexHs.TokenIf -> cont 22;
	LexHs.TokenRecFun -> cont 23;
	LexHs.TokenFunc -> cont 24;
	LexHs.TokenRecFun2 -> cont 25;
	LexHs.TokenFunc2 -> cont 26;
	LexHs.TokenTypeArrow -> cont 27;
	LexHs.TokenTypeNat -> cont 28;
	LexHs.TokenTypeBool -> cont 29;
	LexHs.TokenVoid -> cont 30;
	_ -> happyError' ((tk:tks), [])
	}

happyError_ explist 31 tk tks = happyError' (tks, explist)
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

data Type 
        = Nat
        | Boolean
        | Arrow Type Type
        deriving (Show, Eq)
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
