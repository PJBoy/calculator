-- This uses an exact real arithmetic library (unmodified); this file therefore, is focused on parsing and precedence handling

import Data.Char
import CRealI

type NumberSet = CReal;

data Op = None | Add | Subtract | Multiply | Divide | Exponentiate |
          Abs | Log | Sqrt | Factorial |
          Sin | Cos | Tan | Sinh | Cosh | Tanh | Csc | Sec | Cot | Csch | Sech | Coth |
          ArcSin | ArcCos | ArcTan | ArcSinh | ArcCosh | ArcTanh |
          ArcCsc | ArcSec | ArcCot | ArcCsch | ArcSech | ArcCoth deriving(Eq);
data Ass = AssLeft | AssRight deriving(Eq);

do_op :: NumberSet -> Op -> NumberSet -> NumberSet
do_op lhs Add          rhs = lhs + rhs
do_op lhs Subtract     rhs = lhs - rhs
do_op lhs Multiply     rhs = lhs * rhs
do_op lhs Divide       rhs = lhs / rhs
do_op lhs Exponentiate rhs = exp ((log lhs) * rhs)

do_unary_op :: NumberSet -> Op -> NumberSet
do_unary_op n Abs       = abs n
do_unary_op n Log       = log n
do_unary_op n Sqrt      = sqrt n
do_unary_op n Factorial = product [1..n]
do_unary_op n Sin       = sin n
do_unary_op n Cos       = cos n
do_unary_op n Tan       = (sin n) / (cos n)
do_unary_op n Csc       = recip (sin n)
do_unary_op n Sec       = recip (cos n)
do_unary_op n Cot       = (cos n) / (sin n)
do_unary_op n Sinh      = sinh n
do_unary_op n Cosh      = cosh n
do_unary_op n Tanh      = tanh n
do_unary_op n Csch      = recip (sinh n)
do_unary_op n Sech      = recip (cosh n)
do_unary_op n Coth      = recip (tanh n)
do_unary_op n ArcSin    = asin n
do_unary_op n ArcCos    = acos n
do_unary_op n ArcTan    = atan n
do_unary_op n ArcCsc    = asin (recip n)
do_unary_op n ArcSec    = acos (recip n)
do_unary_op n ArcCot    = atan (recip n)
do_unary_op n ArcSinh   = asinh n
do_unary_op n ArcCosh   = acosh n
do_unary_op n ArcTanh   = atanh n
do_unary_op n ArcCsch   = asinh (recip n)
do_unary_op n ArcSech   = acosh (recip n)
do_unary_op n ArcCoth   = atanh (recip n)

precedence :: Op -> Int
precedence None         = 0
precedence Add          = 1
precedence Subtract     = 1
precedence Multiply     = 2
precedence Divide       = 2
precedence Factorial    = 2
precedence Exponentiate = 3

associativity :: Op -> Ass
associativity Add          = AssLeft
associativity Subtract     = AssLeft
associativity Multiply     = AssLeft
associativity Divide       = AssLeft
associativity Factorial    = AssRight
associativity Exponentiate = AssLeft

arity :: Op -> Int
arity Add          = 2
arity Subtract     = 2
arity Multiply     = 2
arity Divide       = 2
arity Factorial    = 1
arity Exponentiate = 2

look_ahead :: String -> Op
look_ahead ""        = None
look_ahead ('+':_)   = Add
look_ahead ('-':_)   = Subtract
look_ahead ('*':_)   = Multiply
look_ahead ('/':_)   = Divide
look_ahead ('!':_)   = Factorial
look_ahead ('^':_)   = Exponentiate
look_ahead (' ':ss)  = look_ahead ss
look_ahead (s:ss)    = error ("Unknown operator "++[s]++" in: "++ss)

next_op :: String -> (String, Op)
next_op ""       = ("", None)
next_op (' ':ss) = next_op ss
next_op (s:ss)   = (ss, look_ahead [s])

pre_op :: String -> (String, Op)
pre_op s
    | map toLower (take 3 s) == "abs"     = (drop 3 s, Abs)
    | map toLower (take 3 s) == "log"     = (drop 3 s, Log)
    | map toLower (take 4 s) == "sqrt"    = (drop 4 s, Sqrt)
    | map toLower (take 4 s) == "sinh"    = (drop 4 s, Sinh)
    | map toLower (take 4 s) == "cosh"    = (drop 4 s, Cosh)
    | map toLower (take 4 s) == "tanh"    = (drop 4 s, Tanh)
    | map toLower (take 4 s) == "csch"    = (drop 4 s, Csch)
    | map toLower (take 4 s) == "sech"    = (drop 4 s, Sech)
    | map toLower (take 4 s) == "coth"    = (drop 4 s, Coth)
    | map toLower (take 3 s) == "sin"     = (drop 3 s, Sin)
    | map toLower (take 3 s) == "cos"     = (drop 3 s, Cos)
    | map toLower (take 3 s) == "tan"     = (drop 3 s, Tan)
    | map toLower (take 3 s) == "csc"     = (drop 3 s, Csc)
    | map toLower (take 3 s) == "sec"     = (drop 3 s, Sec)
    | map toLower (take 3 s) == "cot"     = (drop 3 s, Cot)
    | map toLower (take 7 s) == "arcsinh" = (drop 7 s, ArcSinh)
    | map toLower (take 7 s) == "arccosh" = (drop 7 s, ArcCosh)
    | map toLower (take 7 s) == "arctanh" = (drop 7 s, ArcTanh)
    | map toLower (take 7 s) == "arccsch" = (drop 7 s, ArcCsch)
    | map toLower (take 7 s) == "arcsech" = (drop 7 s, ArcSech)
    | map toLower (take 7 s) == "arccoth" = (drop 7 s, ArcCoth)
    | map toLower (take 6 s) == "arcsin"  = (drop 6 s, ArcSin)
    | map toLower (take 6 s) == "arccos"  = (drop 6 s, ArcCos)
    | map toLower (take 6 s) == "arctan"  = (drop 6 s, ArcTan)
    | map toLower (take 6 s) == "arccsc"  = (drop 6 s, ArcCsc)
    | map toLower (take 6 s) == "arcsec"  = (drop 6 s, ArcSec)
    | map toLower (take 6 s) == "arccot"  = (drop 6 s, ArcCot)
    | otherwise                           = (s, None)

constant :: String -> (String, NumberSet)
constant s
    | head s == 'e'                   = (tail s, exp 1)
    | take 2 s == "pi"                = (drop 2 s, pi)
    | take 3 s == "phi"               = (drop 3 s, (1+(sqrt 5)) / 2)
    | map toLower (take 3 s) == "deg" = (drop 3 s, 180/pi)
    | otherwise                       = (s, 0)

decimals :: String -> NumberSet -> NumberSet -> (String, NumberSet)
decimals "" num _     = ("", num)
decimals (s:ss) num n = if isDigit s then decimals ss (num + (read [s])/n) (n*10) else (s:ss, num)

number :: String -> NumberSet -> (String, NumberSet)
number "" num       = ("", num)
number ('.':ss) num = decimals ss num 10
number (s:ss) num   = if isDigit s then number ss (num*10+(read [s])) else (s:ss, num)

numeral :: String -> NumberSet -> (String, NumberSet)
numeral "" num = ("", num)
numeral s  num
    | take 2 s == "IV" = numeral (drop 2 s) (num+4)
    | take 2 s == "IX" = numeral (drop 2 s) (num+9)
    | take 2 s == "XL" = numeral (drop 2 s) (num+40)
    | take 2 s == "XC" = numeral (drop 2 s) (num+90)
    | take 2 s == "CD" = numeral (drop 2 s) (num+400)
    | take 2 s == "CM" = numeral (drop 2 s) (num+900)
    | head s == 'I'    = numeral (tail s) (num+1)
    | head s == 'V'    = numeral (tail s) (num+5)
    | head s == 'X'    = numeral (tail s) (num+10)
    | head s == 'L'    = numeral (tail s) (num+50)
    | head s == 'C'    = numeral (tail s) (num+100)
    | head s == 'D'    = numeral (tail s) (num+500)
    | head s == 'M'    = numeral (tail s) (num+1000)
    | otherwise        = (s, num)

sub_expression_r :: String -> String -> Int -> (String, String)
sub_expression_r ""       sub depth = error ("Mismatched parentheses ("++(show depth)++" unclosed): ("++sub)
sub_expression_r (')':ss) sub 0     = (ss, sub)
sub_expression_r (')':ss) sub depth = sub_expression_r ss (sub++")") (depth-1)
sub_expression_r ('(':ss) sub depth = sub_expression_r ss (sub++"(") (depth+1)
sub_expression_r (s:ss)   sub depth = sub_expression_r ss (sub++[s]) depth

sub_expression_e :: (String, String) -> (String, NumberSet)
sub_expression_e (s, sub) = (s, expression sub)

sub_expression :: String -> (String, NumberSet)
sub_expression s = sub_expression_e (sub_expression_r s "" 0)

primitive :: String -> (String, NumberSet)
primitive ""       = error "Incomplete input"
primitive (')':ss) = error ("Mismatched parentheses, the closing parenthesis starting: )"++ss)
primitive (' ':ss) = primitive ss
primitive ('-':ss) = let (rs, n) = primitive ss in (rs, -n)
primitive ('(':ss) = sub_expression ss
primitive (s:ss)   = 
    if isDigit s then
        number (s:ss) 0
    else let (s2, op) = pre_op (s:ss) in
            if op /= None then let (s3, n) = primitive s2 in
                    (s3, do_unary_op n op)
            else let (s3, num) = constant (s:ss) in
                if num /= 0 then (s3, num)
                else let (s4, num2) = numeral (s:ss) 0 in
                    if num2 /= 0 then (s4, num2)
                    else error ("Unrecognised symbol '"++[s]++"' in: "++(s:ss))

expression_r2 :: String -> NumberSet -> NumberSet -> Int -> Op -> Op -> (String, NumberSet)
expression_r2 s lhs rhs p op op2 =
    if (precedence op2)>(precedence op) ||
        (precedence op2)==(precedence op) && (associativity op2)==AssRight then
        let (s2, rhs2) = expression_r (s, rhs) (precedence op2) in
            expression_r2 s2 lhs rhs2 p op (look_ahead s2)
    else expression_r (s, (do_op lhs op rhs)) p

expression_r :: (String, NumberSet) -> Int -> (String, NumberSet)
expression_r (s, lhs) p =
    let (s2, op) = next_op s in
        if (precedence op) >= p then
            if (arity op) /= 1 then
                let (s3, rhs) = primitive s2 in
                    expression_r2 s3 lhs rhs p op (look_ahead s3)
            else expression_r (s2, do_unary_op lhs op) p
        else (s2, lhs)

expression :: String -> NumberSet
expression s = snd (expression_r (primitive s) 1)

main :: IO()
main = do
{
    putStr "> ";
    s <- getLine;
    putStrLn (show (expression s));
    main;
}