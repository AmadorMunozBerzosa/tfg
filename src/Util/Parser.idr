||| Small unification-compatible parser combinator library
module Util.Parser

import Data.String
import Data.String.Extra
import Util.Application
import Util.List
import Data.Nat
import Data.Maybe

||| Represents a valid token stream type
public export
interface Parseable a where
    ||| Indicates how many tokens the stream consists of
    size: a -> Nat
    ||| The empty stream 
    empty: a
    ||| Returns True when there are no more tokens to consume
    isEmpty: a -> Bool

public export
Parseable String where
    size = length
    empty = ""
    isEmpty = null

public export
Parseable (List a) where
    size = length
    empty = []
    isEmpty = null

||| The result of a parser, indexed by the type of results
||| it produces and the type of tokens it consumes
public export
data State a b =
      -- Parsing was successful, producing a value and remaining tokens
      Accept a b
      -- Parsing failed
    | Reject

||| A parser is a function that takes a token stream and produces a result
public export
data Parser a b = From (a -> State b a)

||| Calls the parser
public export
run: Parser a b -> a -> State b a
run (From f) a = f a

||| Tries to parse to the end of the token stream
||| If successful, it returns the parsed value
public export
parse: Parseable b => Parser b a -> b -> Maybe a
parse parser tokens = do
    case run parser tokens of
        Accept a val => if isEmpty val then Just a else Nothing
        Reject => Nothing

||| Tries to parse and indicates whether it succeeds.
||| Parsing is successful when it doesn't fail and it
||| consumes the entire token stream
public export
parses: Parseable b => Parser b _ -> b -> Bool
parses parser tokens =
    case run parser tokens of
        Accept _ val => isEmpty val
        Reject => False

public export
Functor (Parser a) where
    map f parser = From (\tokens =>
        case run parser tokens of
            Accept a tokens' => Accept (f a) tokens'
            Reject => Reject
        )
        

public export
Applicative (Parser b) where
    pure a = From (Accept a)

    (f <*> parser) = From (\tokens =>
        case run f tokens of
            Accept f tokens' =>
                case run parser tokens' of
                    Accept a tokens'' => Accept (f a) tokens''
                    Reject => Reject
            Reject => Reject
        )

public export
Monad (Parser b) where
    (>>=) parser f = From (\tokens =>
        case run parser tokens of
            Accept a tokens' => run (f a) tokens'
            Reject => Reject
        )
        
public export
Alternative (Parser b) where
    empty = From (const Reject)

    parser <|> parser' = From (\tokens =>
        case run parser tokens of
            Accept a tokens' => Accept a tokens'
            Reject =>
                case run parser' tokens of
                    Accept a tokens' => Accept a tokens'
                    Reject => Reject
        )

||| Applies a parser. If it succeeds, it applies a
||| function to the produced value. It only succeeds
||| if the return value is `Just`
public export
mapMaybe: Parseable b => (a -> Maybe c) -> Parser b a -> Parser b c
mapMaybe f parser = do
    a <- parser

    case f a of
        Just c => pure c
        Nothing => empty

||| Succeeds for every token and consumes all tokens
public export
anything: Parseable b => Parser b ()
anything = From (\_ => Accept () empty)

||| Always succeeds. Parses True if the stream has no more tokens
public export
emptyStream: Parseable b => Parser b Bool
emptyStream = From (\tokens =>
    if isEmpty tokens then
        Accept True tokens
    else
        Accept False tokens
    )

||| Returns a version of a parser that succeeds if the original succeeds
||| and it consumes tokens
public export
requireConsuming: Parseable b => Parser b a -> Parser b a
requireConsuming parser = From (\tokens => 
    case run parser tokens of
        Accept a tokens' =>
            if size tokens' < size tokens then
                Accept a tokens'
            else
                Reject
        Reject => Reject
    )

||| Parses the empty stream
public export
nothing: Parseable b => Parser b ()
nothing = if !emptyStream then pure () else empty


namespace Combinators
    -- Sequence combinators
    -- .>>. Keeps both values
    -- .>>  Keeps the left value
    --  >>. Keeps the right value
    public export
    infixr 1 .>>., .>>, >>.

    -- Alternative combinators
    -- .||. Keeps whichever value succeeds
    -- .|| Only keeps the left value
    public export
    infixr 4 .<|>.

    ||| Runs two parsers consecutively and keeps both results
    public export
    (.>>.): Monad f => f a -> f b -> f (a,b)
    (.>>.) parserB parserC = do
        b <- parserB
        c <- parserC

        pure (b,c)

    ||| Runs two parsers consecutively and keeps the first result
    public export
    (.>>): Monad f => f a -> f b -> f a
    (.>>) a b = map fst (a .>>. b)

    ||| Runs two parsers consecutively and keeps the second result
    public export
    (>>.): Monad f => f a -> f b -> f b
    (>>.) a b = map snd (a .>>. b)

    -- ||| Runs two parsers consecutively and discards the result
    -- public export
    -- (>>): Monad f => f a -> f b -> f ()
    -- (>>) a b = ignore (a .>>. b)

    ||| Runs a parser. If it doesn't succeed, it tries the second one
    public export
    (.<|>.): Alternative f => f a -> f b -> f (Either a b)
    (.<|>.) a b = map Left a <|> map Right b

    ||| Runs a parser. If it doesn't succeed, it tries the second one.
    ||| It discards the result
    public export
    (||): Alternative f => f a -> f b -> f ()
    (||) a b = ignore (a .<|>. b)

    ||| Parses 0 or 1 occurrences of a given parser
    public export
    optional: Alternative f => f a -> f (Maybe a)
    optional parser = map Just parser <|> pure Nothing

    ||| Parses 0 or more occurrences of a given parser. Always succeeds
    public export
    many: Parseable b => Parser b a -> Parser b (List a)
    many parser = do
        parsed <- (requireConsuming parser .<|>. pure ())
                
        case parsed of
            Left a => do
                list <- many parser
                pure (a::list)
            Right () => pure []

    ||| Parses 1 or more occurrences of a given parser
    public export
    atLeastOne: Parseable b => Parser b a -> Parser b (List a)
    atLeastOne parser =
        parser .>>. many parser |> map (\(a,b) => a :: b)

    ||| Parses a number of items separated by a given separator
    public export
    list: Parseable b => Parser b _ -> Parser b a -> Parser b (List a)
    list separator parser = do        
        if !emptyStream then pure [] else do
        a <- parser

        if !emptyStream then pure [a] else do
        _ <- separator
        
        if !emptyStream then empty else do
        as <- list separator parser
        
        pure (a :: as)


||| Parsers specific to lists of elements
namespace List
    public export
    ||| Parses a token and, if successful, returns the rest
    atomic: (a -> Bool) -> Parser (List a) ()
    atomic pred = From (\case
        [] => Reject
        x::xs => if pred x then Accept () xs else Reject
        )

    ||| Terminal parser. Checks whether all tokens meet a predicate
    public export
    all: (a -> Bool) -> Parser (List a) ()
    all pred = From (\case
        [] => Accept () []
        x::xs => if pred x then run (all pred) xs else Reject
        )

    ||| Parses a given parser, surrounded by tokens that meet a particular condition
    public export
    intermixed: (b -> Bool) -> Parser (List b) a -> Parser (List b) a
    intermixed pred parser = From (\tokens => run parser (filter (pred .> not) tokens)) 

namespace String
    ||| Replaces a parser with a version that produces the tokens the first consumes
    public export
    keepOriginal: Parser String a -> Parser String String
    keepOriginal parser = From (\tokens =>
        case run parser tokens of
            Accept _ remaining =>
                let original = tokens |> dropLast (length remaining) in
                Accept original remaining
            Reject => Reject
        )

    ||| Parses a single character
    public export
    char: Parser String Char
    char = From (\string =>
        case strM string of
            StrNil => Reject
            StrCons char string' => Accept char string'
        )
        

    ||| Parses a string without whitespace between words
    public export
    word: Parser String ()
    word = From (\string =>
        case words string of
            [] => Reject
            word::rest => Accept () (string |> drop (length word))
        )
        
-- 
-- 
||| Parses a string without newlines
public export
singleLine: Parser String ()
singleLine = From (\string => if "\n" `isInfixOf` string then Reject else Accept () "")

||| Parses a string without whitespace between words
public export
singleWord: Parser String ()
singleWord = From (\string =>
        case words string of
        [_] => if trim string == string then Accept () "" else Reject
        _ => Reject
    )

||| Parses an exact ocurrence of a string
public export
literal: String -> Parser String ()
literal string' = From (\string =>
        if string' `isPrefixOf` string then
            Accept () (string |> drop (length string'))
        else
            Reject
    )

||| Parses whitespace characters
public export
whitespace: Parser String ()
whitespace = From (\string => Accept () (ltrim string))

||| Parses a 0-9 digit as a character
public export
digit: Parser String Char
digit =
    char
    |> mapMaybe (\char =>
        case parsePositive (cast char) of
            Just _ => Just char
            Nothing => Nothing
    )

||| Parses a 0-9 digit as a natural number
public export
digitNumber: Parser String Integer
digitNumber =
    char
    |> mapMaybe (\char =>
        case parsePositive (cast char) of
            Just n => Just n
            Nothing => Nothing
    )

||| Parses a natural number
public export
natural: Parser String Integer
natural =
    atLeastOne digit
    |>  map (\chars =>
        case parsePositive (join "" (map cast chars)) of
            Just n => n
            Nothing => 0 -- Should never happen, as each digit is in {'0', ..., '9'}
    )

||| Parses an integer
public export
integer: Parser String Integer
integer =
    optional (literal "-") .>>. natural
    |> map (
        \case
            (Nothing,a) => a
            (Just (), a) => -1 * a 
    )

||| Parses a YYYY year string
public export
year: Parser String Integer
year =
    (atLeastOne digit)
    |> mapMaybe (\digits => 
        if length digits < 4 then
            Nothing
        else
            digits
            |> map cast
            |> join ""
            |> parsePositive
    )

||| Parses a MM year string
public export
month: Parser String Integer
month =
    (digitNumber .>>. digitNumber)
    |>  mapMaybe (\(digit, digit') =>
        let num = digit * 10 + digit' in

        if 1 <= num && num <= 12 then
            Just num
        else
            Nothing
    )

||| Parses a YYYY-MM-DD year string
||| It checks whether the day number is valid for the month and year
public export
date: Parser String (Integer,Integer,Integer)
date =
    (year .>>. literal "-" >>. month .>>. literal "-" >>. digitNumber .>>. digitNumber)
    |> mapMaybe (\(year,month,digit,digit') =>
        let day = digit * 10 + digit'
            maxDay = (
                if month `elem` [1,3,5,7,8,10,12] then
                    31
                else if month `elem` [4,6,9,11] then
                    30
                else if (
                    year `mod` 400 == 0
                    || (year `mod` 4 == 0 && not (year `mod` 100 == 0))
                ) then
                    29
                else
                    28
                )
            in

        if 1 <= day && day <= maxDay then
            Just (year,month,day)
        else
            Nothing
    )

||| It parses a MM/DD day.
||| It assumes the year may be a leap year
public export
yearlessDate: Parser String (Integer,Integer)
yearlessDate =
    (month .>>. literal "-" >>. digitNumber .>>. digitNumber)
    |> mapMaybe (\(month,digit,digit') =>
        let day = digit * 10 + digit'
            maxDay = (
                if month `elem` [1,3,5,7,8,10,12] then
                    31
                else if month `elem` [4,6,9,11] then
                    30
                else
                    29
                )
            in

        if 1 <= day && day <= maxDay then
            Just (month,day)
        else
            Nothing
    )

||| Parses a hh hour string
public export
hour: Parser String Integer
hour = 
    (digitNumber .>>. digitNumber)
    |> mapMaybe (\(h,h') =>
        let hours = 10 * h + h' in
        if 0 <= hours && hours <= 23 then
            Just hours
        else
            Nothing
    )

||| Parses a mm minute string
public export
minute: Parser String Integer
minute = 
    (digitNumber .>>. digitNumber)
    |> mapMaybe (\(m,m') =>
        let minutes = 10 * m + m' in
        if 0 <= minutes && minutes <= 60 then
            Just minutes
        else
            Nothing
    )

||| Parses a ss.ddd second-and-millisecond string
public export
seconds: Parser String Double
seconds = 
    (digitNumber .>>. digitNumber .>>. optional (literal ":" >>. atLeastOne digitNumber))
    |> mapMaybe (\(s,s', decimals) =>
        let minutes = 10 * s + s' in
        if 0 <= minutes && minutes <= 60 then
            case decimals of
                Nothing => Just (cast minutes)
                Just [] => Just (cast minutes)
                Just [d1] => Just (cast minutes + cast d1 / 10)
                Just [d1,d2] => Just (cast minutes + cast d1 / 10 + cast d2 / 100)
                Just [d1,d2,d3] => Just (cast minutes + cast d1 / 10 + cast d2 / 100 + cast d3 / 1000)
                Just _ => Nothing
        else
            Nothing
    )

||| Parses a hh:mm:ss.ddd time string
public export
time: Parser String (Integer,Integer,Maybe Double)
time = (hour .>>. literal ":" >>. minute .>>. optional (literal ":" >>. seconds))

||| Parses a YYYY-MM-DDThh:mm:ss.ddd local datetime string
public export
localDateTime: Parser String ((Integer,Integer,Integer),(Integer,Integer,Maybe Double)) 
localDateTime = (date .>>. (literal "t" || literal " ") >>. time)

||| Parses a 1-53 week index
||| It takes into account which years have 52 or 53 weeks
public export
week: Parser String (Integer,Integer)
week =
    (year .>>. literal "-w" >>. digitNumber .>>. digitNumber)
    |> mapMaybe(\(year,digit,digit') =>
        let num = digit * 10 + digit' in

        if 1 <= num && cast num <= weeks year then
            Just (year,num)
        else
            Nothing
    ) where

    p: Integer -> Integer
    p year = (year `mod` 7) + (year `div` 4) - (year `div` 100) + (year `div` 400)

    weeks: Integer -> Integer
    weeks year = 52 + (if p year == 4 || p (year - 1) == 3 then 1 else 0 )

||| Parses a autofill hint
public export
autoFill: Parser String ()
autoFill =
    optional (a >> literal " ") >>. optional (b >> literal " ") >>. c .>> optional (literal " " >> d)
    
    where
        a: Parser String ()
        a = literal "section-" >> word

        b: Parser String ()
        b = literal "shipping" || literal "billing"
    
        c: Parser String ()
        c = choiceMap literal [
            "name", "honorific-prefix", "given-name", "additional-name", "family-name", "honorific-suffix",
            "nickname", "username", "new-password", "current-password", "one-time-code", "organization-title",
            "organization", "street-address", "address-line1", "address-line2", "address-line3", "address-level4",
            "address-level3", "address-level2", "address-level1", "country", "country-name", "postal-code",
            "cc-name", "cc-given-name", "cc-additional-name", "cc-family-name", "cc-number", "cc-exp",
            "cc-exp-month", "cc-exp-year", "cc-csc", "cc-type", "transaction-currency", "transaction-amount", "language",
            "bday", "bday-day", "bday-month", "bday-year", "sex", "url", "photo"
        ] || (
            optional (choiceMap literal ["home ", "work ", "mobile ", "fax ", "pager "])
            >>.
            choiceMap literal [
                "tel", "tel-country-code", "tel-national", "tel-area-code", "tel-local", "tel-local-prefix",
                "tel-local-suffix", "tel-extension", "email", "impp"
            ]
            )

        d: Parser String ()
        d = literal "webauthn"

||| Parses a list of decimal numbers, together with the separator
public export
decimals: Parser String Double
decimals =
    (literal "." >>. many digitNumber)
    |> map (\digits => go 1 digits)
    
    where
    go: Nat -> List Integer -> Double
    go n [] = 0
    go n (x::xs) = cast x / (pow 10.0 (cast n)) + go (n + 1) xs
    
||| Parses a floating-point number using decimal notation
public export
decimal: Parser String Double
decimal = (
    (integer .>>. optional decimals) -- Integer with decimals
    .<|>.
    decimals -- Just the decimals
    )
    |> map (\case
        Left (integer,Nothing) => cast integer
        Left (integer,Just decimals) => cast integer + decimals
        Right decimals => decimals
    ) 

||| Parses a floating number expressed in exponential notation
public export
double: Parser String Double
double =
    (decimal .>>. optional (literal "e" >>. integer))
    
    |> map (\case
        (decimal, Nothing) => decimal
        (decimal, Just exp) => decimal * pow 10 (cast exp)
    )