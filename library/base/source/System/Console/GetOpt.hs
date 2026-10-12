module System.Console.GetOpt where

import Data.List (intercalate, isPrefixOf)

data ArgOrder a
  = RequireOrder
  | Permute
  | ReturnInOrder (String -> a)

data OptDescr a = Option [Char] [String] (ArgDescr a) String

data ArgDescr a
  = NoArg a
  | ReqArg (String -> a) String
  | OptArg (Maybe String -> a) String

-- What one argument of a command line turns out to be.
data Found a
  = Opt a
  | UnreqOpt String
  | NonOpt String
  | EndOfOpts
  | OptErr String

getOpt :: ArgOrder a -> [OptDescr a] -> [String] -> ([a], [String], [String])
getOpt ordering descriptions arguments = (options, others, errors ++ map unrecognised unknown)
  where
    (options, others, unknown, errors) = getOpt' ordering descriptions arguments
    unrecognised name = "unrecognized option `" ++ name ++ "'\n"

getOpt' :: ArgOrder a -> [OptDescr a] -> [String] -> ([a], [String], [String], [String])
getOpt' _ _ [] = ([], [], [], [])
getOpt' ordering descriptions (argument : arguments) = case found of
  Opt option -> (option : options, others, unknown, errors)
  UnreqOpt name -> (options, others, name : unknown, errors)
  NonOpt other -> case ordering of
    RequireOrder -> ([], other : rest, [], [])
    Permute -> (options, other : others, unknown, errors)
    ReturnInOrder f -> (f other : options, others, unknown, errors)
  EndOfOpts -> case ordering of
    RequireOrder -> ([], rest, [], [])
    Permute -> ([], rest, [], [])
    ReturnInOrder f -> (map f rest, [], [], [])
  OptErr message -> (options, others, unknown, message : errors)
  where
    (found, rest) = next argument arguments descriptions
    (options, others, unknown, errors) = getOpt' ordering descriptions rest

next :: String -> [String] -> [OptDescr a] -> (Found a, [String])
next ('-' : '-' : []) rest _ = (EndOfOpts, rest)
next ('-' : '-' : name) rest descriptions = long name rest descriptions
next ('-' : letter : letters) rest descriptions = short letter letters rest descriptions
next other rest _ = (NonOpt other, rest)

long :: String -> [String] -> [OptDescr a] -> (Found a, [String])
long text rest descriptions = go matching argument rest
  where
    (name, argument) = break (== '=') text
    exact = [kind | Option _ names kind _ <- descriptions, name `elem` names]
    starting = [kind | Option _ names kind _ <- descriptions, any (isPrefixOf name) names]
    matching = if null exact then starting else exact
    option = "--" ++ name
    go [NoArg a] [] others = (Opt a, others)
    go [NoArg _] ('=' : _) others = (OptErr ("option `" ++ option ++ "' doesn't allow an argument\n"), others)
    go [ReqArg _ what] [] [] = (OptErr ("option `" ++ option ++ "' requires an argument " ++ what ++ "\n"), [])
    go [ReqArg f _] [] (value : others) = (Opt (f value), others)
    go [ReqArg f _] ('=' : value) others = (Opt (f value), others)
    go [OptArg f _] [] others = (Opt (f Nothing), others)
    go [OptArg f _] ('=' : value) others = (Opt (f (Just value)), others)
    go [] _ others = (UnreqOpt ("--" ++ text), others)
    go _ _ others = (OptErr ("option `" ++ option ++ "' is ambiguous\n"), others)

short :: Char -> String -> [String] -> [OptDescr a] -> (Found a, [String])
short letter letters rest descriptions = go matching letters rest
  where
    matching = [kind | Option known _ kind _ <- descriptions, letter `elem` known]
    option = ['-', letter]
    go [NoArg a] [] others = (Opt a, others)
    go [NoArg a] more others = (Opt a, ('-' : more) : others)
    go [ReqArg _ what] [] [] = (OptErr ("option `" ++ option ++ "' requires an argument " ++ what ++ "\n"), [])
    go [ReqArg f _] [] (value : others) = (Opt (f value), others)
    go [ReqArg f _] value others = (Opt (f value), others)
    go [OptArg f _] [] others = (Opt (f Nothing), others)
    go [OptArg f _] value others = (Opt (f (Just value)), others)
    go [] [] others = (UnreqOpt option, others)
    go [] more others = (UnreqOpt option, ('-' : more) : others)
    go _ _ others = (OptErr ("option `" ++ option ++ "' is ambiguous\n"), others)

usageInfo :: String -> [OptDescr a] -> String
usageInfo header descriptions = unlines (header : table)
  where
    (shorts, longs, texts) = unzip3 (concatMap describe descriptions)
    table = zipWith3 paste (flush shorts) (flush longs) texts
    paste x y z = "  " ++ x ++ "  " ++ y ++ "  " ++ z
    flush column = map (\x -> take (maximum (map length column)) (x ++ repeat ' ')) column

describe :: OptDescr a -> [(String, String, String)]
describe (Option letters names kind text) = case lines text of
  [] -> [(shorts, longs, "")]
  first : others -> (shorts, longs, first) : [("", "", other) | other <- others]
  where
    shorts = intercalate ", " (map shown letters)
    longs = intercalate ", " (map named names)
    shown letter = case kind of
      NoArg _ -> ['-', letter]
      ReqArg _ what -> ['-', letter] ++ " " ++ what
      OptArg _ what -> ['-', letter] ++ "[" ++ what ++ "]"
    named name = case kind of
      NoArg _ -> "--" ++ name
      ReqArg _ what -> "--" ++ name ++ "=" ++ what
      OptArg _ what -> "--" ++ name ++ "[=" ++ what ++ "]"
