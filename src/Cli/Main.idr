||| Entry point to the CLI
module Cli.Main

import System
import System.File.Handle
import System.File.ReadWrite
import Language.JSON
import Cli.Parsing
import Implementation
import Cli.Show

||| Expresses a file error into a human-readable format
fileErrorMessage: FileError -> String
fileErrorMessage (GenericFileError i) = "While reading the file. Code: \{show i}"
fileErrorMessage FileReadError = "Couldn't read the file"
fileErrorMessage FileWriteError = "Couldn't write to the file"
fileErrorMessage FileNotFound = "File not found"
fileErrorMessage FileExists = "File already exists"
fileErrorMessage PermissionDenied = "Permission denied to the file"

||| Given a tree position, it returns the list of errors for each node it contains
public export
report : Tree Index -> NodePosition -> List (Node, Indexed Error)
report indexes node =
    let index = parallel node indexes |> fromMaybe (0,0) in

    if skipValidation node then
        []
    else
        (map (focus node,index,) (errors' node)) ++ (children node >>= report indexes)


||| Given a HTML node, it validates it and prints the 
reportNode: Tree Index -> Node -> IO Unit
reportNode indexes node = do
    case report indexes (fromTree node) of
        [] => putStrLn "Valid document"
        errors => printLn errors

||| Given a path, it parses the contents of the file
||| in that path into a Node value
readFile: String -> IO Unit
readFile path =
    case !(withFile path Read pure fRead) of
        Left error => putStrLn "Error: \{fileErrorMessage error}"
        Right contents =>
            case JSON.parse contents of
                Nothing => putStrLn "Error: The file is not valid JSON"
                Just json =>
                    case Parsing.parse json of
                        Nothing => putStrLn "Error: The file does not represent a HTML element"
                        Just (indexes,node) => reportNode indexes node

||| Receives a single argument, which is the path of an JSON file
||| in the format the himalaya npm library outputs.
||| It transforms it into HTML and validates it.
||| It outputs a list of validation errors, or "Valid document" if
||| it can't find any
main: IO Unit
main =
    case !getArgs of
        [] => putStrLn "Error: Not enough arguments. Expecting the path of the .json file"
        [_] => putStrLn "Error: Not enough arguments. Expecting the path of the .json file"
        _::_::_::_ => putStrLn "Error: Too many arguments. Expecting 1"
        [_, path] => readFile path