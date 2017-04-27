module brainfuck_fable

module Brainfuck =
    type State = { currentIndex : int
                   data : Map<int, int> }
    type Op = IncPointer | DecPointer | Inc | Dec | Print | Read | While of Op list

    type IOOp = { read : unit -> int
                  print : int -> unit }
    with static member def = { read = fun () -> 0
                               print = fun i -> () }

    module private BrainfuckEval =

        let evalCode io =
            let rec eval' state ops =
                let currentVal = match state.data.TryFind state.currentIndex with
                                 | Some x -> x
                                 | None -> 0
                match ops with
                | IncPointer -> { state with currentIndex = state.currentIndex + 1 }
                | DecPointer -> { state with currentIndex = state.currentIndex - 1 }
                | Inc -> { state with data = state.data.Add (state.currentIndex, (currentVal + 1) % 256) }
                | Dec -> { state with data = state.data.Add (state.currentIndex, (currentVal - 1) % 256) }
                | Print ->
                    currentVal |> io.print
                    state
                | Read ->
                    let v = io.read() % 256
                    { state with data = state.data.Add (state.currentIndex, v) }
                | While ops as x ->
                    if currentVal = 0 then state
                    else
                        let state = ops |> List.fold eval' state
                        eval' state x   
            List.fold eval' { currentIndex = 0; data = Map.empty }

        
    module private Parser =
        let allowedSymbols = set ['>'; '<'; '+'; '-'; '.'; ','; '['; ']']

        let (|IsStmt|_|) str = 
            let (|IsChar|_|) = function [] -> None | ch::_ -> Some ch
            
            match str with
            | IsChar '>' -> Some IncPointer
            | IsChar '<' -> Some DecPointer
            | IsChar '+' -> Some Inc
            | IsChar '-' -> Some Dec
            | IsChar '.' -> Some Print
            | IsChar ',' -> Some Read
            | _ -> None
            |> Option.map(fun x->x, List.tail str)

        let rec (|Stmts|) (str:char list)  =
            let rec getStmts curr = function
            | (IsParsable (op, remaining)) -> getStmts (op :: curr) remaining
            | remaining -> curr, remaining
            getStmts [] str 
        
        and (|IsWhileLoop|_|) = function
        | '[' :: Stmts (stms, ']' :: xs)  ->
            Some (stms |> List.rev |> While, xs)
        | _ -> None

        and (|IsParsable|_|) = function
        | IsWhileLoop (op, xs)
        | IsStmt (op, xs) -> (op, xs) |> Some
        | _ -> None

        let rec parse' = function
        | [] -> []
        | IsParsable (op, xs) -> op :: parse' xs
        | x -> failwithf "failed parsing %A" x

        let parse s = s |> Seq.filter allowedSymbols.Contains |> List.ofSeq |> parse'

    let evalCode op : string -> State =  Parser.parse >> BrainfuckEval.evalCode op

    let codeToText code =
        let ast = Parser.parse code
        let rec toDisplay tabs ast = 
            let tab = Array.init tabs (fun _ -> '\t') |> System.String
            match ast with
            | [] -> ""
            | IncPointer :: xs -> sprintf "%spointer++;\n" tab + toDisplay tabs xs
            | DecPointer :: xs -> sprintf "%spointer--;\n" tab + toDisplay tabs xs
            | Inc :: xs -> sprintf "%smemory[pointer]++;\n" tab + toDisplay tabs xs
            | Dec  :: xs -> sprintf "%smemory[pointer]--;\n" tab + toDisplay tabs xs
            | Print :: xs -> sprintf "%sSystem.Console.Write((char) memory[pointer]);\n" tab + toDisplay tabs xs
            | Read :: xs -> sprintf "%smemory[pointer] = (byte)(char) System.Console.Read();\n" tab + toDisplay tabs xs
            | While ops :: xs -> 
                sprintf "%swhile(memory[pointer]!=0) {\n%s%s}\n" tab (toDisplay (tabs + 1) ops) tab  + toDisplay tabs xs
        "var memory = new byte[1024];\nvar pointer = 0;\n" + toDisplay 0 ast

open Fable.Core
open Fable.Core.JsInterop
open Fable.Import


let updateResults code =
    let results = Browser.document.getElementById "results"
    let text = ResizeArray<_> ()
    let ioOp = 
        { Brainfuck.IOOp.def with
            read = fun () -> Browser.document?readChar () :?> char |> int
            print = fun i -> 
                text.Add (char i)
                results.innerText <- text.ToArray() |> System.String }
    results.innerText <- "(thinking...)"     
    Brainfuck.evalCode ioOp code |> ignore
    results.innerText <- text.ToArray() |> System.String
    
    
let refreashCSharpCode code =
    let codeDiv = Browser.document.getElementById "code"
    codeDiv.innerText <- (Brainfuck.codeToText code)


let init() =
    let textarea = Browser.document.getElementsByTagName_textarea().[0]
    updateResults textarea.value
    refreashCSharpCode textarea.value

    textarea.addEventListener_change(fun _ ->
        refreashCSharpCode textarea.value
        box null)
    
    Browser.document.getElementsByTagName_button().[0].addEventListener_click(fun _ -> 
        updateResults textarea.value
        box null)

init()