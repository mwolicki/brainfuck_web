module brainfuck_fable

module Brainfuck =
    type State = { currentIndex : int
                   data : Map<int, byte> }
    type Op = IncPointer | DecPointer | Inc | Dec | Print | Read | While of Op list

    type IOOp = { read : unit -> byte
                  print : byte -> unit }
    with static member def = { read = fun () -> 0uy
                               print = fun i -> () }

    module private BrainfuckEval =

        let evalCode io =
            let rec eval' state ops =
                let currentVal = match state.data.TryFind state.currentIndex with
                                 | Some x -> x
                                 | None -> 0uy
                match ops with
                | IncPointer -> { state with currentIndex = state.currentIndex + 1 }
                | DecPointer -> { state with currentIndex = state.currentIndex - 1 }
                | Inc -> { state with data = state.data.Add (state.currentIndex, currentVal + 1uy) }
                | Dec -> { state with data = state.data.Add (state.currentIndex, currentVal - 1uy) }
                | Print ->
                    currentVal |> io.print
                    state
                | Read ->
                    let v = io.read()
                    { state with data = state.data.Add (state.currentIndex, v) }
                | While ops as x ->
                    if currentVal = 0uy then state
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


open Fable.Core
open Fable.Core.JsInterop
open Fable.Import


let updateResults code =
    let results = Browser.document.getElementById "results"
    let text = ResizeArray<_> ()
    let ioOp = 
        { Brainfuck.IOOp.def with
            read = fun () -> Browser.document?readChar () :?> char |> byte
            print = fun i -> 
                text.Add (char i)
                results.innerText <- text.ToArray() |> System.String
                results.focus ()
                printfn "%s" (text.ToArray() |> System.String) }
    results.innerText <- "(thinking...)"     
    Brainfuck.evalCode ioOp code |> ignore
    results.innerText <- text.ToArray() |> System.String
    

let init() =
    let textarea = Browser.document.getElementsByTagName_textarea().[0]
    updateResults textarea.value
    
    Browser.document.getElementsByTagName_button().[0].addEventListener_click(fun _ -> 
        updateResults textarea.value
        box null)

init()