
#load "Scripts\load-references-debug.fsx"
open System
open System.IO
open Microsoft.FSharp.Compiler.SourceCodeServices
open Microsoft.FSharp.Compiler.Ast
open Fantomas
open Fantomas.FormatConfig
open Microsoft.FSharp.Compiler.Range


let printParsedImplFileInput =
  function
  | ParsedImplFileInput (filename, isScript, qualifiedNameOfFile, scopedPragma, parsedHashDirective, synModuleOrNamespace, b) ->
    sprintf "ParsedImplFile (file, %b)" isScript
  
fsi.AddPrinter (printParsedImplFileInput)

let printRange (r: range ) =
  sprintf "(l=%i-%i, c=%i-%i)" r.StartLine r.EndLine r.StartColumn r.EndColumn

fsi.AddPrinter (printRange)