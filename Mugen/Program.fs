open System
open System.IO
open Microsoft.FSharp.Compiler.SourceCodeServices
open Microsoft.FSharp.Compiler.Ast
open Fantomas
open Fantomas.FormatConfig

let attrsWithNameSfx (attrs:SynAttributes) name : SynAttributes =
  attrs
  |> List.map (fun attr ->
    let (LongIdentWithDots(id, dotms)) = attr.TypeName
    printfn "id: %A" id
    attr
  )
  
let mutName (lid:LongIdent) =
  let txt = lid.Head.idText
  let rng = lid.Head.idRange
  [Ident(txt + "_Mut", rng)]

let mutRecordMember (mem:SynMemberDefn) : SynMemberDefn =
  printfn "Member: %A" mem
  mem

// Doesn't need to be correct to print an AST
let bogusRange = Microsoft.FSharp.Compiler.Range.range0

let fieldRecName (orig_tpId:LongIdent) (field:SynField) : RecordFieldName =
  let (SynField.Field(id=fldId)) = field  
  let id : LongIdent = orig_tpId @ [fldId.Value]
  let lid = LongIdentWithDots(id, [])
  lid, true

let mkRecExpr (orig_tpId:LongIdent) (origRecFields:SynField list) : SynExpr = 
  let flds : (RecordFieldName * SynExpr option * BlockSeparator option) list =
    origRecFields
    |> List.map (fun f ->
      let (SynField.Field(id=id)) = f
      let that = SynExpr.Ident(Ident("this", bogusRange))
      let lid = LongIdentWithDots([id.Value], [bogusRange])
      let expr = Some <| SynExpr.DotGet(that, bogusRange, lid, bogusRange)
      let recName = fieldRecName orig_tpId f
      recName, expr, None)

  SynExpr.Record(None, None, flds, bogusRange)
  
let mutRecordMembers (orig_tpId:LongIdent) (fields:SynFields) (lid:LongIdent) (mut_lid:LongIdent) (members:SynMemberDefns) : SynMemberDefns =
  let xml = PreXmlDocEmpty
  let memberF = 
    { MemberFlags.IsInstance = true
      MemberFlags.IsDispatchSlot = false
      MemberFlags.IsOverrideOrExplicitImpl = false
      MemberFlags.IsFinal = false
      MemberFlags.MemberKind = MemberKind.Member }
        
  //let argInfo = SynArgInfo.SynArgInfo(SynAttribute)
  let valInfo  = SynValInfo.SynValInfo([], SynArgInfo.SynArgInfo([], false, None))
  let id : LongIdent = [Ident("this", bogusRange); Ident("getSnapshot", bogusRange)]
  let d = SynValData.SynValData(Some(memberF), valInfo, None)
  
  let pat = SynPat.Paren(SynPat.Const(SynConst.Unit, bogusRange), bogusRange)
  let ctorArgs = SynConstructorArgs.Pats([pat])
  let sp = SynPat.LongIdent(LongIdentWithDots(id, []), None, None, ctorArgs, None, bogusRange)

  let recExpr = mkRecExpr orig_tpId fields
  let b = SynBinding.Binding(Some(SynAccess.Public), 
                             SynBindingKind.NormalBinding, 
                             false, false, [], 
                             PreXmlDocEmpty, d, sp, None, 
                             recExpr, bogusRange, NoSequencePointAtInvisibleBinding)

  let m = SynMemberDefn.Member(b, bogusRange)
  [m]
  //members
  

//let attrsWithMutable (attrs:SynAttributes) =
//  attrs |> List.iter (fun a -> printfn "Attr: %A" a)
//  attrs

let (|ListTp|_|) (tpName:SynType) =
  match tpName with
  | SynType.App(a,b,c,d,e,f,g) ->
    match a with
    | SynType.LongIdent(lid) ->
      match lid with
      | LongIdentWithDots(hs,j) ->
        match hs with
        | [] -> None
        | h::s ->
          if h.idText = "list" then
            Some(ListTp)
          else None        
      | _ -> None
    | _ -> None
  | _ -> None

let mutTp (tp:SynType) =
  printfn "\n\n\n%A\n\n\n" tp
  match tp with
  | SynType.App(a,b,c,d,e,f,g) ->
    match a with
    | SynType.LongIdent(lid) ->
      match lid with
      | LongIdentWithDots(hs,j) ->
        match hs with
        | [] -> tp
        | h::s ->
          if h.idText = "list" then
            let newH = Ident("ResizeArray", bogusRange)
            let longId = newH :: s
            let lidots = LongIdentWithDots(longId, j)
            let lid = SynType.LongIdent(lidots)
            SynType.App(lid,b,c,d,e,f,g)
          else tp        
      | _ -> tp
    | _ -> tp
  | _ -> tp

let mutRecordField (field:SynField) :SynField =
  let (SynField.Field(attrs, isStatic, id, typeName, isMut, xmlDoc, accessiblity, range)) = field
  SynField.Field(attrs, isStatic, id, (mutTp typeName), true, xmlDoc, accessiblity, range)
  //field

let mutRecordFields (fields:SynFields) : SynFields =
  fields |> List.map mutRecordField

let processTypeDefnSimpleRepr (ci:SynComponentInfo) (repr:SynTypeDefnSimpleRepr) members _range : SynTypeDefn list =
  match repr with
  //| SynTypeDefnSimpleRepr.Union(acc, cases, range) ->
    
    //[SynTypeDefnSimpleRepr.Union(acc, cases, range)]
  | SynTypeDefnSimpleRepr.Record(acc, fields, range) ->
    let repr = SynTypeDefnRepr.Simple(repr, range)
    let typedef = SynTypeDefn.TypeDefn(ci, repr, members, range)

    let (SynComponentInfo.ComponentInfo(attrs, tps, constraints, lid, xd, preferPostFix, acc, rng)) = ci
    //let attrs2 = attrsWithNameSfx attrs "_Mut"
    let mut_lid = mutName lid 
    let mut_ci = SynComponentInfo.ComponentInfo(attrs, tps, constraints, mut_lid, xd, preferPostFix, acc, rng)
    let mut_members = mutRecordMembers lid fields lid mut_lid members
    let mut_fields = mutRecordFields fields
    let mut_repr = SynTypeDefnSimpleRepr.Record(acc, mut_fields, range)
    let mut_simple = SynTypeDefnRepr.Simple(mut_repr, range)
    //let mut_rec = SynTypeDefnSimpleRepr.Record(acc, mut_fields, range)
    let mut_typedef = SynTypeDefn.TypeDefn(mut_ci, mut_simple, mut_members, range)

    [typedef;mut_typedef]
    //[SynTypeDefnSimpleRepr.Record(acc, fields, range)]
  | x -> 
    let s = SynTypeDefnRepr.Simple(repr, _range)
    let t = SynTypeDefn.TypeDefn(ci, s, members, _range)
    [t]

  //SynTypeDefn.TypeDefn(ci, SynTypeDefnRepr.Simple(repr, _r2), members, _range))

let processTypeDefn (typeDefn:SynTypeDefn) :SynTypeDefn list =
  match typeDefn with
  | SynTypeDefn.TypeDefn(ci, typeDefnR, members, _range) as td ->
    match typeDefnR with
    | SynTypeDefnRepr.Simple(newRepr, _r2) ->
      processTypeDefnSimpleRepr ci newRepr members _r2
//      |> List.map (fun repr ->
//        SynTypeDefn.TypeDefn(ci, SynTypeDefnRepr.Simple(repr, _r2), members, _range))
    | SynTypeDefnRepr.ObjectModel(_) -> [td]

let processTypeDefns (typeDefns: SynTypeDefn list) : SynTypeDefn list = 
  typeDefns |> List.collect processTypeDefn

let processSynModuleDecl decl : SynModuleDecl = 
  match decl with
  | SynModuleDecl.Types(typeDefns, range) ->
    let newTypeDefns = processTypeDefns typeDefns
    SynModuleDecl.Types(newTypeDefns, range)
  | SynModuleDecl.NestedModule(ci, moduleDecls, hi, range) ->
    SynModuleDecl.NestedModule(ci, moduleDecls, hi, range)
  | _ -> decl    

let processSynModuleDecls decls : SynModuleDecls = 
  decls |> List.map processSynModuleDecl

let processSynModuleOrNs (mns:SynModuleOrNamespace) : SynModuleOrNamespace =
  match mns with
  | SynModuleOrNamespace.SynModuleOrNamespace(id, isModule, decls, xmlDoc, attributes, access, range) ->
    SynModuleOrNamespace.SynModuleOrNamespace(id, isModule, (processSynModuleDecls decls), xmlDoc, attributes, access, range)

let processSynModuleOrNss (mnss:SynModuleOrNamespace list) : SynModuleOrNamespace list =
  mnss |> List.map processSynModuleOrNs

let processImplFile (implFile:ParsedImplFileInput) : ParsedImplFileInput =
  match implFile with 
  | ParsedImplFileInput.ParsedImplFileInput(filename, isScript, qualifiedNameOfFile, pragmas, hasDirs,modOrNss, hi) ->
    ParsedImplFileInput.ParsedImplFileInput(filename, isScript, qualifiedNameOfFile, pragmas, hasDirs,(processSynModuleOrNss modOrNss), hi)

let processAst (tree:ParsedInput) : ParsedInput =
  match tree with
  | ParsedInput.ImplFile(f) -> 
    ParsedInput.ImplFile(processImplFile f)
  | x -> failwithf "Wanted ParsedInput.ImplFile, got %A" x

[<EntryPoint>]
let main argv = 
  let file = __SOURCE_DIRECTORY__ + "\DU1.fsx"
  let contents = File.ReadAllText file

  let checker = FSharpChecker.Create()

  let projOptions = 
    checker.GetProjectOptionsFromScript(file, contents)
    |> Async.RunSynchronously

  let ast = 
    CodeFormatter.ParseAsync(file, contents, projOptions, checker)
    |> Async.RunSynchronously

  if CodeFormatter.isValidAST ast then
    printfn "Input:\n------------\n%s\n------------" contents
    let ast2 = processAst ast
    let fmtCfg = FormatConfig.Default
    let src2 = CodeFormatter.formatAST ast2 None fmtCfg
    printfn "Output:\n------------\n%s\n-----------" src2
  else exit 1

  Console.ReadLine() |> ignore
  0 // return an integer exit code
