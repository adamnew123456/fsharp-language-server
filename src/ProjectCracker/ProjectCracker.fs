module ProjectCracker

open LSP.Log
open System
open System.Diagnostics
open System.IO
open System.Xml
open System.Reflection
open System.Reflection.Metadata
open System.Reflection.PortableExecutable
open System.Collections.Generic
open FSharp.Data
open FSharp.Data.JsonExtensions
open LSP.Json.Ser
open Dotnet.ProjInfo
open Dotnet.ProjInfo.Workspace

// Other points of reference:
// Omnisharp-roslyn cracks .csproj files: https://github.com/OmniSharp/omnisharp-roslyn/blob/master/tests/OmniSharp.MSBuild.Tests/ProjectFileInfoTests.cs
// Roslyn cracks .csproj files: https://github.com/dotnet/roslyn/blob/master/src/Workspaces/MSBuildTest/MSBuildWorkspaceTests.cs

type CrackedProject = {
    /// ?.fsproj file that was cracked
    fsproj: FileInfo
    /// ?.dll file built by this .fsproj file
    /// Dependent projects will reference this dll in fscArgs, like "-r:?.dll"
    target: FileInfo
    /// List of source files.
    /// These are fsc args, but presented separately because that's how FSharpProjectOptions wants them.
    sources: FileInfo list
    /// .fsproj files 
    projectReferences: FileInfo list 
    /// referenced .dlls including from the framework, from projects/packages and direct via <Reference>
    assemblyReferences: FileInfo list 
    /// An error was encountered while cracking the project
    /// This message should be displayed at the top of every file
    error: string option
}

/// Runner for executing dotnet msbuild commands. We don't capture any output
/// since msbuild writes FSC arglists to temp files
let runCommand (workingDirectory: string) (executable: string) (args: string list): int * string =
    let processInfo =
        ProcessStartInfo(FileName = executable,
                         WorkingDirectory = workingDirectory,
                         Arguments = String.concat " " args,
                         CreateNoWindow = true,
                         UseShellExecute = false,
                         RedirectStandardOutput = true,
                         RedirectStandardError = true)

    use proc = new Process(StartInfo = processInfo)
    let outputs = List<string>()
    let writeOutput _ (args: DataReceivedEventArgs) = outputs.Add(args.Data)
    proc.OutputDataReceived.AddHandler(DataReceivedEventHandler writeOutput)
    proc.ErrorDataReceived.AddHandler(DataReceivedEventHandler writeOutput)

    proc.Start() |> ignore
    proc.BeginOutputReadLine()
    proc.BeginErrorReadLine()
    proc.WaitForExit()
    (proc.ExitCode, String.concat "\n" outputs)

/// Runner for msbuild
let runMsbuild (fsproj: string) (args: Inspect.MSBuild.MSbuildCli list) =
    let fsprojDirectory = Path.GetDirectoryName(fsproj)
    let msbuildPath = Inspect.MSBuildExePath.DotnetMsbuild "dotnet"
    Inspect.msbuild msbuildPath (runCommand fsprojDirectory) fsproj args

/// Categorizes the output of the msbuild FSC args task
let processFscArgs(args: string list) =
    let processArg(targetFile, asmReferences, sources) (arg: string) =
        if arg.StartsWith("-r:") then
            (targetFile,
             arg.Substring(3) :: asmReferences,
             sources)
         elif arg.StartsWith("-o:") then
             (arg.Substring(3),
              asmReferences,
              sources)
          elif not (arg.StartsWith("-")) then
              (targetFile,
               asmReferences,
               arg :: sources)
           else
               (targetFile,
                asmReferences,
                sources)

    List.fold processArg ("", [], []) args

// Recursively gets the project references for the given .fsproj
let transitiveProjectReferences(fsproj: string): Result<string list, string> =
    let singleReferences project =
        match Inspect.getProjectInfo ignore runMsbuild Inspect.getP2PRefs [] project with
        | Result.Ok (Inspect.P2PRefs projectReferences) -> 
            Result.Ok (Set.ofSeq projectReferences)

        | Result.Ok _ -> 
            Result.Error (sprintf "dependency %s unexpected result from msbuild runner" project)

        | Result.Error (Inspect.UnexpectedMSBuildResult error) -> 
            Result.Error (sprintf "dependency %s failed with error %s" project error)

        | Result.Error (Inspect.MSBuildFailed (_, output)) -> 
            Result.Error (sprintf "dependency %s failed with output %s" project output)

        | Result.Error Inspect.MSBuildSkippedTarget -> 
            Result.Error (sprintf "dependency %s target to retrieve project refs did not run" project)

    let mutable (error: string option) = None
    let mutable scanned = Set.empty
    let mutable toScan = Set.singleton fsproj
    while not (Set.isEmpty toScan) && Option.isNone error do
        let project = Set.minElement toScan
        toScan <- Set.remove project toScan
        if not (Set.contains project scanned) then
            scanned <- Set.add project scanned

            match singleReferences project with
            | Result.Ok references -> toScan <- Set.union toScan references
            | Result.Error message -> error <- Some message

    match error with
    | None -> Result.Ok (List.ofSeq scanned)
    | Some message -> Result.Error message

/// Crack an .fsproj file by parsing the compiler options that MSBuild
/// generates
let crack(fsproj: FileInfo): CrackedProject =
    let buildFailureResult message =
        let baseName = Path.GetFileNameWithoutExtension(fsproj.Name)
        let dllName = baseName + ".dll"
        let placeholderTarget = FileInfo(Path.Combine [|fsproj.DirectoryName; "bin"; "Debug"; "placeholder"; dllName|])

        dprintfn "Failed to build %s: %s" fsproj.Name message
        {
            fsproj=fsproj
            target=placeholderTarget
            sources=[]
            projectReferences=[]
            assemblyReferences=[]
            error=Some(message)
        }

    // Build and restore the project to ensure that all the dependent assemblies are available.
    runCommand fsproj.DirectoryName "dotnet" ["msbuild"; "/restore"] |> ignore

    match Inspect.getProjectInfo ignore runMsbuild Inspect.getFscArgs [] fsproj.FullName with
    | Result.Ok (Inspect.FscArgs fscArgs) ->
        let (target, asmReferences, sources) = processFscArgs(fscArgs)

        match transitiveProjectReferences fsproj.FullName with
        | Result.Ok projectReferences ->
            {
                fsproj=fsproj
                target=FileInfo(target)
                sources=List.map FileInfo sources
                projectReferences=List.map FileInfo projectReferences
                assemblyReferences=List.map FileInfo asmReferences
                error=None
            }

        | Result.Error message -> buildFailureResult message

    | Result.Ok _ -> buildFailureResult "unexpected result from msbuild runner"
    | Result.Error (Inspect.UnexpectedMSBuildResult error) -> buildFailureResult error
    | Result.Error (Inspect.MSBuildFailed (_, output)) -> buildFailureResult output
    | Result.Error Inspect.MSBuildSkippedTarget -> buildFailureResult "target to retrieve FSC args did not run"
