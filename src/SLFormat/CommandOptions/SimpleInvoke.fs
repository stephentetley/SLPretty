// Copyright (c) Stephen Tetley 2019
// License: BSD 3 Clause

// A simple module to run a process.

namespace SLFormat.CommandOptions

// Don't AutoOpen

module SimpleInvoke = 

    open System.IO

    open SLFormat.CommandOptions

    // ************************************************************************
    // Running a process

    /// Running a process 
    /// Return Error(error message) or Ok(exit code)
    let executeProcess (workingDirectory:string option) 
                       (toolPath:string) 
                       (commandOptions:CmdOpt list) : Result<int,string> = 
        try
            use proc = new System.Diagnostics.Process()
            match workingDirectory with
            | Some working -> proc.StartInfo.WorkingDirectory <- working
            | None -> ()
            proc.StartInfo.FileName <- toolPath
            proc.StartInfo.Arguments <- renderCmdOpts commandOptions
            proc.StartInfo.CreateNoWindow <- true
            proc.Start() |> ignore
            proc.WaitForExit () 
            Ok <| proc.ExitCode
        with
        | ex -> Error (sprintf "executeProcess: \n%s" ex.Message)

    type ProcessResult = 
        { ExitCode: int
          StdOut: string }

    /// Running a process 
    /// Return Error(error message) or Ok(exit code & stdout)
    let runProcess (workingDirectory:string option) 
                   (toolPath:string) 
                   (commandOptions:CmdOpt list) : Result<ProcessResult, string> = 
        try
            use proc = new System.Diagnostics.Process()
            proc.StartInfo.FileName <- toolPath
            proc.StartInfo.Arguments <- renderCmdOpts commandOptions
            match workingDirectory with
            | Some working -> proc.StartInfo.WorkingDirectory <- working
            | None -> ()
            proc.StartInfo.UseShellExecute <- false
            proc.StartInfo.RedirectStandardOutput <- true
            proc.Start() |> ignore

            let reader : System.IO.StreamReader = proc.StandardOutput
            let stdout = reader.ReadToEnd()

            proc.WaitForExit ()
            Ok { ExitCode = proc.ExitCode; StdOut = stdout }
        with
        | ex -> Error (sprintf "executeProcess: \n%s" ex.Message)


    /// Very simple process runner.
    /// Fails if the exit code is not 0. 
    /// This may not be a prudent strategy.
    let runProcessSimple (workingDirectory:string option) (toolPath:string) (commandArguments:CmdOpt list) : unit = 
        let args = renderCmdOpts commandArguments
        try
            match executeProcess workingDirectory toolPath commandArguments with
            | Error msg -> failwith msg
            | Ok code when code <> 0 ->
                    failwithf "runProcess fail - error code: %i" code
            | _ -> ()
        with
        | ex -> 
            let diagnosis = 
                String.concat "\n" <| 
                    [ ex.Message
                    ; sprintf "Working Directory: %O" workingDirectory 
                    ; sprintf "Command Args: %s" args
                    ]
            failwithf "runProcess exception: \n%s" diagnosis

  