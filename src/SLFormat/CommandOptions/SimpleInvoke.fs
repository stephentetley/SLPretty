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

    // Running a process 
    let executeProcess (workingDirectory:string) (toolPath:string) (commandOptions:CmdOpt list) : Choice<string,int> = 
        try
            let procInfo = new System.Diagnostics.ProcessStartInfo ()
            procInfo.WorkingDirectory <- workingDirectory
            procInfo.FileName <- toolPath
            procInfo.Arguments <- renderCmdOpts commandOptions
            procInfo.CreateNoWindow <- true
            let proc = new System.Diagnostics.Process()
            proc.StartInfo <- procInfo
            proc.Start() |> ignore
            proc.WaitForExit () 
            Choice2Of2 <| proc.ExitCode
        with
        | ex -> Choice1Of2 (sprintf "executeProcess: \n%s" ex.Message)



    /// Very simple process runner.
    /// Fails if the exit code is not 0. 
    /// This may not be a prudent strategy.
    let runProcess (workingDirectory:string) (toolPath:string) (commandArguments:CmdOpt list) : unit = 
        let args = renderCmdOpts commandArguments
        try
            match executeProcess workingDirectory toolPath commandArguments with
            | Choice1Of2(errMsg) -> failwith errMsg
            | Choice2Of2(code) -> 
                if code <> 0 then
                    failwithf "runProcess fail - error code: %i" code
                else ()
        with
        | ex -> 
            let diagnosis = 
                String.concat "\n" <| 
                    [ ex.Message
                    ; sprintf "Working Directory: %s" workingDirectory 
                    ; sprintf "Command Args: %s" args
                    ]
            failwithf "runProcess exception: \n%s" diagnosis

  