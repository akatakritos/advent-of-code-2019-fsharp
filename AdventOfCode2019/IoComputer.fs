module IoComputer

type IoComputerState = AwaitingInput | AwaitingOutput | Finished | Running
type IoComputer(program: string) =
    let mutable computer = IntCodeComputer.loadProgram program
    let mutable status = Running


    let assertRunning () =
        if status = Finished then failwith "Program is completed"

    let assertInput () =
        if status <> AwaitingInput then failwithf "Expected to be awaiting input but was %A" status

    let assertOutput () =
        if status <> AwaitingOutput then failwithf "Expected to be awaiting output but was %A" status


    member this.AdvanceToIo () =
        assertRunning ()

        match IntCodeComputer.advanceToIo computer with
            | IntCodeComputer.PausedForOutput c ->
                computer <- c
                status <- AwaitingOutput
            | IntCodeComputer.PausedForInput c ->
                computer <- c
                status <- AwaitingInput
            | IntCodeComputer.Aborted ->
                status <- Finished

    member this.Receive () =
        this.AdvanceToIo ()
        assertRunning ()
        assertOutput ()

        let (computer', output) = IntCodeComputer.retrieveOutput computer
        computer <- computer'
        output

    member this.Send i =
        this.AdvanceToIo ()
        assertRunning ()
        assertInput ()


        computer <- IntCodeComputer.advanceToIo computer |> IntCodeComputer.ComputerState.unwrapInput
        computer <- IntCodeComputer.provideInput i computer

    member this.SendLine (s: string) =
        for c in s do
            this.Send (int64 c)

        this.Send (int64 '\n')

    member this.ReadLine () =
        this.AdvanceToIo ()
        assertRunning()
        assertOutput()
        let sb = System.Text.StringBuilder()
        while status = AwaitingOutput do
            sb.Append (this.Receive () |> char) |> ignore
            this.AdvanceToIo ()

        sb.ToString()

    member this.IsComplete =
        status = Finished

    member this.Status = status

    member this.Computer = computer

    member this.OverrideMemory address value =
        computer.Memory.[address] <- value

    member this.IsAwaitingInput = status = AwaitingInput