module Midi

type MetaEvent =
    | EndOfTrack
    | TimeSig of int * int
    | SetTempo of float32
    
type MidiNote =
    | Note of note: byte * velocity: byte //note number * velocity

type MidiEvent =
    | NoteOff of MidiNote //1000nnnn
    | NoteOn of MidiNote
    | ControlChange of num: byte * value: byte

type Event =
    | Midi of channel: byte * MidiEvent
    | Meta of MetaEvent

type MtrkEvent = int * Event

module MidiInternal =
    let (++) = Array.append

    let getBytes (x: int) =
        System.BitConverter.GetBytes(x) |> Array.rev

    let makeVar (x: int) =
        let mutable n = x
        let result = Array.zeroCreate<byte> 4
        result.[3] <- (byte n) &&& 0x7Fuy
        n <- n >>> 7
        let mutable c = 2
        while (c >= 0 || n > 0) do
            result.[c] <- ((byte n) &&& 0x7Fuy) ||| 0x80uy
            n <- n >>> 7
            c <- c - 1
        [| for r in result do
            if r <> 0x80uy then yield r |]

    let format0Header =
        "MThd"B ++ 
        [|0uy;0uy;0uy;6uy|] ++ //length (constant)
        [|0uy; 0uy|] ++ //format = midi 0
        [|0uy; 1uy|] ++ //ntracks = for format 0 this is always 1
        [|1uy; 128uy|] //ticks per quarter note = 384 (96 * 4) based on e2s' observable resolution
                        (*bits 14 thru 0 represent the number of delta time "ticks" which make up a
                    quarter-note. For instance, if division is 96, then a time interval of an eighth-note between two events in the
                    file would be 48.*)

    let format1Header ntrks  =
        "MThd"B ++ 
        [|0uy;0uy;0uy;6uy|] ++ //length (constant)
        [|0uy; 1uy|] ++ //format 
        [|0uy; ntrks|] ++ //ntracks 
        [|1uy; 128uy|] //ticks per quarter note = 384 (96 * 4) based on e2s' observable resolution

    let metaEvent (me: MetaEvent) =
        match me with
        | EndOfTrack -> [|0xFFuy;0x2Fuy;0x00uy|]
        | _ -> failwith "MetaEvent not yet supported"

    let midiEvent channel (me: MidiEvent) =
        let ch = channel &&& 0b00001111uy
        match me with
        | NoteOff (Note (note, vel)) ->
            [|0b10000000uy ||| ch; note &&& 0x7Fuy; vel &&& 0x7Fuy |] 
        | NoteOn (Note (note, vel)) ->
            [| 0b10010000uy ||| ch; note &&& 0x7Fuy; vel &&& 0x7Fuy |]
        | ControlChange (num, value) ->
            [| 0b10110000uy ||| ch; num &&& 0x7Fuy; value &&& 0x7Fuy |]

    let mtrkEvent delta x =
        makeVar delta ++
            match x with
            | Midi (ch, e) -> midiEvent ch e
            | Meta e -> metaEvent e
            
    let trackChunks (events: MtrkEvent list) =
        let eventData =
            events
            |> List.fold (fun s (delta, v) ->
                mtrkEvent delta v
                |> Array.append s) Array.empty
        let length = getBytes eventData.Length
        [| yield! "MTrk"B
           yield! length
           yield! eventData |]

let trailer delta =
  [ delta, Midi (0uy, ControlChange (0x7Buy, 0uy)) //all notes off
    delta, Meta EndOfTrack ]
///renders a format 0 midi file from a list of MtrkEvents
///with a ticks per quarter note of 384 (96 * 4)
let renderFormat0 (events: MtrkEvent list) =
    [| yield! MidiInternal.format0Header
       yield! MidiInternal.trackChunks events |]

let renderFormat1 (tracks: MtrkEvent list list) =
    let len = List.length tracks
    [|  yield! MidiInternal.format1Header (byte len)
        for events in tracks do
           yield! MidiInternal.trackChunks events |]

//http://www.skytopia.com/project/articles/midi.html
#if INTERACTIVE
open MidiInternal
[ 0, Midi (0uy, NoteOn (Note (69uy, 96uy)))
  384, Midi (0uy, NoteOff (Note (69uy, 0uy)))
  0, Midi (0uy, ControlChange (0x7Buy, 0uy)) //all notes off
  0, Meta EndOfTrack ]
|> renderFormat0
|> fun data -> System.IO.File.WriteAllBytes(@"c:\\dump\test.mid", data)
#endif

#if INTERACTIVE
#load "test.fsx"

test
    [ "trackChunks should return empty MTrk", fun () -> 
        trackChunks [] |> assertEqual ("MTrk"B ++ [|0uy;0uy;0uy;0uy|])
      "second", fun () -> assertEqual 1 1 ]

#endif


#if INTERACTIVE

open System.IO

let files =
    DirectoryInfo(__SOURCE_DIRECTORY__).GetFiles("*.fsx")
    |> Seq.filter (fun f ->
        let text = File.ReadAllText f.FullName
        text.Contains("test.fsx"))
    |> Seq.toList

[ for f in files do
    printfn "***Running test in file: %s ***" f.Name
    //shell out to fsi 
    ]
        
#endif
