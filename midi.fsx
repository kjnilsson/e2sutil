type MetaEvent =
    | EndOfTrack
    | TimeSig of int * int
    | SetTempo of float32
    
type MidiNote =
    | Note of byte * byte //note number * velocity

type MidiEvent =
    | NoteOff of MidiNote //1000nnnn
    | NoteOn of MidiNote
    | ControlChange of int * int

type Event =
    | Midi of byte * MidiEvent//channel
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
        | _ -> failwith "MidiEvent not yet supported"

    let mtrkEvent delta x =
        makeVar delta ++
            match x with
            | Midi (ch, e) -> midiEvent ch e
            | Meta e -> metaEvent e
            
    let trackChunk (events: MtrkEvent list) =
        let eventData = 
            events
            |> List.fold (fun s (delta, v) ->
                mtrkEvent delta v
                |> Array.append s) Array.empty
        let length = getBytes eventData.Length
        [| yield! "Mtrk"B
           yield! length
           yield! eventData |]

open MidiInternal

#if INTERACTIVE
[ 0, Midi (0uy, NoteOn (Note (69uy, 96uy)))
  384, Midi (0uy, NoteOff (Note (69uy, 96uy)))
  100, Meta EndOfTrack ]
|> trackChunk
#endif
