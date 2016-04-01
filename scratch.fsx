open System
open System.IO

let data = File.ReadAllBytes (__SOURCE_DIRECTORY__ + "/040_Test.e2spat")

let partOffsets = [0x900 .. 0x330 .. 0x900 + (0x330 * 15)]
let seqOffsets = partOffsets |> List.map ((+) 0x30)
let stepOffsetsRelative = [0 .. 12 .. 12*63] 

type Groove =
    | First of byte

type Part =
    { LastStep: byte
      VoiceAssign: byte
      MotionSeq: byte
      TriggerPadVelocity: byte
      ScaleMode: byte
      Groove: Groove }

type Step =
    { HasNotes: byte
      Gate: byte
      Velocity: byte
      Chord: byte
      Notes: byte list
      Remains: byte[] }

let parseStep (data: byte[]) =
    { HasNotes = data.[0]
      Gate = data.[1] 
      Velocity = data.[2]
      Chord = data.[3]
      Notes = data.[4 .. 7] |> Array.toList
      Remains = data.[8 .. 11] }

let parsePart (data: byte[]) =
    { LastStep = data.[0]
      VoiceAssign = data.[2]
      MotionSeq = data.[4]
      TriggerPadVelocity = data.[5]
      ScaleMode = data.[6]
      Groove = First data.[0x1d] }

partOffsets
|> List.map (fun off -> data.[off .. off + 0x30])    
|> List.map parsePart

let seqs =
    seqOffsets
    |> List.map (fun off -> 
        stepOffsetsRelative |> List.map ((+) off)
        |> List.map (fun o ->
            parseStep data.[o .. o + 11]))
let two  =seqs.[1].[0..15]



(*
 *
	const partOffset = 0x900;
    const partSize = 0x330;
	const sequenceOffset = 0x30;
	this.partSize = 0x30;
	this.sequenceSize = 0x300;
	const stepSize = 12;

	this.parts = [];
    for (var i = 0; i < 16; i++)
    {
		this.parts[i] = new E2Part(this.pattern, partOffset + i * partSize);
    }

	this.sequences = [];
    for (var i = 0; i < 16; i++)
    {
		this.sequences[i] = 
        new E2Sequence(this.pattern, partOffset + i * partSize + sequenceOffset);
    }
    BitConverter.ToUInt16([|0xC0uy;0x03uy|], 0)
    BitConverter.GetBytes(384us) |> Array.rev
    BitConverter.GetBytes(384) |> Array.rev

    *)


let (++) = Array.append

let getBytes (x: int) =
    BitConverter.GetBytes(x) |> Array.rev

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

type MetaEvent =
    | EndOfTrack
    | TimeSig of int * int
    | SetTempo of float32
    
let metaEvent (me: MetaEvent) =
    match me with
    | EndOfTrack -> [|0xFFuy;0x2Fuy;0x00uy|]
    | _ -> failwith "MetaEvent not yet supported"

type MidiNote =
    | Note of byte * byte //note number * velocity

type MidiEvent =
    | NoteOff of MidiNote //1000nnnn
    | NoteOn of MidiNote
    | ControlChange of int * int

type Event =
    | Midi of byte * MidiEvent//channel
    | Meta of MetaEvent

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

type MtrkEvent = int * Event
        
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


[ 0, Midi (0uy, NoteOn (Note (69uy, 96uy)))
  384, Midi (0uy, NoteOff (Note (69uy, 96uy)))
  100, Meta EndOfTrack ]
|> trackChunk



let stepsToMidi channel (steps: Step list) : MtrkEvent list =
    let makeNoteOns delta notes velocity =
        notes
        |> List.filter ((<) 0uy)
        |> List.map (fun n -> (delta,  NoteOn (Note (n, velocity))))
    let makeNoteOffs delta notes velocity =
        notes
        |> List.filter ((<) 0uy)
        |> List.map (fun n -> (delta,  NoteOff (Note (n, velocity)))) 
    let rec inner delta (steps: Step list) (notes: (int * MidiEvent) list) =
        match steps with
        | { Notes = [0uy;0uy;0uy;0uy] } :: rest -> //no notes
                inner (delta + 96) rest notes
        | s :: rest ->
            let ons = makeNoteOns delta s.Notes s.Velocity
            if s.Gate = 255uy then //tie
                //should be first in a tied sequence
                tied (Set.ofList s.Notes) (delta + 96) rest (notes @ ons)
            else
                let offs = makeNoteOffs (delta + int s.Gate) s.Notes s.Velocity
                inner (96 - int s.Gate) rest (notes @ ons @ offs)
        | [] -> notes
    and tied (inFlight: Set<byte>) delta (steps: Step list) (notes: (int * MidiEvent) list) =
        match steps with
        | { Gate = 255uy } as s :: rest when s.Notes = (Set.toList inFlight) -> //do nothing
            tied inFlight (delta + 96) rest notes
        | { Gate = 255uy } as s :: rest -> //notes have changed
            let added = Set.difference (s.Notes |> Set.ofList) inFlight
            let removed = Set.difference inFlight (s.Notes |> Set.ofList)
            let inFlight = (Set.union inFlight added) |> Set.difference removed
            let ons = makeNoteOns delta (Set.toList added) s.Velocity
            let offs = makeNoteOffs delta (Set.toList removed) s.Velocity
            tied inFlight (delta + 96) rest (notes @ ons @ offs)
        | s :: rest -> //no longer tied - create note offs
            let added = Set.difference (s.Notes |> Set.ofList) inFlight |> Set.toList
            let ons = makeNoteOns delta added s.Velocity
            let offs = makeNoteOffs (delta + int s.Gate) s.Notes s.Velocity
            inner 0 rest (notes @ ons @ offs)
        | [] -> notes
    inner 0 steps []
    |> List.map (fun (x, y) -> x, Midi(channel,y))

            
let step notes vel gate =
    { HasNotes = List.length notes |> byte
      Gate = gate
      Velocity = vel
      Notes = notes
      Chord = 0uy
      Remains = Array.empty }


[ step [71uy] 96uy 90uy
  step [69uy] 96uy 255uy
  step [69uy; 68uy] 96uy 255uy
  step [69uy] 96uy 30uy ]
|> stepsToMidi 1uy 

stepsToMidi 1uy two
      


[49uy; 0uy; 0uy; 0uy]
|> List.filter ((<) 0uy)
                
                

(*
<Track Chunk> = <chunk type><length><MTrk event>+
The syntax of an MTrk event is very simple:
<MTrk event> = <delta-time><event>
*)

//tempo and time sig - meta event

//track chunks


(*

ppq = 96 * 4 (4/4) = 384

IDI DOCS
http://sites.uci.edu/camp2014/2014/05/19/timing-in-midi-files/
*)
//MSB
let isSet bit (x: byte) =
    if x = (x ||| (1uy <<< bit)) then true
    else false

let setBit bit (x: byte) =
    x ||| (1uy <<< (7 - bit))

let clearBit bit (x: byte) =
    x ^^^ (1uy <<< (7 - bit))

let bitPrint (x: byte) =
    "00000000"
    |> String.mapi (fun i _ ->
        if isSet (7-i) x then '1'
        else '0')
    |> sprintf "0b%suy"

(*result[3] = (byte)(n & 0x7f);
n >>= 7;  

for (int i = 2; (i >= 0) || (n > 0); i--)
{
    result[i] = ((byte)(n & 0x7f)) | 0x80;
    n >>= 7;
}
*)        
///http://codereview.stackexchange.com/questions/88604/encoding-variable-length-quanties-for-midi



exception ValidationException of string * int
let ex = ValidationException ("blah", 4)

ex.GetBaseException().GetBaseException().
raise (ValidationException "blah") |> ignore

