type Groove =
    | First of byte

type Part =
    { LastStep: byte
      VoiceAssign: byte
      MotionSeq: byte
      TriggerPadVelocity: byte
      ScaleMode: byte
      Groove: Groove }

type Gate =
    | Gate of byte
    | Tie
    static member parse x =
        if x >= 0x7Fuy then Tie
        else Gate x

//a step is 12 bytes
type Step =
    { HasNotes: byte
      Gate: Gate
      Velocity: byte
      Chord: byte
      Notes: byte list
      Remains: byte[] }
    static member parse (data : byte [] ) =
        { HasNotes = data.[0]
          Gate = Gate.parse data.[1]
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


let partOffsets = [0x900 .. 0x330 .. 0x900 + (0x330 * 15)]
let seqOffsets = partOffsets |> List.map ((+) 0x30)
let stepOffsetsRelative = [0 .. 12 .. 12*63] 

#if INTERACTIVE
open System
open System.IO
let data = File.ReadAllBytes (__SOURCE_DIRECTORY__ + "/data/040_Test.e2spat")

partOffsets
|> List.map (fun off -> data.[off .. off + 0x30])    
|> List.map parsePart

let seqs =
    seqOffsets
    |> List.map (fun off -> 
        stepOffsetsRelative |> List.map ((+) off)
        |> List.map (fun o ->
            Step.parse data.[o .. o + 11]))
let two  =seqs.[1].[0..15]

#endif