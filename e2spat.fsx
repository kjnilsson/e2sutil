module E2S

type Groove =
    | First of byte

type Part =
    { Num: int
      LastStep: byte
      VoiceAssign: byte
      MotionSeq: byte
      TriggerPadVelocity: byte
      ScaleMode: byte
      Groove: Groove }
    static member parse i (data: byte[]) =
        { Num = i
          LastStep = data.[0]
          VoiceAssign = data.[2]
          MotionSeq = data.[4]
          TriggerPadVelocity = data.[5]
          ScaleMode = data.[6]
          Groove = First data.[0x1d] }

type Gate =
    | Gate of byte
    | Tie
    static member parse x =
        if x >= 0x7Fuy then Tie
        else Gate x

//a step is 12 bytes
type Step =
    { HasNotes: bool
      Gate: Gate
      Velocity: byte
      Chord: byte
      Notes: byte list
      Remains: byte[] }
    static member parse (data : byte [] ) =
        { HasNotes = data.[0] > 0uy
          Gate = Gate.parse data.[1]
          Velocity = data.[2]
          Chord = data.[3]
          Notes = data.[4 .. 7] |> Array.toList
          Remains = data.[8 .. 11] }

type Pattern =
    { Bpm: float32
      Swing: int
      Length: byte
      Beat: byte
      Key: byte }
    static member parse (data : byte[] ) =
        { Bpm = float32 (uint16 data.[0x123] <<< 8 ||| uint16 data.[0x122]) / 10.f
          Swing = 
            let x = int data.[0x124]
            if  x < 100 then x else x - 256
          Length = data.[0x125]
          Beat = data.[0x126]
          Key = data.[0x127] }

let partOffsets = [0x900 .. 0x330 .. 0x900 + (0x330 * 15)]
let seqOffsets = partOffsets |> List.map ((+) 0x30)
let stepOffsetsRelative = [0 .. 12 .. 12*63] 

let hasNotes (steps : Step list) =
    steps
    |> List.exists (fun { HasNotes = x } -> x)

let e2sParse (data: byte[]) =
    let pat = Pattern.parse data
    let parts =
        partOffsets
        |> List.map (fun off -> data.[off .. off + 0x30])    
        |> List.mapi Part.parse
    let seqs =
        seqOffsets
        |> List.map (fun off -> 
            stepOffsetsRelative |> List.map ((+) off)
            |> List.map (fun o ->
                Step.parse data.[o .. o + 11]))
    pat, List.zip parts seqs

//    |> List.filter (snd >> hasNotes)
        

#if INTERACTIVE
open System
open System.IO
let data = File.ReadAllBytes (__SOURCE_DIRECTORY__ + "/data/040_Test.e2spat")
let vox = File.ReadAllBytes (__SOURCE_DIRECTORY__ + "/data/025_Vox.e2spat")
Pattern.parse vox
Pattern.parse data

e2sParse vox
//|> List.filter (snd >> hasNotes)

#endif