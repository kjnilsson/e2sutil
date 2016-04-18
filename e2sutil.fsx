#if INTERACTIVE
#load "e2spat.fsx"
#load "midi.fsx"
#endif

open E2S
open Midi

let stepsToMidi channel (steps: Step list) : MtrkEvent list =
    let makeNoteOns delta notes velocity =
        notes
        |> List.filter ((<) 0uy)
        |> List.map (fun n -> (delta,  NoteOn (Note (n, velocity))))
    let makeNoteOffs delta notes velocity =
        notes
        |> List.filter ((<) 0uy)
        |> List.map (fun n -> (delta,  NoteOff (Note (n, velocity)))) 

    let rec tied (inFlight: Set<byte>) delta (steps: Step list) (notes: (int * MidiEvent) list) =
        match steps with
        | { Gate = Tie } as s :: rest when s.Notes = (Set.toList inFlight) -> //do nothing
            tied inFlight (delta + 96) rest notes
        | { Gate = Tie } as s :: rest -> //notes have changed
            let added = Set.difference (s.Notes |> Set.ofList) inFlight
            let removed = Set.difference inFlight (s.Notes |> Set.ofList)
            let inFlight = Set.difference (Set.union inFlight added) removed
            let ons = makeNoteOns delta (Set.toList added) s.Velocity
            let offs = makeNoteOffs delta (Set.toList removed) s.Velocity
            tied inFlight (delta + 96) rest (notes @ ons @ offs)
        | { Gate = Gate gate } as s :: rest -> //no longer tied - create note offs
            let added = Set.difference (s.Notes |> Set.ofList) inFlight |> Set.toList
            let removed = Set.difference inFlight (s.Notes |> Set.ofList) |> Set.toList
            let removedOffs = makeNoteOffs delta removed s.Velocity
            let ons = makeNoteOns delta added s.Velocity
            let offs = makeNoteOffs (delta + int gate) s.Notes s.Velocity
            tied Set.empty (delta + 96) rest (notes @ removedOffs @ ons @ offs)
        | [] -> notes
    tied Set.empty 0 steps []
    |> List.map (fun (x, y) -> x, Midi(channel,y))
    |> List.fold (fun (last, agg) (d, m) ->
        d, (d - last, m) :: agg) (0, [])
    |> snd
    |> List.rev
        

            
#if INTERACTIVE
#load "test.fsx"
open Midi
let step notes vel gate =
    { HasNotes = List.length notes > 0 
      Gate = gate
      Velocity = vel
      Notes = notes
      Chord = 0uy
      Remains = Array.empty }

let noteOn c n v =
    Midi(c, NoteOn (Note(n, v)))
let noteOff c n v =
    Midi(c, NoteOff (Note(n, v)))

let test_single_step_with_gate () =
    let expected =
        [ 0, noteOn 0uy 71uy 96uy
          90, noteOff 0uy 71uy 96uy ]
    [ step [71uy] 96uy (Gate 90uy) ]
    |> stepsToMidi 0uy
    |> assertEqual expected

let test_with_tie_then_gate () =
    let expected =
        [ 0, noteOn 0uy 71uy 96uy
          96, noteOn 0uy 69uy 96uy
          96 + 96, noteOff 0uy 69uy 96uy
          96 + 96 + 90, noteOff 0uy 71uy 96uy ]
    [ step [71uy] 96uy Tie
      step [71uy;69uy] 96uy Tie 
      step [71uy] 96uy (Gate 90uy) ]
    |> stepsToMidi 0uy
    |> assertEqual expected

test ["single note with gate", test_single_step_with_gate]
test ["single note with tie + gate", test_with_tie_then_gate]

#endif

#if INTERACTIVE

[ step [71uy] 96uy (Gate 90uy)
  step [69uy] 96uy Tie
  step [69uy; 68uy] 96uy Tie
  step [69uy] 96uy (Gate 30uy) ]
|> stepsToMidi 1uy 

let vox = System.IO.File.ReadAllBytes (__SOURCE_DIRECTORY__ + "/data/025_Vox.e2spat")
let part, one = e2sParse vox |> snd |> List.head
stepsToMidi 0uy one

#endif


[<EntryPoint>]
let main args =
    match Array.toList args with
    | ["midi"; path] ->
        let fi = System.IO.FileInfo(path)
        let data = System.IO.File.ReadAllBytes fi.FullName
        let fileName = fi.Name
        let pat, parts = e2sParse data
        let tracks = 
            parts 
            |> List.map (fun (part, steps) ->
                stepsToMidi (byte part.Num) steps)
        let midi = Midi.renderFormat1 tracks
        System.IO.File.WriteAllBytes (fileName + ".MID", midi)
        0
    | _ ->
        printfn "not supported"
        0
        
