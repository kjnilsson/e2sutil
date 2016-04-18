#if INTERACTIVE
#load "e2spat.fsx"
#load "midi.fsx"
#endif

open E2S
open Midi

///swing is a pre-scaled value between -48 and 48
let stepsToMidi channel swing (steps: Step list) : MtrkEvent list =
    let makeNoteOns delta notes velocity =
        notes
        |> List.filter ((<) 0uy)
        |> List.map (fun n -> (delta,  NoteOn (Note (n, velocity))))
    let makeNoteOffs delta notes velocity =
        notes
        |> List.filter ((<) 0uy)
        |> List.map (fun n -> (delta,  NoteOff (Note (n, velocity)))) 

    let rec loop (inFlight: Set<byte>) offset (steps: Step list) (notes: (int * MidiEvent) list) =
        let withSwing x =
            if List.length steps % 2 = 0 then x 
            else x + swing
        let noteOnOffset = withSwing offset
        let noteOffOffset o =
            min (withSwing o) 96 //truncate noteoff at step end - TODO: is this even correct?
        //TODO: does swing only affect note ons? can the note off get pushed into the next step 
        match steps with
        | { Gate = Tie } as s :: rest when s.Notes = (Set.toList inFlight) -> //do nothing
            loop inFlight (offset + 96) rest notes
        | { Gate = Tie } as s :: rest -> //notes have changed
            let added = Set.difference (s.Notes |> Set.ofList) inFlight
            let removed = Set.difference inFlight (s.Notes |> Set.ofList)
            let inFlight = Set.difference (Set.union inFlight added) removed
            let ons = makeNoteOns noteOnOffset (Set.toList added) s.Velocity
            let offs = makeNoteOffs offset (Set.toList removed) s.Velocity
            loop inFlight (offset + 96) rest (notes @ ons @ offs)
        | { Gate = Gate gate } as s :: rest -> //no longer tied - create note offs
            let added = Set.difference (s.Notes |> Set.ofList) inFlight |> Set.toList
            let removed = Set.difference inFlight (s.Notes |> Set.ofList) |> Set.toList
            let removedOffs = makeNoteOffs offset removed s.Velocity
            let ons = makeNoteOns noteOnOffset added s.Velocity
            let offs = makeNoteOffs (noteOffOffset (offset + int gate)) s.Notes s.Velocity
            loop Set.empty (offset + 96) rest (notes @ removedOffs @ ons @ offs)
        | [] -> notes
    loop Set.empty 0 steps []
    |> List.map (fun (x, y) -> x, Midi(channel,y))
    |> fun l -> List.append l (Midi.trailer (List.length steps * 96)) //make the trailer absolute before making deltas
    |> List.fold (fun (last, agg) (d, m) -> //turn absolute offsets into deltas
        d, (d - last, m) :: agg) (0, [])
    |> snd
    |> List.rev

            
#if INTERACTIVE
let step notes vel gate =
    { HasNotes = List.length notes > 0 
      Gate = gate
      Velocity = vel
      Notes = notes
      Chord = 0uy
      Remains = Array.empty }

[ step [71uy] 96uy (Gate 90uy)
  step [69uy] 96uy Tie
  step [69uy; 68uy] 96uy Tie
  step [69uy] 96uy (Gate 30uy) ]
|> stepsToMidi 1uy 20 

[ step [70uy] 96uy (Gate 90uy)
  step [69uy] 96uy (Gate 96uy) ]
|> stepsToMidi 1uy 20 
         
let vox = System.IO.File.ReadAllBytes (__SOURCE_DIRECTORY__ + "/data/025_Vox.e2spat")

let part, one = e2sParse vox |> snd |> List.head

stepsToMidi 0uy 20 one

#endif

[<EntryPoint>]
let main args =
    match Array.toList args with
    | ["midi"; path] ->
        let fi = System.IO.FileInfo(path)
        let data = System.IO.File.ReadAllBytes fi.FullName
        let fileName = System.IO.Path.GetFileNameWithoutExtension fi.Name
        let pat, parts = e2sParse data
        let tracks = 
            parts 
            |> List.map (fun (part, steps) ->
                stepsToMidi (byte part.Num) 0 steps) //TODO: verify how to read swing value
        let midi = Midi.renderFormat1 tracks
        System.IO.File.WriteAllBytes (fileName + ".MID", midi)
        0
    | _ ->
        printfn "not supported"
        0
        
