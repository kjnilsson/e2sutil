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
|> stepsToMidi 1uy 

         
let vox = System.IO.File.ReadAllBytes (__SOURCE_DIRECTORY__ + "/data/025_Vox.e2spat")

let part, one = e2sParse vox |> snd |> List.head

stepsToMidi 0uy one


#endif
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

