

type pitch = A | B | C | D | E | F | G


type accidental = Flat | Natural | Sharp


type song =
| Note of pitch * accidental * song
| End



let a = A 


let blop = Natural

let song = Note(a,blop, End)



let string_of_pitch pitch = match pitch with 
| A -> "A"
| B -> "B"
| C -> "C"
| D -> "D"
| E -> "E"
| F -> "F"
| G -> "G"


let string_of_accidental acc = match acc with
| Flat -> "b"
| Natural -> ""
| Sharp -> "#"


let _ = print_endline (string_of_pitch A ^ string_of_accidental Sharp)

let string_of_song song = match song with
| Note(p,a,s) ->
    string_of_pitch p ^ string_of_accidental a ^ " " string_of_song s
| End -> ""


let _ = print_endline (string_of_song (
  Note (A, Sharp, Note(B, Natural, Note(D, Flat, End)))
)

type list =
| Cons of int * list 
| Nil



type song = (pitch * accidental) list
