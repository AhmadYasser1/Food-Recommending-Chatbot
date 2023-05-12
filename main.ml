Random.self_init ();;
open Printf

type rules = 
| Greeting of string
| Question of string
| Hungry of string
| Food_Type of string
| Location of string
| In_Out of string 
| Price_Range of string
| Recommendation of string
| Departure of string
;;

let dictionary = "Dictionary.csv";;

let read_dictionary =
  let dictionary_ic = open_in dictionary in
  let rec loop acc =
    try
      let line = input_line dictionary_ic in
      let fields = String.split_on_char ',' line in
      loop (fields :: acc)
    with
    | End_of_file -> close_in dictionary_ic; List.rev acc
  in
  loop []
;;

let words_lst = read_dictionary;;

let get_string =
  read_line
;;

let greet_user =
  let rand = Random.int 5 in
  match rand with
  | 0 -> "\nHello, how can I help you?\n\n"
  | 1 -> "\nWELCOME HUNGRY, how can I help you?\n\n"
  | 2 -> "\n*rumble rumble* Okay okay, How can I help you?\n\n"
  | 3 -> "\nI'm here I'm here. What can I do for you?\n\n"
  | 4 -> "\nWhy the rush? How can I be of service?\n\n"
  | _ -> "\nHello, how can I help you?as"
;;

let is_char c =
  let character = "abcdefghijklmnopqrstuvwxyz'-" in
  let loc = String.index_opt character c in
  loc <> None
;;

let is_digit c =
  let digits = "0123456789-" in
  let loc = String.index_opt digits c in
  loc <> None
;;

let word_in_lst dictionary_lst word in
  let rec helper dictionary_lst word index =
    
;;


let rec lex_string string = (* create a list of tokens *)
  let len = String.length string in
  let rec lex pos =
    if pos >= len then
    []
  else
    match string.[pos] with
    | ' ' | '\t' | '\n' | '?' -> lex (pos+1)
    | d -> if (is_char d) then 
        let stop = ref pos in
        while !stop < len && is_char (Char.lowercase_ascii string.[!stop]) do
        incr stop;
        done;
        let word = String.lowercase_ascii (String.sub string pos (!stop - pos)) in
        let num = word_in_lst words_lst word in
        match num with
        | 1 -> Greeting (word) :: lex(!stop)
        match word with
        | "hello" | "hey" | "hi" | "heyo" -> Greeting (word) :: lex (!stop)
        | List.filter (x = word) lst
        | "what" -> Question (word) :: lex (!stop)
        | "food" | "eat" | "hungry" | "starving" -> Hungry (word) :: lex (!stop) 
        | "chicken" | "steak" | "shwarema" | "fries" | "feter" | "pizza" |
        "sushi" | "fish" | "wings" | "burger" -> Food_Type (word) :: lex (!stop)
        | "indoor" | "indoors" | "outdoor" | "outdoors" | "take-away" -> In_Out word :: lex (!stop)
        | "recommend" | "recommendations" | "recommendation"-> Recommendation (word) :: lex (!stop)
        | "bye" | "goodbye" | "later" -> Departure (word) :: lex (!stop)
        | _ -> lex (!stop)
      else
        if (is_digit d) then
          let stop = ref pos in
          while !stop < len && is_digit string.[!stop] do
          incr stop;
          done;
          let numstr = String.sub string pos (!stop - pos) in
          let num = int_of_string numstr in
          match num with
          | 0 -> printf "Food isn't free buddy. Food isn't free. Please enter a reasonable amount:\n\n"; get_string () |> lex_string
          | num when num >= 5 && num <= 200 -> Price_Range("$") :: (lex !stop) 
          | num when num >= 201 && num <= 500 -> Price_Range("$$") :: (lex !stop)
          | _ -> lex (!stop)
        else
          lex (pos+1)
  in
  lex 0
;;

let lex_place string =
  match string with
  | s when String.lowercase_ascii s = "first settlement" -> [Location string]
  | s when String.lowercase_ascii s = "rehab" -> [Location string]
  | _ -> []
;;

let rec generate_response lst flist =
  match lst with
  | Question _::t -> generate_response t (flist)
  | Greeting _::_ -> printf "%s" greet_user; generate_response (get_string () |> lex_string) (flist)
  | Hungry _::_ -> printf "\nOkay, What are you in the mood to eat?\n\n"; generate_response (get_string () |> lex_string) (flist)
  | Recommendation _::_ -> printf "\nWell there's burger, fried chicken, sushi, beef, steak, shwarema and koshary, 
  Choose your pick.\n\n"; generate_response (get_string () |> lex_string) (flist)
  | Food_Type w::_ ->  printf "\nOhh nice choice. Okay what's the location where you'll eat?\n\n"; generate_response (get_string () |> lex_place) (flist@[w])
  | Location w::_ -> printf "\nOkay that's a nice place. Do you want to dine (indoors, outdoors or do you want it delivery)?\n\n"; generate_response (get_string () |> lex_string) (flist@[w])
  | In_Out w::_ -> printf "\nI like your picks. Okay now what's ur price range?\n\n"; generate_response (get_string () |> lex_string) (flist@[w])
  | Price_Range w::_ -> printf "\nOooh that's gonna be cutting it. Okay let me process things and I'll get to you in a moment\n\n"; (flist @ [w])
  | Departure _::_ -> printf "\nHope to see you again!!\n\n"; flist
  | [] -> printf "\nI didn't quite understand that. Could you come again please.\n\n"; generate_response (get_string () |> lex_string) (flist)
;;

printf "\nHello User\n
My name is Mr. Hungry a chatbot made to help you choose your next eating expereience.
But first I need to ask you some stuff. Rumbling HAHA Looks like you're dying to get a bite to eat. Let's get into it.
How can i help you???\n\n"

let final = generate_response (get_string () |> lex_string) [];;

let rec print_lex lst =
  match lst with
  | Food_Type w::t -> printf "\nFood type is %s" w; print_lex t
  | Location w::t -> printf "\nThe location is %s" w; print_lex t
  | In_Out w::t -> printf "\nThe type of enviorment is %s" w; print_lex t
  | Price_Range w::t -> printf "\nThe price range is %s" w; print_lex t
  | [] -> ()
  | _::_ -> ()
;;

let restaurants = "Restaurants Dataset.csv";;

let take_until_comma str =
  let len = String.length str in
  let rec loop i =
    if i = len || str.[i] = ',' then
      String.sub str 0 i
    else
      loop (i + 1)
  in
  loop 0
;;

let matchingStrings str substr =
  let len_str = String.length str in
  let len_substr = String.length substr in
  let rec loop i =
    if i + len_substr > len_str then
      false
    else if String.sub str i len_substr = substr then
      true
    else
      loop (i + 1)
  in
  loop 0
;;

let rec findMatch str lst = 
  match lst with
  | [] -> true
  | description::tail -> if (matchingStrings str description = true) 
    then findMatch str tail 
  else false
;;

let take_until_comma str =
  let len = String.length str in
  let rec loop i =
    if i = len || str.[i] = ',' then
      String.sub str 0 i
    else
      loop (i + 1)
  in
  loop 0

let () =

  (* flush and close the channel *)

  (* Read file and display the first line *)
  if final <> [] then
    let ic = open_in restaurants 
      in
        let lineFound = ref "" in 
        let found_match = ref false in
        let matchingRest = ref true
        in 
          while !matchingRest && not !found_match do
            match input_line ic with
            | exception End_of_file -> matchingRest := false
            | line -> begin found_match := findMatch (String.lowercase_ascii line) final; lineFound := line end
          done;
        if !found_match then
            Printf.printf ("My Pick for you is %s") (take_until_comma !lineFound)
        else print_endline "Sorry I don't have a match for what you want";
      (* write the result to stdout *)
      flush stdout;
      (* write on the underlying device now *)
      close_in ic

