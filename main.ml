Random.self_init ();;
open Printf

type rules = 
| Greeting of string
| Question of string
| Hungry of string
| Food_Type of string
| In_Out of string 
| Recommendation of string
| Departure of string
| Delivery of string
| Location of string
| Price_Range of string
;;

let get_string =
  read_line
;;

let greet_user_response =
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

let dictionary_file = "Dictionary.csv";;

let read_dictionary_csv =
  let dictionary_ic = open_in dictionary_file in
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

let string_to_clst word = 
  String.fold_left(fun acc x -> acc @ [x]) [] word
;;

let remove_duplicates clst =
  let rec helper clst acc =
  match clst with
  | [] -> acc
  | h::t -> if List.mem h acc then helper (t) (acc) else helper (t) (h::acc)
  in 
  helper clst []
;;

let final_string word = 
  List.fold_left (fun acc x -> acc ^ (String.make 1 x)) "" (List.rev (remove_duplicates (string_to_clst word)))
;;

let dictionary_lst = read_dictionary_csv;;

let map_dictionary_to_list_list dictionary_lst word =
  List.map (fun x -> List.filter (fun y -> final_string(y) = word) x) dictionary_lst
;;

let map_list_list_to_list lst word = List.map (fun x -> 
  match x with
  | [] | _::_::_ -> ""
  | [x] -> x) (map_dictionary_to_list_list dictionary_lst word)
;;

let get_index_of_keyword filtered_list =
 let rec helper filtered_list pos =
   match filtered_list with
   | strin :: b when (String.length strin > 0)-> pos
   | _::b -> helper b (pos+1)
   | [] -> pos
 in
 helper filtered_list 1
;;

let rec lex_string string =
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
        let num =  map_list_list_to_list (map_dictionary_to_list_list dictionary_lst word) (final_string(word)) |> get_index_of_keyword in
        match num with
        | 1 -> Greeting (word) :: lex(!stop)
        | 2 -> Question (word) :: lex (!stop)
        | 3 -> Hungry (word) :: lex (!stop)
        | 4 -> Food_Type (word) :: lex (!stop)
        | 5 -> In_Out (word) :: lex (!stop)
        | 6 -> Recommendation (word) :: lex (!stop)
        | 7 -> Departure (word) :: lex (!stop)
        | 8 -> Delivery (word) :: lex (!stop)
        | 9 -> Location (word) :: lex (!stop)
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
  | Greeting _::_ -> printf "%s" greet_user_response; generate_response (get_string () |> lex_string) (flist)
  | Hungry _::_ -> printf "\nOkay, What are you in the mood to eat?\n\n"; generate_response (get_string () |> lex_string) (flist)
  | Recommendation _::_ -> printf "\nWell there's burger, fried chicken, sushi, beef, steak, shwarema and koshary, 
Choose your pick.\n\n"; generate_response (get_string () |> lex_string) (flist)
  | Food_Type w::_ ->  printf "\nOhh nice choice. Okay what's the location where you'll eat?\n\n"; generate_response (get_string () |> lex_string) (flist@[w])
  | Location w::_ -> printf "\nOkay that's a nice place. Do you want to dine (indoors, outdoors or do you want it delivery)?\n\n"; generate_response (get_string () |> lex_string) (flist@[w])
  | In_Out w::_ -> printf "\nI like your picks. Okay now what's ur price range?\n\n"; generate_response (get_string () |> lex_string) (flist@[w])
  | Price_Range w::_ -> printf "\nOooh that's gonna be cutting it. Okay let me process things and I'll get to you in a moment\n\n"; (flist @ [w])
  | Departure _::_ -> printf "\nHope to see you again!!\n\n"; flist
  | [] -> printf "\nI didn't quite understand that. Could you come again please.\n\n"; generate_response (get_string () |> lex_string) (flist)
  | _ -> []
;;

printf "\nHello User\n
My name is Mr. Hungry a chatbot made to help you choose your next eating expereience.
But first I need to ask you some stuff. *RUMBLING* HAHA Looks like you're dying to get a bite to eat. Let's get into it.
How can i help you???\n\n";;

let final = generate_response (get_string () |> lex_string) [];;

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

let matching_strings str substr =
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

let calculatePoints str list =
  let rec helper lst restaurantName points =
    match lst with
    | [] -> (restaurantName, points)
    | description::tail when (matching_strings str description = true) -> helper tail restaurantName (points+10)
    | description::tail when (matching_strings str description = false) -> helper tail restaurantName points
    | _::_ -> (restaurantName, points)
  in
  helper list (take_until_comma str) 0
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
;;

let rec createRestList ic = 
  match input_line ic with
  | exception End_of_file -> []
  | line -> (calculatePoints ((String.lowercase_ascii line)) final) :: createRestList ic
;;
  
let () =

  (* flush and close the channel *)

  (* Read file and display the first line *)
  if final <> [] then
    let ic = open_in restaurants in

    let restaurantsPoints = createRestList ic in

    let sortedRestaurants = List.sort (fun (_, c1) (_, c2) -> compare c2 c1) restaurantsPoints in
      Printf.printf "The first restaurant I recommend for you is %s" (fst (List.nth sortedRestaurants 0));
      print_endline "";
      Printf.printf "The second restaurant I recommend for you is %s" (fst (List.nth sortedRestaurants 1));
      print_endline "";
      Printf.printf "The third restaurant I recommend for you is %s" (fst (List.nth sortedRestaurants 2));

      (* write the result to stdout *)
      flush stdout;
      (* write on the underlying device now *)
      close_in ic

(*The first restaurant I recommend to you is

Restaurant name: 
Rating:
Popular Dish:
Location:
Hotline:

*)
