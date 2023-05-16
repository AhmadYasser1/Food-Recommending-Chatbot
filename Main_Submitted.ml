Random.self_init ();; (* Randomize the numbers used in the below random_int variable *)
open Printf

(* Define a set of rules to base the program upon *)
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
| Name of string
;;

(* Get input from the user *)
let get_string =
  read_line
;;

(* Responds to the user's greeting *)
let greet_user_response =
  (* Greetings are chosen on a random basis for diversity *)
  let rand = Random.int 5 in
  match rand with
  | 0 -> "\nHello, how can I help you?\n\n"
  | 1 -> "\nWELCOME HUNGRY, how can I help you?\n\n"
  | 2 -> "\n*rumble rumble* Okay okay, How can I help you?\n\n"
  | 3 -> "\nI'm here I'm here. What can I do for you?\n\n"
  | 4 -> "\nWhy the rush? How can I be of service?\n\n"
  | _ -> "\nHello, how can I help you?as"
;;

(* Checks if the given character (char) is a char 
 * that belongs to the rules
 * Helper function used in the lexing function below *)
let is_char c =
  let character = "abcdefghijklmnopqrstuvwxyz'-" in
  let loc = String.index_opt character c in
  loc <> None
;;

(* Checks if the given digit (char) is actually a digit or not 
 * Helper function used in the lexing function below *)
let is_digit c =
  let digits = "0123456789-" in
  let loc = String.index_opt digits c in
  loc <> None
;;

(* Reads the dictionary.csv file as a string and converts it to a String List List *)
let read_dictionary_csv =
  let dictionary_file = "Dictionary.csv" in
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

(* Convert String to Char List *)
let string_to_clst word = 
  String.fold_left(fun acc x -> acc @ [x]) [] word
;;

(* Removes the duplicated characters in a Char List *)
let remove_duplicates clst =
  let rec helper clst acc =
  match clst with
  | [] -> acc
  | h::t -> if List.mem h acc then helper (t) (acc) else helper (t) (h::acc)
  in 
  helper clst []
;;

(* Removes the duplicated characters in a String *)
let final_string word = 
  List.fold_left (fun acc x -> acc ^ (String.make 1 x)) "" (List.rev (remove_duplicates (string_to_clst word)))
;;

let dictionary_lst = read_dictionary_csv;;

(* Filters the dictionary list given each time with the word
 * to check if it's one of the rules or not *)
let map_dictionary_to_list_list dictionary_lst word =
  List.map (fun x -> List.filter (fun y -> final_string(y) = word) x) dictionary_lst
;;

(* Maps the string list list to a string list *)
let map_list_list_to_list word = List.map (fun x -> 
  match x with
  | [] | _::_::_ -> ""
  | [x] -> x) (map_dictionary_to_list_list dictionary_lst word)
;;

(* Gets the index of the keyword found in the filtered list *)
let get_index_of_keyword filtered_list =
 let rec helper filtered_list pos =
   match filtered_list with
   | strin :: _ when (String.length strin > 0)-> pos
   | _::b -> helper b (pos+1)
   | [] -> pos
 in
 helper filtered_list 1
;;

(* Lexes the user input (explained thoroughly below) *)
let rec lex_string string =
  let len = String.length string in
  let rec lex pos =
    if pos >= len then
    []
  else
    (* Check if the string taken is a location *)
    let place = final_string (String.lowercase_ascii string) in
    (* maps the keyword to the rules in the dictionary to identify 
     * if it falls under a location from the dictionary or not *)
    let is_place = map_list_list_to_list (place) |> get_index_of_keyword in
    if is_place = 8 then
      [Location place]
    else
    match string.[pos] with
    (* Ignore tabs, spaces, new lines, and  punctuations *)
    | ' ' | '\t' | '\n' | '?' -> lex (pos+1)
    (* If the lexer meets a character of type Char it takes all 
     * characters after it until the next character is not of type Char *)
    | d -> if (is_char d) then 
        let stop = ref pos in
        while !stop < len && is_char (Char.lowercase_ascii string.[!stop]) do
        incr stop;
        done;
        (* keyword is saved as a substring of the original string from 
         *the calcaulated index above *)
        let word = final_string (String.lowercase_ascii (String.sub string pos (!stop - pos))) in
        (* maps the keyword to the rules in the dictionary to identify 
         * it falls under which rule while removing the duplicates 
         * Example: heeyy and helloo are validated *)
        let num =  map_list_list_to_list (word) |> get_index_of_keyword in
        match num with
        | 1 -> Greeting (word) :: lex(!stop)
        | 2 -> Question (word) :: lex (!stop)
        | 3 -> Hungry (word) :: lex (!stop)
        | 4 -> Food_Type (word) :: lex (!stop)
        | 5 -> In_Out (word) :: lex (!stop)
        | 6 -> Recommendation (word) :: lex (!stop)
        | 7 -> Departure (word) :: lex (!stop)
        | _ -> lex (!stop)
      else
        (* If the lexer meets a character of type int it takes all 
        * characters after it until the next character is not of type int *)
        if (is_digit d) then
          let stop = ref pos in
          while !stop < len && is_digit string.[!stop] do
          incr stop;
          done;
          (* Any number computed is saved in as string and will 
           * represent the Price Range rule *)
          let price_string  = String.sub string pos (!stop - pos) in
          (* Price is converted to an int *)
          let price  = int_of_string price_string  in
          match price  with
          | 0 -> printf "Food isn't free buddy. Food isn't free. Please enter a reasonable amount:\n\n"; get_string () |> lex_string
          | price when price  >= 5 && price  <= 200 -> Price_Range("$") :: (lex !stop) 
          | price when price  >= 201 && price  <= 500 -> Price_Range("$$") :: (lex !stop)
          | price when price >= 501 -> Price_Range("$$$") :: (lex !stop)
          | _ -> lex (!stop)
        else
          lex (pos+1)
  in
  lex 0
;;

(* Generates response according to the user's response *)
let rec generate_response lst flist =
  match lst with
  | Question _::t -> if t = [Hungry "eat"] then generate_response ([Recommendation "recommend"]) flist else generate_response t flist
  | Greeting _::_ -> printf "%s" greet_user_response; generate_response (get_string () |> lex_string) (flist)
  | Hungry _::_ -> printf "\nOkay, What are you in the mood to eat?\n\n"; generate_response (get_string () |> lex_string) (flist)
  | Recommendation _::_ -> printf "\nWell there's burger, fried chicken, sushi, beef, steak, shawerma and koshary 
Choose your pick.\n\n"; generate_response (get_string () |> lex_string) (flist)
  | Food_Type w::_ ->  printf "\nOhh nice choice. Okay what's the location where you'll eat?\n\n"; generate_response (get_string () |> lex_string) (flist@[w])
  | Location w::_ -> printf "\nOkay that's a nice place. Do you want to dine (indoors, outdoors or do you want it delivery)?\n\n"; generate_response (get_string () |> lex_string) (flist@[w])
  | In_Out w::_ -> printf "\nI like your picks. Okay now what's ur price range?\n\n"; generate_response (get_string () |> lex_string) (flist@[w])
  | Price_Range w::_ -> printf "\nOooh that's gonna be cutting it. Okay let me process things and I'll get to you in a moment\n\n"; (flist @ [w])
  | Departure _::_ -> printf "\nHope to see you again!!\n\n"; flist
  | [] -> printf "\nI didn't quite understand that. Could you come again please.\n\n"; generate_response (get_string () |> lex_string) (flist)
  | _ -> []
;;

(* Greets the user as the first message of the program *)
printf "\nHello User\n
My name is Mr. Hungry a chatbot made to help you choose your next eating expereience.
But first I need to ask you some stuff. *RUMBLING* HAHA Looks like you're dying to get a bite to eat. Let's get into it.
How can i help you???\n\n";;

let user_preference = generate_response (get_string () |> lex_string) [];;

(* Reads each first string prior to the first comma in each row 
 * in the Restaurant.csv file to save each restaurant in a tuple 
 * for later computation *)
 let readStringUntilFirstComma string =
  let string_length = String.length string in
  let rec loopOverLine index =
    if index = string_length || string.[index] = ',' then
      String.sub string 0 index
    else
      loopOverLine (index + 1)
  in
  loopOverLine 0
;;

(* Checks if the user preference is in the row in question
 * Helper function to the calculatePoints function *)
let isPreference_Substring_Of_CurrentRow current_row user_preference =
  let current_row_length = String.length current_row in
  let user_preference_length = String.length user_preference in
  let rec loopOverRow starting_index =
    if starting_index + user_preference_length > current_row_length then
      false
    else if String.sub current_row starting_index user_preference_length = user_preference then
      true
    else
      loopOverRow (starting_index + 1)
  in
  loopOverRow 0
;;

(* calculates the points for a given restaurant/row based on the user's preferences 
 * Returns a tuple as follows (Restaurant Name, Points)*)
let calculatePoints current_row list_of_user_preferences =
  let rec helper list points =
    match list with
    | [] -> points
    (* Checks if the user_preference is included in the row in question 
     * If it is a substring of the row, then 10 points are added
     * Else, no points are added*)
    | user_preference :: tail when (isPreference_Substring_Of_CurrentRow (String.lowercase_ascii current_row) user_preference = true) -> helper tail (points + 10)
    | user_preference :: tail when (isPreference_Substring_Of_CurrentRow (String.lowercase_ascii current_row) user_preference = false) -> helper tail points
    | _ :: _ -> points
  in
  helper list_of_user_preferences 0
;;

(* Calls the calculatePoints function over each restaurant/row
 * Returns a list of tuples as follows [(Restaurant1 Name, Points1) ; (Restaurant2 Name, Points2) ]*)
let rec createRestaurantsList restaurant_file_from_csv = 
  match input_line restaurant_file_from_csv with
  | exception End_of_file -> []
  | line -> (line ,calculatePoints line user_preference) :: createRestaurantsList restaurant_file_from_csv
;;

let split_on_comma str =
  String.split_on_char ',' str
;;

(* Anonymous function that displays the restaurant recommendations to the user based on the given preferences *)
let () =

  (* flush and close the channel *)

  (* Read file and display the first line *)
  if user_preference <> [] then
    let restaurant_file_from_csv = open_in "Restaurants.csv" in

    let restaurants_points = createRestaurantsList restaurant_file_from_csv in

    (* Sort, in descending order, the restaurants based on their calculated points *)
    let sorted_restaurants_points = List.sort (fun (_, point1) (_, point2) -> compare point2 point1) restaurants_points in

    for i = 0 to 2 do
      let list_of_restaurant_info = split_on_comma(fst (List.nth sorted_restaurants_points i)) in
      printf "Restaurant #%i I recommend for you is %s\n" (i+1) (List.nth list_of_restaurant_info 0);
      print_endline "\nRestaurant's Info\n";
      printf "\tCity: %s,  Serving: %s,\n\tServices Available: %s,  You can Dine: %s,  Working Hours: %s\n
      \tBest Dish: %s,  Capacity: %s,  Busyness: %s,  Waiting time: %s minutes,  Price Range (per person): %s\n
      \tDelivery time: %s,  Hidden Gem?: %s,  GPS Location: %s,  You can take: %s there,  Rating: %s,  Hotline: %s\n\n" (List.nth list_of_restaurant_info 1)
       (List.nth list_of_restaurant_info 2) (List.nth list_of_restaurant_info 3) (List.nth list_of_restaurant_info 4) (List.nth list_of_restaurant_info 5)
      (List.nth list_of_restaurant_info 6) (List.nth list_of_restaurant_info 7) (List.nth list_of_restaurant_info 8) (List.nth list_of_restaurant_info 9)
       (if (List.nth list_of_restaurant_info 10) = "$" then "5-200 EGP" else if (List.nth list_of_restaurant_info 10) = "$$" then "201-500 EGP" else "501+ EGP") 
       (List.nth list_of_restaurant_info 11) (List.nth list_of_restaurant_info 12) (List.nth list_of_restaurant_info 13)
      (List.nth list_of_restaurant_info 14) (List.nth list_of_restaurant_info 15) (List.nth list_of_restaurant_info 16);
    done;

      (* write the result to stdout *)
      flush stdout;
      close_in restaurant_file_from_csv

(*The first restaurant I recommend to you is

Restaurant name: 
Rating:
Popular Dish:
Location:
Hotline:

*)
