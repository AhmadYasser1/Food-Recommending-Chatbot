(* Randomize the numbers used in the below random_int variable *)
Random.self_init ();
open Printf

(* Define a set of rules to base the program upon *)
type rules = 
| Greeting of string 
| Recommendation of string 
| Question of string 
| Hunger of string
| Cuisine of string
| Indoors of string
| Outdoors of string
| Delivery of string
| Takeaway of string
| Restaurant_Location of string (*check its functionality on github*)
| Price_Range of string
| Closing_Conversation of string
| Family_Friends_Date of string
| Busyness of string
| Hidden_Gem of string
| Yes of string
| No of string
| Dine_In of string
;;

type 'a my_option = None | Some of 'a;;

type rules_option = rules my_option;;

(* Get input from the user *)
let getUserInput string =
  printf "%s" string;
  read_line;
;;

(* Responds to the user's greeting *)
let greetUserResponse =  
  (* Greetings are chosen on a random basis for diversity *)
  let random_int = Random.int 5 in
  match random_int with
  | 0 -> "\nHello, how can I help you?\n\n"
  | 1 -> "\nWELCOME HUNGRY, how can I help you?\n\n"
  | 2 -> "\n*rumble rumble* Okay okay, How can I help you?\n\n"
  | 3 -> "\nI'm here I'm here. What can I do for you?\n\n"
  | 4 -> "\nWhy the rush? How can I be of service?\n\n"
  | _ -> "\nHello, how can I help you?\n\n"
;;

(* Checks if the given character (char) is a char 
 * that belongs to the rules
* Helper function used in the lexing function below *)
let isChar character =
  let alphabet = "abcdefghijklmnopqrstuvwxyzABCDEFGHIJKLMNOPQRSTUVWXYZ'- " in
  let char_index_option = String.index_opt alphabet character in
  char_index_option <> None
;;

(* Checks if the given digit (char) is actually a digit or not 
  * Helper function used in the lexing function below *)
let isDigit digit =
  let digits = "0123456789-" in
  let int_index_option = String.index_opt digits digit in
  int_index_option <> None
;;

(* Reads the dictionary.csv file as a string and converts it to a String List List *)
let convert_dictionary_stringListList () =
  let dictionary_file_from_csv = open_in "Dictionary.csv" in
  let rec loop_over_lines accumulator =
    try
      let single_line = input_line dictionary_file_from_csv in
      let single_field = String.split_on_char ',' single_line in
      loop_over_lines (single_field :: accumulator)
    with
    | End_of_file -> close_in dictionary_file_from_csv; List.rev accumulator
  in
  loop_over_lines []
;;

(* Convert String to Char List*)
let string_to_charList string = 
  String.fold_left(fun accumulator char -> accumulator @ [char]) [] string
;;

(* Removes the duplicated characters in a Char List *)
let removeDuplicatesInCharList char_list =
  let rec helper char_list accumulator =
    match char_list with
    | [] -> accumulator
  | head :: tail -> if List.mem head accumulator then helper (tail) (accumulator) else helper (tail) (head :: accumulator)
  in 
  helper char_list []
;;

(* Removes the duplicated characters in a String *)
let removeDuplicatesInString userInput = 
  List.fold_left (fun accumulator char -> accumulator ^ (String.make 1 char)) "" (List.rev (removeDuplicatesInCharList (string_to_charList userInput)))
;;

(* Result of the convert_dictionary_csv_to_stringListList *)
(* let dictionary_stringListList = convert_dictionary_csv_to_stringListList;; *)

(* Filters the dictionary *)
let filterDictionaryByKeyword dictionary_stringListList keyword =
  List.map (fun string_list -> List.filter (fun word -> removeDuplicatesInString(word) = removeDuplicatesInString(keyword)) string_list) (dictionary_stringListList)
;;

(* Maps the string list list to a string list *)
let map_stringListList_to_stringList (dictionary_stringListList) keyword = 
  List.map (
    fun word -> match word with
    | [] | _::_::_ -> ""
      | [word] -> word) (filterDictionaryByKeyword dictionary_stringListList keyword)
    ;;
    (* Gets the index of the keyword found in the filtered list *)
    let get_index_of_keyword filtered_list =
      let rec helper filtered_list index =
        match filtered_list with
        | keyword :: _ when (String.length keyword > 0) -> index
        | _ :: tail -> helper tail (index + 1)
        | [] -> index
      in
  helper filtered_list 1
;;
let resultArray : rules_option list array = Array.make 18 ([None]);;

let printResponse index = 
  match index with
  | 5 -> printf "cuisine: "
  | 6 -> printf "Do you want to Dine indoors / outdoors"
  | 7 -> printf "Do you want to dine in / takeaway / delivery"
  | 11 -> printf "location: "
  | 13 -> printf "family / friends / date: "
  | 16 -> printf "Price Range: "
  | _ -> printf "ay 7aga"
;;

(* Lexes the user input (explained thoroughly below) *)
let rec lexString userInput =
  let string_length = String.length userInput in
  let rec tokenizeString starting_index =
    if starting_index >= string_length then
      ()
    else
    match userInput.[starting_index] with
    (* Ignore tabs, spaces, new lines, and  punctuations *)
    | ' ' | '\t' | '\n' | '?' | '!' | '.' -> tokenizeString (starting_index + 1)
    (* If the lexer meets a character of type Char it takes all 
     * characters after it until the next character is not of type Char *)
    | char when (isChar char) ->(
      let stopping_index = ref starting_index in
        while !stopping_index < string_length && isChar (Char.lowercase_ascii userInput.[!stopping_index]) do
        incr stopping_index;
      done;
      (* keyword is saved as a substring of the original string from 
         *the calcaulated index above *)
        let keyword = removeDuplicatesInString(String.lowercase_ascii (String.sub userInput starting_index (!stopping_index - starting_index))) in
        (* maps the keyword to the rules in the dictionary to identify 
        * it falls under which rule while removing the duplicates 
        * Example: heeyy and helloo are validated *)
        let keyword_index =  get_index_of_keyword (map_stringListList_to_stringList (filterDictionaryByKeyword (convert_dictionary_stringListList ()) keyword) (removeDuplicatesInString(keyword))) in
        match keyword_index with
        | 1 -> resultArray.(1) <- [Some (Greeting (keyword))] @ resultArray.(1); tokenizeString (!stopping_index)
        | 2 -> resultArray.(2) <- [Some (Recommendation (keyword))] @ resultArray.(2); tokenizeString (!stopping_index)
        | 3 -> resultArray.(3) <- [Some (Question (keyword))] @ resultArray.(3); tokenizeString (!stopping_index)
        | 4 -> resultArray.(4) <- [Some (Hunger (keyword))] @ resultArray.(4); tokenizeString (!stopping_index)
        | 5 -> resultArray.(5) <- [Some (Cuisine (keyword))] @ resultArray.(5); tokenizeString (!stopping_index)
        | 6 -> resultArray.(6) <- [Some (Dine_In (keyword))] @ resultArray.(16); tokenizeString (!stopping_index)
        | 7 -> resultArray.(7) <- [Some (Indoors (keyword))] @ resultArray.(6); tokenizeString (!stopping_index)
        | 8 -> resultArray.(8) <- [Some (Outdoors (keyword))] @ resultArray.(7); tokenizeString (!stopping_index)
        | 9 -> resultArray.(9) <- [Some (Delivery (keyword))] @ resultArray.(8); tokenizeString (!stopping_index)
        | 10 -> resultArray.(10) <- [Some (Takeaway (keyword))] @ resultArray.(9); tokenizeString (!stopping_index)
        | 11 -> resultArray.(11) <- [Some (Restaurant_Location (keyword))] @ resultArray.(10); tokenizeString (!stopping_index)
        | 12 -> resultArray.(12) <- [Some (Closing_Conversation (keyword))] @ resultArray.(11); tokenizeString (!stopping_index)
        | 13 -> resultArray.(13) <- [Some (Family_Friends_Date (keyword))] @ resultArray.(12); tokenizeString (!stopping_index)
        | 14 -> resultArray.(14) <- [Some (Yes (keyword))] @ resultArray.(13); tokenizeString (!stopping_index)
        | 15 -> resultArray.(15) <- [Some (No (keyword))] @ resultArray.(14); tokenizeString (!stopping_index)
        | _ -> tokenizeString (!stopping_index)
    )
    (* If the lexer meets a character of type int it takes all
    * characters after it until the next character is not of type int *)
    | digit when (isDigit digit) ->(
      let stopping_index = ref starting_index in
          while !stopping_index < string_length && isDigit userInput.[!stopping_index] do
            incr stopping_index;
          done;
          (* Any number computed is saved in as string and will 
          * represent the Price Range rule *)
          let price_string = String.sub userInput starting_index (!stopping_index - starting_index) in
          (* Price is converted to an int *)
          let price = int_of_string price_string in
          match price with
          | price when price >= 0 && price <= 200 -> resultArray.(16) <- [Some (Price_Range ("$"))] @ resultArray.(15); tokenizeString(!stopping_index)
          | price when price >= 201 && price <= 500 -> resultArray.(16) <- [Some (Price_Range ("$$"))] @ resultArray.(15); tokenizeString(!stopping_index)
          | price when price >= 501 -> resultArray.(16) <- [Some (Price_Range ("$$$"))] @ resultArray.(15); tokenizeString(!stopping_index)
          | _ -> tokenizeString (!stopping_index))
    | _ -> tokenizeString (starting_index + 1)
  in
  tokenizeString 0
;;

let lexed_user_input s = () |> getUserInput s |> lexString;;

let checkRecommendation ()=
  if (resultArray.(2) = [None]) then false
  else true
;;

let meetsMinimum =
  if (resultArray.(5) <> [None] && resultArray.(11) <> [None] && resultArray.(16) <> [None]) then true
  else false
;;

let checkBareMinimum ()=
  if (resultArray.(5) = [None]) then false
  else if (resultArray.(11) = [None]) then false
  else if (resultArray.(16) = [None]) then false
  else true
;;

let rec needRecommendations () =
  if resultArray.(14) <> [None]
    then 
      true
  else if resultArray.(15) <> [None]
    then 
      false
  else
    begin
      (() |> getUserInput "enter an appropriate answer yala: " |> lexString);
      needRecommendations ()
    end
;;
  
let rec printNotMeetingBareMinimum () =
  if (resultArray.(5) = [None]) then 
    begin
        [@warning "-10"] lexed_user_input "enter Cuisine: ";
        match resultArray.(5) with
        | _ -> printNotMeetingBareMinimum ()
    end
  else if (resultArray.(11) = [None]) then 
    begin
      [@warning "-10"] lexed_user_input "enter Restaurant Location: ";
      match resultArray.(11) with
      | _ -> printNotMeetingBareMinimum ()
    end
  else if (resultArray.(16) = [None]) then 
    begin
      [@warning "-10"] lexed_user_input "enter Price Range: ";
      match resultArray.(16) with
      | _ -> printNotMeetingBareMinimum ()
    end
;;

let string_of_option = function
  | None -> "None"
  | Some s -> "Some \"" ^ s ^ "\""



let rec mapListToString lst x =
  match x with 
  | 5 -> (match lst with
  | [] -> ""
  | option :: tail -> match option with
  | Some (Cuisine y) -> y ^ (mapListToString tail x)
  | _ -> ""
        )
  | 6 -> (match lst with
        | [] -> ""
        | option :: tail -> match option with
        | Some (Indoors y) -> y ^ (mapListToString tail x)
                | _ -> ""
        )
  | 7 -> (match lst with
        | [] -> ""
        | option :: tail -> match option with
                | Some (Outdoors y) -> y ^ (mapListToString tail x)
                | _ -> ""
                )  
                | 8 -> (match lst with
                | [] -> ""
        | option :: tail -> match option with
                | Some (Delivery y) -> y ^ (mapListToString tail x)
                | _ -> ""
        ) 
  | 9 -> (match lst with
        | [] -> ""
        | option :: tail -> match option with
                | Some (Takeaway y) -> y ^ (mapListToString tail x)
                | _ -> ""
        )   
  | 10 -> (match lst with
        | [] -> ""
        | option :: tail -> match option with
                | Some (Restaurant_Location y) -> y ^ (mapListToString tail x)
                | _ -> ""
        )
  | 12 -> (match lst with
  | [] -> ""
  | option :: tail -> match option with
  | Some (Family_Friends_Date y) -> y ^ (mapListToString tail x)
  | _ -> ""
  )
  | 15 -> (match lst with
  | [] -> ""
        | option :: tail -> match option with
        | Some (Price_Range y) -> y ^ (mapListToString tail x)
        | _ -> ""
        )
  | _ -> ""
  
;;
let finalListFromResultArray resultArray = 
  let rec helper x = 
    match x with
    | 5 -> (mapListToString resultArray.(x) x) :: (helper (x+1))
    | 6 -> (mapListToString resultArray.(x) x) :: (helper (x+1))
    | 7 -> (mapListToString resultArray.(x) x) :: (helper (x+1))
    | 8 -> (mapListToString resultArray.(x) x) :: (helper (x+1))
    | 9 -> (mapListToString resultArray.(x) x) :: (helper (x+1))
    | 10 -> (mapListToString resultArray.(x) x) :: (helper (x+1))
    | 11 -> (mapListToString resultArray.(x) x) :: (helper (x+1))
    | 13 -> (mapListToString resultArray.(x) x) :: (helper (x+1))
    | 16 -> (mapListToString resultArray.(x) x) :: (helper (x+1))
    | n when n > 16 -> []
    | _ -> helper (x+1)
  in
  helper 0
;;


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
let calculatePoints current_row finalListFromResultArray =
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
  helper finalListFromResultArray 0
;;

(* Calls the calculatePoints function over each restaurant/row
 * Returns a list of tuples as follows [(Restaurant1 Name, Points1) ; (Restaurant2 Name, Points2) ]*)
let rec createRestaurantsList restaurant_file_from_csv finalListFromResultArray= 
  match input_line restaurant_file_from_csv with
  | exception End_of_file -> []
  | line -> (line ,calculatePoints line finalListFromResultArray) :: createRestaurantsList restaurant_file_from_csv finalListFromResultArray
;;

let split_on_comma str =
  String.split_on_char ',' str
;;

let generateSuggestions finalListFromResultArray=

  (* flush and close the channel *)
  List.iter(fun x -> print_endline x) finalListFromResultArray;
  (* Read file and display the first line *)
  if (finalListFromResultArray <> [])
     then
    let restaurant_file_from_csv = open_in "Restaurants.csv" in

    let restaurants_points = createRestaurantsList restaurant_file_from_csv finalListFromResultArray in

    (* Sort, in descending order, the restaurants based on their calculated points *)
    let sorted_restaurants_points = List.sort (fun (_, point1) (_, point2) -> compare point2 point1) restaurants_points in

    for i = 0 to 2 do
      let list_of_restaurant_info = split_on_comma(fst (List.nth sorted_restaurants_points i)) in
      printf "The first restaurant I recommend for you is %s\n" (List.nth list_of_restaurant_info 0);
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
  ;;
;;


let checkValidity index =
  if (resultArray.(index) = [None]) then false
  else true
;;

let rec generate_response () = 
  begin
    if (checkRecommendation ()) then
      begin
        if (checkBareMinimum ()) then 
          generateSuggestions (finalListFromResultArray resultArray)
        else
          begin
            printf "You are not meeting the bare minimum khod\n";
            printNotMeetingBareMinimum ();
            generateSuggestions (finalListFromResultArray resultArray)
          end
      end
    else
      begin
        (() |> getUserInput "Do you need recommendations: " |> lexString);
        if (needRecommendations ()) then
          begin
            resultArray.(2) <- [Some (Recommendation ("Recommend"))] @ resultArray.(2);
            (generate_response ())
          end
        else 
          begin 
            let stopping_index = ref 5 in
            while !stopping_index <= 16 do
              begin
                if (!stopping_index >= 6 && !stopping_index <= 10) then
                  begin
                    if (resultArray.(6) <> [None])
                        then 
                          begin
                            if (resultArray.(7) <> [None] || resultArray.(8) <> [None])
                              then stopping_index := 11
                          else
                            begin
                            printResponse 6;
                            (() |> getUserInput "\nEnter your input: " |> lexString)
                            end
                          end
                    else if (resultArray.(9) <> [None] || resultArray.(10) <> [None])
                      then stopping_index := 11
                    else
                      begin
                        printResponse 7;
                        (() |> getUserInput "\nEnter your input: " |> lexString)
                      end
                  end
                else if (!stopping_index = 12)
                  then stopping_index := 16
                else if (checkValidity (!stopping_index) <> true) then
                  begin
                    printResponse (!stopping_index);
                    (() |> getUserInput "\nEnter your input: " |> lexString)
                  end
                else
                  incr stopping_index
              end
            done;
            generateSuggestions (finalListFromResultArray resultArray)
          end
      end
  end
;;

let main () = 
  printf "Hey peeps, I'm Omar Elabasery";
  printf "What can I help you with?\n";
  lexed_user_input "Enter input: ";
  generate_response ();
;;

main ();
  



