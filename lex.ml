(* Randomize the numbers used in the below random_int variable *)
Random.self_init ();
open Printf
#use "rules.ml"

(* Get input from the user *)
let getUserInput = read_line;;

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
  let alphabet = "abcdefghijklmnopqrstuvwxyz'-" in
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
let dictionary_stringListList =
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
  List.map (fun string_list -> List.filter (fun word -> removeDuplicatesInString(word) = removeDuplicatesInString(keyword)) string_list) dictionary_stringListList
;;

(* Maps the string list list to a string list *)
let map_stringListList_to_stringList dictionary_stringListList keyword = 
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
(* Lexes the user input (explained thoroughly below) *)
let rec lexString userInput =
  let string_length = String.length userInput in
  let rec tokenizeString starting_index accumulator =
    if starting_index >= string_length then
    accumulator
  else
    match userInput.[starting_index] with
    (* Ignore tabs, spaces, new lines, and  punctuations *)
    | ' ' | '\t' | '\n' | '?' | '!' | '.' -> tokenizeString (starting_index + 1) (accumulator)
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
        let keyword_index =  get_index_of_keyword (map_stringListList_to_stringList (filterDictionaryByKeyword dictionary_stringListList keyword) (removeDuplicatesInString(keyword))) in
        match keyword_index with
        | 1 -> tokenizeString(!stopping_index) (Greeting (keyword) :: accumulator)
        | 2 -> tokenizeString(!stopping_index) (Recommendation (keyword) :: accumulator)
        | 3 -> tokenizeString(!stopping_index) (Question (keyword) :: accumulator)
        | 4 -> tokenizeString(!stopping_index) (Hunger (keyword) :: accumulator)
        | 5 -> tokenizeString(!stopping_index) (Cuisine (keyword) :: accumulator)
        | 6 -> tokenizeString(!stopping_index) (Dining (keyword) :: accumulator)
        | 7 -> tokenizeString(!stopping_index) (Indoors_Outdoors (keyword) :: accumulator)
        | 8 -> tokenizeString(!stopping_index) (Delivery (keyword) :: accumulator)
        | 9 -> tokenizeString(!stopping_index) (Takeaway (keyword) :: accumulator)
        | 10 -> tokenizeString(!stopping_index) (Restaurant_Location (keyword) :: accumulator)
        | 11 -> tokenizeString(!stopping_index) (Price_Range (keyword) :: accumulator)
        | 12 -> tokenizeString(!stopping_index) (Closing_Conversation (keyword) :: accumulator)
        | _ -> tokenizeString (!stopping_index) (accumulator))
    (* If the lexer meets a character of type int it takes all 
    * characters after it until the next character is not of type int *)
    | digit when (isDigit digit) ->
          (let stopping_index = ref starting_index in
          while !stopping_index < string_length && isDigit userInput.[!stopping_index] do
            incr stopping_index;
          done;
          (* Any number computed is saved in as string and will 
           * represent the Price Range rule *)
          let price_string = String.sub userInput starting_index (!stopping_index - starting_index) in
          (* Price is converted to an int *)
          let price = int_of_string price_string in
          match price with
          | 0 -> printf "Food isn't free buddy. Food isn't free. Please enter a reasonable amount:\n\n"; getUserInput () |> lexString
          | price when price >= 5 && price <= 200 -> tokenizeString(!stopping_index) (Price_Range("$") :: accumulator)
          | price when price >= 201 && price <= 500 -> tokenizeString(!stopping_index) (Price_Range("$$") :: accumulator)
          | price when price >= 501 -> tokenizeString(!stopping_index) (Price_Range("$$$") :: accumulator)
          | _ -> tokenizeString (!stopping_index) (accumulator))
    | _ -> tokenizeString (starting_index + 1) (accumulator)
  in
  tokenizeString 0 []
;;

(* Greets the user as the first message of the program *)
print_string "\nHello User\n
          My name is Mr. Hungry a chatbot made to help you choose your next eating expereience.
          But first I need to ask you some stuff. *RUMBLING* HAHA Looks like you're dying to get a bite to eat. Let's get into it.
          How can i help you???\n\n";;

let userPreference = getUserInput () |> lexString ;;
