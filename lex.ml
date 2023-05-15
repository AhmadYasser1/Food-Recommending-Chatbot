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

(* Reads the dictionary.csv file as a string and converts it to a String List List  *)
let convert_dictionary_csv_to_stringListList =
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
let removeDuplicatesFromCharList char_list =
  let rec helper char_list accumulator =
  match char_list with
  | [] -> accumulator
  | head :: tail -> if List.mem head accumulator then helper (tail) (accumulator) else helper (tail) (head :: accumulator)
  in 
  helper char_list []
;;

(* Computes a string with no duplicated characters *)
let removeDuplicatesFromString userInput = 
  (* String.make is used to create a string of length 1 for it to be concatenated with the accumulator
   * List.rev is used for the list to be compatible with the fold_left method *)
  List.fold_left (fun accumulator char -> accumulator ^ (String.make 1 char)) "" (List.rev (removeDuplicatesFromCharList (string_to_charList userInput)))
;;

(* Result of the convert_dictionary_csv_to_stringListList *)
let dictionary_stringListList = convert_dictionary_csv_to_stringListList;;

(* Maps the dictionary file to a string list list *)
let filterDictionaryByKeyword dictionary_file keyword =
  List.map (fun string_list -> List.filter (fun word -> removeDuplicatesFromString(word) = removeDuplicatesFromString(keyword)) string_list) dictionary_file
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
 let rec helper filtered_list position =
   match filtered_list with
   | keyword :: _ when (String.length keyword > 0)-> position
   | _ :: tail -> helper tail (position + 1)
   | [] -> position
 in
 helper filtered_list 1
;;

(* Lexes the user input (explained thoroughly below) *)
let rec lexString userInput =
  let string_length = String.length userInput in
  let rec tokenizeString position =
    if position >= string_length then
    []
  else
    match userInput.[position] with
    (* Ignore tabs, spaces, new lines, and  punctuations *)
    | ' ' | '\t' | '\n' | '?' | '!' | '.' -> tokenizeString (position + 1)
    (* If the lexer meets a character of type Char it takes all 
     * characters after it until the next character is not of type Char *)
    | char when (isChar char) ->(
        let stopping_index = ref position in
        while !stopping_index < string_length && isChar (Char.lowercase_ascii userInput.[!stopping_index]) do
        incr stopping_index;
        done;
        (* keyword is saved as a substring of the original string from 
         *the calcaulated index above *)
        let keyword = removeDuplicatesFromString(String.lowercase_ascii (String.sub userInput position (!stopping_index - position))) in
        (* maps the keyword to the rules in the dictionary to identify 
         *it falls under which rule while removing the duplicates 
         * Example: heeyy and helloo are validated*)
        let keyword_index =  map_stringListList_to_stringList (filterDictionaryByKeyword dictionary_stringListList userInput) (removeDuplicatesFromString(userInput)) |> get_index_of_keyword in
        match keyword_index with
        | 1 -> Greeting (keyword) :: tokenizeString(!stopping_index)
        | 2 -> Question (keyword) :: tokenizeString (!stopping_index)
        | 3 -> Hunger (keyword) :: tokenizeString (!stopping_index)
        | 4 -> Cuisine (keyword) :: tokenizeString (!stopping_index)
        | 5 -> Indoors_Outdoors (keyword) :: tokenizeString (!stopping_index)
        | 6 -> Food_Recommendation (keyword) :: tokenizeString (!stopping_index)
        | 7 -> Closing_Conversation (keyword) :: tokenizeString (!stopping_index)
        | 8 -> Delivery (keyword) :: tokenizeString (!stopping_index)
        | 9 -> Restaurant_Location (keyword) :: tokenizeString (!stopping_index)
        | _ -> tokenizeString (!stopping_index))
    (* If the lexer meets a character of type int it takes all 
     * characters after it until the next character is not of type int *)
    | digit when (isDigit digit) ->
          (let stopping_index = ref position in
          while !stopping_index < string_length && isDigit userInput.[!stopping_index] do
          incr stopping_index;
          done;
          (* Any number computed is saved in as string and will 
           * represent the Price Range rule *)
          let price_string = String.sub userInput position (!stopping_index - position) in
          (* Price is converted to an int *)
          let price = int_of_string price_string in
          match price with
          | 0 -> printf "Food isn't free buddy. Food isn't free. Please enter a reasonable amount:\n\n"; getUserInput () |> lexString
          | price when price >= 5 && price <= 200 -> Price_Range("$") :: (tokenizeString !stopping_index) 
          | price when price >= 201 && price <= 500 -> Price_Range("$$") :: (tokenizeString !stopping_index)
          | price when price >= 501 -> Price_Range("$$$") :: (tokenizeString !stopping_index)
          | _ -> tokenizeString (!stopping_index) )
    | _ -> tokenizeString (position + 1)
  in
  tokenizeString 0
;;

(* Generates response according to the user's response *)
let rec generateResponse list_of_tokens list_of_responses =
  match list_of_tokens with
  | Question _ :: tail -> generateResponse tail (list_of_responses)
  | Greeting _ :: _ -> printf "%s" greetUserResponse ; generateResponse (getUserInput () |> lexString) (list_of_responses)
  | Hunger _ :: _ -> printf "\nOkay, What are you in the mood to eat?\n\n"; generateResponse (getUserInput () |> lexString) (list_of_responses)
  | Food_Recommendation _ :: _ -> printf "\nWell there's burger, fried chicken, sushi, beef, steak, shawerma and koshary, 
Choose your pick.\n\n"; generateResponse (getUserInput () |> lexString) (list_of_responses)
  | Cuisine head :: _ ->  printf "\nOhh nice choice. Okay what's the location where you'll eat?\n\n"; generateResponse (getUserInput () |> lexString) (list_of_responses @ [head])
  | Restaurant_Location head :: _ -> printf "\nOkay that's a nice place. Do you want to dine (indoors, outdoors or do you want it delivery)?\n\n"; generateResponse (getUserInput () |> lexString) (list_of_responses @ [head])
  | Indoors_Outdoors head :: _ -> printf "\nI like your picks. Okay now what's ur price range?\n\n"; generateResponse (getUserInput () |> lexString) (list_of_responses @ [head])
  | Price_Range head :: _ -> printf "\nOooh that's gonna be cutting it. Okay let me process things and I'll get to you in a moment\n\n"; (list_of_responses @ [head])
  | Closing_Conversation _ :: _ -> printf "\nHope to see you again!!\n\n"; list_of_responses
  | [] -> printf "\nI didn't quite understand that. Could you come again please.\n\n"; generateResponse (getUserInput () |> lexString) (list_of_responses)
  | _ -> []
;;

(* Greets the user as the first message of the program *)
print_string "\nHello User\n
          My name is Mr. Hungry a chatbot made to help you choose your next eating expereience.
          But first I need to ask you some stuff. *RUMBLING* HAHA Looks like you're dying to get a bite to eat. Let's get into it.
          How can i help you???\n\n";;


let userPreference = generateResponse (getUserInput () |> lexString) [];;
