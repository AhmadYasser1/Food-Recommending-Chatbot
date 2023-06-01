(* Checks if the given character (char) is a char 
 * that belongs to the rules
 * Helper function used in the lexing function below *)
 let isChar character =
  let alphabet = "abcdefghijklmnopqrstuvwxyzABCDEFGHIJKLMNOPQRSTUVWXYZ'-" in
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

(* Filters the dictionary by a keyword*)
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