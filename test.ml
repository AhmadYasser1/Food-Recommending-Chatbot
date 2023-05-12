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

let print_csv rows =
  List.iter (fun row ->
    let line = String.concat " " row in
    print_endline line
  ) rows
;;

let dictionary_lst = read_dictionary;;

let filtering_dictionary dictionary_lst word =
  let filtered_list = List.map (fun x -> List.filter (fun y -> y = word) x) dictionary_lst in
    List.flatten (List.map (fun list -> list) filtered_list) 
;;

let flattened_list filtered_list =
  List.flatten (List.map (fun list -> list) filtered_list)
;;



let list = filtering_dictionary dictionary_lst "bye";;