open Printf
#use "dictionary.ml"
#use "response.ml"

(* Main function that takes the user input *)
let main () = 
  printf "Hey User, I'm Mr. Hungry, your food recommending mate who will save your day!\n";
  printf "What can I help you with?\n";
  lexed_user_input "Enter here: ";
  generate_response ();
;;

main ();