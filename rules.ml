(* Define a set of rules to base the program upon *)
type rules = 
| Recommendation of string 
| Hunger of string
| Cuisine of string
| Dine_In of string
| Indoors of string
| Outdoors of string
| Delivery of string
| Takeaway of string
| Restaurant_Location of string
| Price_Range of string
| Yes of string
| No of string
;;

type 'a my_option = None | Some of 'a;;

type rules_option = rules my_option;;

(* Get input from the user *)
let getUserInput string =
  printf "%s" string;
  read_line;
;;
