(* Define a set of rules to base the program upon *)
type rules = 
| Greeting of string
| Question of string
| Hunger of string
| Cuisine of string
| Indoors_Outdoors of string 
| Food_Recommendation of string
| Closing_Conversation of string
| Delivery of string
| Restaurant_Location of string
| Price_Range of string
;;