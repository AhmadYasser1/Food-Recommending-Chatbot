#use "lex.ml"

(* Check if the user wants a recommendation *)
let checkRecommendation () =
  if (resultArray.(1) = [None]) then false
  else true
;;

let checkValidity index =
  if (resultArray.(index) <> [None]) then true
  else false
;;
  
(* Check if the user meets the bare minimum *)
let checkBareMinimum () =
  if (resultArray.(3) = [None]) then false
  else if (resultArray.(9) = [None]) then false
  else if (resultArray.(12) = [None]) then false
  else true
;;
  
(* Check if the user needs a recommendation or not *)
let rec needRecommendations () =
  if resultArray.(10) <> [None] then true
  else if resultArray.(11) <> [None] then false
  else
    begin
      (() |> getUserInput "I didn't recognise your answer, please enter yes or no: " |> lexString);
      needRecommendations ()
    end
;;
  
(* Tells the user if he/she does not meet the minimum 
 * requirements for a recommendation *)
let rec printNotMeetingBareMinimum () =
  if (resultArray.(3) = [None]) then 
    begin
        lexed_user_input "Enter Cuisine here (Sushi, Wings, Burger, Pizza, etc.): ";
        match resultArray.(3) with
        | _ -> printNotMeetingBareMinimum ()
    end
  else if (resultArray.(9) = [None]) then 
    begin
      lexed_user_input "\nEnter Restaurant Location here: ";
      match resultArray.(9) with
      | _ -> printNotMeetingBareMinimum ()
    end
  else if (resultArray.(12) = [None]) then 
    begin
      lexed_user_input "\nEl Mezaneya 3amla eh? (Budget wenaby) \n";
      match resultArray.(12) with
      | _ -> printNotMeetingBareMinimum ()
    end
;;


(* Helper function that maps an option list to a string 
 * according to a given index *)
let rec mapListToString lst index =
  match index with 
  | 3 -> (match lst with
          | [] -> ""
          | option :: tail -> match option with
          | Some (Cuisine element) -> element ^ (mapListToString tail index)
          | _ -> ""
          )
  | 4 -> (match lst with
          | [] -> ""
          | option :: tail -> match option with
          | Some (Dine_In element) -> element ^ (mapListToString tail index)
                  | _ -> ""
          )
  | 5 -> (match lst with
          | [] -> ""
          | option :: tail -> match option with
          | Some (Indoors element) -> element ^ (mapListToString tail index)
                  | _ -> ""
          )
  | 6 -> (match lst with
        | [] -> ""
        | option :: tail -> match option with
                | Some (Outdoors element) -> element ^ (mapListToString tail index)
                | _ -> ""
          )  
  | 7 -> (match lst with
          | [] -> ""
          | option :: tail -> match option with
                | Some (Delivery element) -> element ^ (mapListToString tail index)
                | _ -> ""
          ) 
  | 8 -> (match lst with
        | [] -> ""
        | option :: tail -> match option with
                | Some (Takeaway element) -> element ^ (mapListToString tail index)
                | _ -> ""
          )   
  | 9 -> (match lst with
        | [] -> ""
        | option :: tail -> match option with
                | Some (Restaurant_Location element) -> element ^ (mapListToString tail index)
                | _ -> ""
          )
  | 12 -> (match lst with
  | [] -> ""
        | option :: tail -> match option with
        | Some (Price_Range element) -> element ^ (mapListToString tail index)
        | _ -> ""
        )
  | _ -> ""
;;
(* Maps each option list in the resultArray to a string *)
let finalListFromResultArray resultArray = 
  let rec helper index = 
    match index with
    | 3 -> (mapListToString resultArray.(index) index) :: (helper (index + 1))
    | 4 -> (mapListToString resultArray.(index) index) :: (helper (index + 1))
    | 5 -> (mapListToString resultArray.(index) index) :: (helper (index + 1))
    | 6 -> (mapListToString resultArray.(index) index) :: (helper (index + 1))
    | 7 -> (mapListToString resultArray.(index) index) :: (helper (index + 1))
    | 8 -> (mapListToString resultArray.(index) index) :: (helper (index + 1))
    | 9 -> (mapListToString resultArray.(index) index) :: (helper (index + 1))
    | n when n > 9 -> []
    | _ -> helper (index + 1)
  in
  helper 0
;;