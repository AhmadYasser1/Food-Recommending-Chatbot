#use "restaurant.ml"
(* Prints response based on index value *)
let printResponse index = 
  match index with
  | 3 -> printf "\nChoose your desired cuisine (Sushi, Wings, Burger, Pizza, etc.) \n"
  | 4 -> printf "\nDo you want to Dine indoors or outdoors? \n"
  | 7 -> printf "\nDo you want to dine in, takeaway, or delivery? \n"
  | 9 -> printf "\nWhere do you wish to eat? \n"
  | 12 -> printf "\nEl Mezaneya 3amla eh? (Budget wenaby) \n"
  | _ -> printf ""
;;

let rec generate_response () = 
 begin
   (* Checks if the user wants a recommendations *)
   if (checkRecommendation ()) then
     begin
       (* Checks if the user meets the bare minimum
        * If true, it generates a response/suggestion right away 
        * If false, it asks the user some questions to generate a response/suggestion *)
       if (checkBareMinimum ()) then 
         generateSuggestions (finalListFromResultArray resultArray)
       else
         begin
           printf "\n I cannot recommend a restaurant with this few information.\n";
           printf "\nYou can help me by answering questions related to your desired cuisine, location, and price range!\n";
           printNotMeetingBareMinimum ();
           generateSuggestions (finalListFromResultArray resultArray)
         end
     end
   else
     begin
       (* If the user wants a recommendation right away, then the same function is called recursively *)
       printf "\nDo you want me to recommend a restaurant right away?\n";
       (() |> getUserInput "Enter your response: " |> lexString);
       if (needRecommendations ()) then
         begin
           resultArray.(1) <- [Some (Recommendation ("Recommend"))] @ resultArray.(1);
           (generate_response ())
         end
       else
         (* The user, then, is asked all questions to generate the most personalized responses *) 
         begin 
           let stopping_index = ref 3 in
           while !stopping_index <= 12 do
             begin
               if (!stopping_index >= 4 && !stopping_index <= 8) then
                 begin
                   if (resultArray.(4) <> [None])
                       then 
                         begin
                           if (resultArray.(5) <> [None] || resultArray.(6) <> [None])
                             then stopping_index := 9
                         else
                           begin
                           printResponse 4;
                           (() |> getUserInput "Enter here: " |> lexString)
                           end
                         end
                   else if (resultArray.(7) <> [None] || resultArray.(8) <> [None])
                     then stopping_index := 9
                   else
                     begin
                       printResponse 7;
                       (() |> getUserInput "Enter here: " |> lexString)
                     end
                 end
               else if (!stopping_index = 10)
                 then stopping_index := 12
               else if (checkValidity (!stopping_index) <> true) then
                 begin
                   printResponse (!stopping_index);
                   (() |> getUserInput "Enter here: " |> lexString)
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