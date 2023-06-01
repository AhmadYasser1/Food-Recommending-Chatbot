#use "validation.ml"
(* Helper function used to convert options to strings for 
 * displaying the recommendations*)
 let string_of_option = function
 | None -> "None"
 | Some s -> "Some \"" ^ s ^ "\""
;;

(* Reads each first string prior to the first comma in each row 
* in the Restaurant.csv file to save each restaurant in a tuple 
* for later computation *)
let readStringUntilFirstComma s =
 let string_length = String.length s in
 let rec loopOverLine index =
   if index = string_length || s.[index] = ',' then
     String.sub s 0 index
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
 let rec helper list points index =
   match list with
   | [] -> points
   (* Checks if the user_preference is included in the row in question 
    * If it is a substring of the row, then 10 points are added
    * Else, no points are added*)
   | user_preference :: tail when (isPreference_Substring_Of_CurrentRow (String.lowercase_ascii current_row) user_preference = true && index = 0) -> helper tail (points + 50) (index+1)
   | user_preference :: tail when (isPreference_Substring_Of_CurrentRow (String.lowercase_ascii current_row) user_preference = true && index <> 0) -> helper tail (points + 10) (index + 1)
   | user_preference :: tail when (isPreference_Substring_Of_CurrentRow (String.lowercase_ascii current_row) user_preference = false) -> helper tail points (index + 1)
   | _ :: _ -> points
 in
 helper finalListFromResultArray 0 0
;;

(* Calls the calculatePoints function over each restaurant/row
* Returns a list of tuples as follows [(Restaurant1 Name, Points1) ; (Restaurant2 Name, Points2) ]*)
let rec createRestaurantsList restaurant_file_from_csv finalListFromResultArray = 
 match input_line restaurant_file_from_csv with
 | exception End_of_file -> []
 | line -> (line ,calculatePoints line finalListFromResultArray) :: createRestaurantsList restaurant_file_from_csv finalListFromResultArray
;;

let split_on_comma str =
 String.split_on_char ',' str
;;

let generateSuggestions finalListFromResultArray =

 (* flush and close the channel *)
 (* Read file and display the first line *)
 if (finalListFromResultArray <> [])
    then
   let restaurant_file_from_csv = open_in "Restaurants.csv" in

   let restaurants_points = createRestaurantsList restaurant_file_from_csv finalListFromResultArray in

   (* Sort, in descending order, the restaurants based on their calculated points *)
   let sorted_restaurants_points = List.sort (fun (_, point1) (_, point2) -> compare point2 point1) restaurants_points in

   for i = 0 to 2 do
     let list_of_restaurant_info = split_on_comma(fst (List.nth sorted_restaurants_points i)) in
     printf "Restaurant #%d I recommend for you is %s\n" (i+1) (List.nth list_of_restaurant_info 0);
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