#use "lex.ml"

(* Reads each first string prior to the first comma in each row 
 * in the Restaurant.csv file to save each restaurant in a tuple 
 * for later computation *)
 let readStringUntilFirstComma string =
  let string_length = String.length string in
  let rec loopOverLine index =
    if index = string_length || string.[index] = ',' then
      String.sub string 0 index
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
let calculatePoints current_row list_of_user_preferences =
  let rec helper list restaurant_name points =
    match list with
    | [] -> (restaurant_name, points)
    (* Checks if the user_preference is included in the row in question 
     * If it is a substring of the row, then 10 points are added
     * Else, no points are added*)
    | user_preference :: tail when (isPreference_Substring_Of_CurrentRow current_row user_preference = true) -> helper tail restaurant_name (points + 10)
    | user_preference :: tail when (isPreference_Substring_Of_CurrentRow current_row user_preference = false) -> helper tail restaurant_name points
    | _ :: _ -> (restaurant_name, points)
  in
  helper list_of_user_preferences (readStringUntilFirstComma current_row) 0
;;

(* Calls the calculatePoints function over each restaurant/row
 * Returns a list of tuples as follows [(Restaurant1 Name, Points1) ; (Restaurant2 Name, Points2) ]*)
let rec createRestaurantsList restaurant_file_from_csv = 
  match input_line restaurant_file_from_csv with
  | exception End_of_file -> []
  | line -> (calculatePoints (String.lowercase_ascii line) userPreference) :: createRestaurantsList restaurant_file_from_csv
;;

(* Anonymous function that displays the restaurant recommendations to the user based on the given preferences *)
let () =

  (* flush and close the channel *)

  (* Read file and display the first line *)
  if userPreference
     <> [] then
    let restaurant_file_from_csv = open_in "Restaurants.csv" in

    let restaurants_points = createRestaurantsList restaurant_file_from_csv in

    (* Sort, in descending order, the restaurants based on their calculated points *)
    let sorted_restaurants_points = List.sort (fun (_, point1) (_, point2) -> compare point2 point1) restaurants_points in
      Printf.printf "The first restaurant I recommend for you is %s" (fst (List.nth sorted_restaurants_points 0));
      print_endline "";
      Printf.printf "The second restaurant I recommend for you is %s" (fst (List.nth sorted_restaurants_points 1));
      print_endline "";
      Printf.printf "The third restaurant I recommend for you is %s" (fst (List.nth sorted_restaurants_points 2));

      (* write the result to stdout *)
      flush stdout;
      close_in restaurant_file_from_csv

(*The first restaurant I recommend to you is

Restaurant name: 
Rating:
Popular Dish:
Location:
Hotline:

*)
