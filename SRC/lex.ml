#use "rules.ml"

(* Create a result array that has all user input in each list
 * Any lexed token has an index in the resultArray
 * If there are too many tokens of the same rule, 
 * then they are in a list at the given index*)
let resultArray : rules_option list array = Array.make 13 ([None]);;
 
 (* Lexes the user input (explained thoroughly below) *)
let rec lexString userInput =
   let string_length = String.length userInput in
   let rec tokenizeString starting_index =
     if starting_index >= string_length then
       ()
     else
     match userInput.[starting_index] with
     (* Ignore tabs, spaces, new lines, and  punctuations *)
     | ' ' | '\t' | '\n' | '?' | '!' | '.' -> tokenizeString (starting_index + 1)
     (* If the lexer meets a character of type Char it takes all 
      * characters after it until the next character is not of type Char *)
     | char when (isChar char) ->(
       let stopping_index = ref starting_index in
         while !stopping_index < string_length && isChar (Char.lowercase_ascii userInput.[!stopping_index]) do
         incr stopping_index;
       done;
       (* keyword is saved as a substring of the original string from 
          *the calcaulated index above *)
         let keyword = removeDuplicatesInString(String.lowercase_ascii (String.sub userInput starting_index (!stopping_index - starting_index))) in
         (* maps the keyword to the rules in the dictionary to identify 
         * it falls under which rule while removing the duplicates 
         * Example: heeyy and helloo are validated *)
         let keyword_index =  get_index_of_keyword (map_stringListList_to_stringList (filterDictionaryByKeyword (convert_dictionary_stringListList () ) keyword) (removeDuplicatesInString(keyword))) in
         match keyword_index with
         | 1 -> resultArray.(1) <- [Some (Recommendation (keyword))] @ resultArray.(1); tokenizeString (!stopping_index)
         | 2 -> resultArray.(2) <- [Some (Hunger (keyword))] @ resultArray.(2); tokenizeString (!stopping_index)
         | 3 -> resultArray.(3) <- [Some (Cuisine (keyword))] @ resultArray.(3); tokenizeString (!stopping_index)
         | 4 -> resultArray.(4) <- [Some (Dine_In (keyword))] @ resultArray.(4); tokenizeString (!stopping_index)
         | 5 -> resultArray.(5) <- [Some (Indoors (keyword))] @ resultArray.(5); tokenizeString (!stopping_index)
         | 6 -> resultArray.(6) <- [Some (Outdoors (keyword))] @ resultArray.(6); tokenizeString (!stopping_index)
         | 7 -> resultArray.(7) <- [Some (Delivery (keyword))] @ resultArray.(7); tokenizeString (!stopping_index)
         | 8 -> resultArray.(8) <- [Some (Takeaway (keyword))] @ resultArray.(8); tokenizeString (!stopping_index)
         | 9 -> resultArray.(9) <- [Some (Restaurant_Location (keyword))] @ resultArray.(9); tokenizeString (!stopping_index)
         | 10 -> resultArray.(10) <- [Some (Yes (keyword))] @ resultArray.(10); tokenizeString (!stopping_index)
         | 11 -> resultArray.(11) <- [Some (No (keyword))] @ resultArray.(11); tokenizeString (!stopping_index)
         | _ -> tokenizeString (!stopping_index)
     )
     (* If the lexer meets a character of type int it takes all
     * characters after it until the next character is not of type int *)
     | digit when (isDigit digit) ->(
       let stopping_index = ref starting_index in
           while !stopping_index < string_length && isDigit userInput.[!stopping_index] do
             incr stopping_index;
           done;
           (* Any number computed is saved in as string and will 
           * represent the Price Range rule *)
           let price_string = String.sub userInput starting_index (!stopping_index - starting_index) in
           (* Price is converted to an int *)
           let price = int_of_string price_string in
           match price with
           | price when price >= 10 && price <= 200 -> resultArray.(12) <- [Some (Price_Range ("$"))] @ resultArray.(12); tokenizeString(!stopping_index)
           | price when price >= 201 && price <= 500 -> resultArray.(12) <- [Some (Price_Range ("$$"))] @ resultArray.(12); tokenizeString(!stopping_index)
           | price when price >= 501 -> resultArray.(12) <- [Some (Price_Range ("$$$"))] @ resultArray.(12); tokenizeString(!stopping_index)
           | _ -> tokenizeString (!stopping_index))
     | _ -> tokenizeString (starting_index + 1)
   in
   tokenizeString 0
 ;;
 
 (* Gets the user input with a displaying message and stores the 
  * lexed input in the variable *)
let lexed_user_input message = () |> getUserInput message |> lexString;;