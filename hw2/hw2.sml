(* CSE 341, HW2 Provided Code *)

(* main datatype definition we will use throughout the assignment *)
datatype json =
         Num of real (* real is what SML calls floating point numbers *)
       | String of string
       | False
       | True
       | Null
       | Array of json list
       | Object of (string * json) list

(* some examples of values of type json *)
val json_pi    = Num 3.14159
val json_hello = String "hello"
val json_false = False
val json_array = Array [Num 1.0, String "world", Null]
val json_obj   = Object [("foo", json_pi), ("bar", json_array), ("ok", True)]

(* some provided one-liners that use the standard library and/or some features
   we have not learned yet. (Only) the challenge problem will need more
   standard-library functions. *)

(* dedup : string list -> string list -- it removes duplicates *)
fun dedup xs = ListMergeSort.uniqueSort String.compare xs

(* strcmp : string * string -> order compares strings alphabetically
   where datatype order = LESS | EQUAL | GREATER *)
val strcmp = String.compare                                        
                        
(* convert an int to a real *)
val int_to_real = Real.fromInt

(* absolute value of a real *)
val real_abs = Real.abs

(* convert a real to a string *)
val real_to_string = Real.toString

(* return true if a real is negative : real -> bool *)
val real_is_negative = Real.signBit

(* We now load a file with police data represented as values of type json:
   small_incident_reports (10 reports).
*)

(* Make SML print a little less while we load a bunch of data. *)
       ; (* this semicolon is important -- it ends the previous binding *)
Control.Print.printDepth := 3;
Control.Print.printLength := 3;

use "parsed_small_police.sml";




(* Now make SML print more again so that we can see what we're working with. *)
; Control.Print.printDepth := 20;
Control.Print.printLength := 20;



(**** PUT PROBLEMS 1-8 HERE ****)

fun make_silly_json(i) =
   let fun silly_helper(i, record) =
      case i of
         0 => record 
      |  int => let val new_obj = Object [(real_to_string(int_to_real(i)), True)]
            in
                  silly_helper(i - 1, record @ [new_obj])
            end
   in
      silly_helper(i, [])
   end;

fun assoc(k, xs) =
   case xs of
      [] => NONE
   |  (k_match, value)::tail => if k_match = k then SOME value else assoc(k, tail);
   

fun dot(f, j) = 
   case j of
      Object inner => assoc(f, inner)
   |  _ => NONE

fun one_fields(j) =
   let fun fields_acc(j, l) =
      case j of
         Object [] => l
      |  Object ((first, second)::tail) => first::fields_acc(Object tail, l)
      | _ => []
   in 
      fields_acc(j, [])
   end;


(* histogram and historgram_for_field are provided, but they use your 
   count_occurrences and string_values_for_field, so uncomment them 
   after doing earlier problems *)

(* histogram_for_field takes a field name f and a list of objects js and 
   returns counts for how often a string is the contents of f in js. *)
(*
exception SortIsBroken

fun histogram (xs : string list) : (string * int) list =
  let
    fun compare_strings (s1 : string, s2 : string) : bool = s1 > s2

    val sorted_xs = ListMergeSort.sort compare_strings xs
    val counts = count_occurrences (sorted_xs,SortIsBroken)

    fun compare_counts ((s1 : string, n1 : int), (s2 : string, n2 : int)) : bool =
      n1 < n2 orelse (n1 = n2 andalso s1 < s2)
  in
    ListMergeSort.sort compare_counts counts
  end

fun histogram_for_field (f,js) =
  histogram (string_values_for_field (f, js))
*)

(**** PUT PROBLEMS 9-11 HERE ****)

;Control.Print.printDepth := 3;
Control.Print.printLength := 3;

(**** PUT PROBLEMS 12-15 HERE ****)

;Control.Print.printDepth := 20;
Control.Print.printLength := 20;

(**** PUT PROBLEMS 16-19 HERE ****)




