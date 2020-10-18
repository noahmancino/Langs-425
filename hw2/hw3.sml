(*
fun g f1 f2 p =
    let
        val r = g f1 f2
    in
        case p of
            WildcardP         => f1 ()
          | VariableP x       => f2 x
          | ConstructorP(_,p) => r p
          | TupleP ps         => List.foldl (fn (p,i) => (r p) + i) 0 ps
          | _                 => 0
    end
*)
fun first_lower xs = Char.isLower (String.sub(xs, 0))
val only_lowercase = List.filter first_lower
fun longer_string (xs, ys) = if String.size xs > String.size ys then xs else ys
val longest_string = List.foldl longer_string ""
fun reverse_longer_string (xs, ys) = if String.size xs < String.size ys then ys else xs
val reverse_longest_string = List.foldl reverse_longer_string ""

fun longest_string_helper comp_fun = 
    fn acc =>
        fn xs =>
            case xs of
                [] => acc
                (* Surely there's a nicer way than putting comp_fun in parens *)
            |    x::xs => longest_string_helper comp_fun (comp_fun(x, acc)) xs;


val four = 4;
fun longest_lower_string(s1, s2) = 
    if first_lower s1 andalso first_lower s2 then longer_string(s1, s2)
    else if first_lower s1 then s1
    else if first_lower s2 then s2
    else "";

fun longest_lowercase xs = List.foldl longer_lower_string "" xs