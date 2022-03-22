(* Dan Grossman, Coursera PL, HW2 Provided Code *)

(* if you use this function to compare two strings (returns true if the same
   string), then you avoid several of the functions in problem 1 having
   polymorphic types that may be confusing *)
fun same_string(s1 : string, s2 : string) =
    s1 = s2

(* put your solutions for problem 1 here *)
fun all_except_option(str, strs) =
   let
      fun help(h_strs) = 
         case h_strs of
            [] => []
            | s::strs' => 
               if same_string(s, str)
               then help(strs')
               else s :: help(strs')
      val remain_strs = help(strs)
   in
      if remain_strs = strs
      then NONE
      else SOME remain_strs
   end

fun get_substitutions1(strss, s) =
   case strss of
      [] => []
      | strs::strss' => 
         case all_except_option(s, strs) of
            NONE => get_substitutions1(strss', s)
            | SOME x => x @ get_substitutions1(strss', s)

fun get_substitutions2(strss, s) =
   let
      fun help(h_strss, res) = 
         case h_strss of
            [] => res
            | strs::strss' =>
               case all_except_option(s, strs) of
                  NONE => help(strss', res)
                  | SOME x => 
                        help(strss', res @ x)
   in
      help(strss, [])
   end

fun similar_names(strss, fullName) =
   let
      fun help(strs) =
         case fullName of 
            {first=x,last=y,middle=z} => 
               case strs of
                  [] => []
                  | str::strs' => 
                     {first=str,last=y,middle=z} :: help(strs')

      val res = 
         case fullName of 
            {first=x,last=_,middle=_} => get_substitutions2(strss, x)
   in
      fullName :: help(res)
   end

(* (* you may assume that Num is always used with values 2, 3, ..., 10
   though it will not really come up *)
datatype suit = Clubs | Diamonds | Hearts | Spades
datatype rank = Jack | Queen | King | Ace | Num of int 
type card = suit * rank

datatype color = Red | Black
datatype move = Discard of card | Draw 

exception IllegalMove

(* put your solutions for problem 2 here *)
x1.00 *)