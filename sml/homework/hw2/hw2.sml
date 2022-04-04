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

(* you may assume that Num is always used with values 2, 3, ..., 10
   though it will not really come up *)
datatype suit = Clubs | Diamonds | Hearts | Spades
datatype rank = Jack | Queen | King | Ace | Num of int 
type card = suit * rank

datatype color = Red | Black
datatype move = Discard of card | Draw 

exception IllegalMove

(* put your solutions for problem 2 here *)

fun card_color(card, _) = 
   case card of
      Spades => Black
      | Clubs => Black
      | Diamonds => Red
      | Hearts => Red

fun card_value(_, cardRank) =
   case cardRank of
      Num x => x
      | Ace => 11
      | Jack => 10
      | King => 10

fun remove_card(cs, c, e) =
   let
      fun help(h_cs, acc, flag) = 
         case h_cs of
            [] => acc
            | h_c::h_cs' => 
               if h_c = c andalso flag
               then help(h_cs', acc, false)
               else help(h_cs', h_c :: acc, flag)
      val remainCards = help(cs, [], true)
   in
      if remainCards = cs
      then raise e
      else remainCards
   end

fun all_same_color(cs) = 
   let
      fun help(h_cs, color, flag) = 
         case h_cs of 
            [] => true
            | c::h_cs' => 
               let
                  val curColor = card_color(c)
               in
                  if flag orelse curColor = color
                  then help(h_cs', curColor, false)
                  else false
               end
   in
      help(cs, Red, true)
   end

fun sum_cards(cs) = 
   let
      fun help(h_cs, sum) =
         case h_cs of
            [] => sum
            | c::h_cs' =>
               help(h_cs', card_value(c) + sum)
   in
      help(cs, 0)
   end

fun score(cs, goal) = 
   let
      val sum = sum_cards(cs)
      val preliminary_score = 
         if sum > goal 
         then 3 * (sum - goal) 
         else goal - sum
   in
      if all_same_color(cs)
      then preliminary_score div 2
      else preliminary_score
   end

fun officiate(cs, moves, goal) =
   let
      fun help(cs, moves, helds) =
         case moves of
            [] => score(helds, goal)
            | move::moves' =>
               case move of
                  Discard c => 
                     help(remove_card(cs, c, IllegalMove),
                          moves', helds)
                  | Draw => 
                     case cs of
                        [] => score(helds, goal)
                        | c::cs' =>
                           if sum_cards(c::helds) > goal
                           then score(c::helds, goal)
                           else help(cs', moves', c::helds)

   in
      help(cs, moves, [])
   end
