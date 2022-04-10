(* Coursera Programming Languages, Homework 3, Provided Code *)

exception NoAnswer

datatype pattern = Wildcard
		 | Variable of string
		 | UnitP
		 | ConstP of int
		 | TupleP of pattern list
		 | ConstructorP of string * pattern

datatype valu = Const of int
	      | Unit
	      | Tuple of valu list
	      | Constructor of string * valu

fun g f1 f2 p =
    let 
	val r = g f1 f2 
    in
	case p of
	    Wildcard          => f1 ()
	  | Variable x        => f2 x
	  | TupleP ps         => List.foldl (fn (p,i) => (r p) + i) 0 ps
	  | ConstructorP(_,p) => r p
	  | _                 => 0
    end

(**** for the challenge problem only ****)

datatype typ = Anything
	     | UnitT
	     | IntT
	     | TupleT of typ list
	     | Datatype of string

(**** you can put all your code here ****)
fun only_capitals (strs: string list) = 
	List.filter (fn str => Char.isUpper(String.sub(str, 0))) strs

(* val it =  foldl (fn (a,b) => a + b) 0 [1,2,3,4] == 10 *)
fun longest_string1(strs: string list) =
	foldl 
		(fn (str, b) => 
			if String.size str > String.size b
			then str
			else b
		)
		"" strs

fun longest_string2(strs: string list) = 
	foldl 
		(fn (str, b) => 
			if String.size str >= String.size b
			then str
			else b
		)
		"" strs

fun longest_string_helper f strs =
	foldl 
		(fn (str, b) => 
			if f(String.size str, String.size b)
			then str
			else b
		)
	"" strs

val longest_string3 = longest_string_helper (fn (a, b) => a > b)
val longest_string4 = longest_string_helper (fn (a, b) => a >= b)

val longest_capitalized = fn strs => longest_string1(only_capitals strs)

fun rev_string(str: string) = 
	(String.implode o rev o String.explode) str

fun first_answer f lst = 
	case lst of
		[] => raise NoAnswer
	   | e::lst' => case f e of
	   		NONE => first_answer f lst'
	   	  | SOME v => v

fun all_answers f lst = 
	let
		fun helper f lst acc = 
			case lst of
				[] => SOME acc
			   | x::lst' => case f x of
			   		 NONE => NONE
				   | SOME v => helper f lst' (v @ acc)
	in
		helper f lst []
	end


fun count_wildcards p = g (fn () => 1) (fn x => 0) p

fun count_wild_and_variable_lengths p = g (fn () => 1) String.size p

fun count_some_var(str, p) = g (fn () => 0) 
	(fn x => if x = str then 1 else 0)
	p 

fun check_pat p = 
	let 
		fun all_variable p1 = 
			case p1 of 
			Variable x => [x]
			| TupleP ps => List.foldl (fn (a, b) => (all_variable a) @ b) [] ps
			| ConstructorP(_, ps) => all_variable ps
			| _ => []
		fun has_repeat strs = 
			case strs of
			[] => false
			| str::strs' => List.exists (fn x => x = str) strs' orelse has_repeat strs'
	in
		 not (has_repeat(all_variable(p)))
	end



fun match (v, p) = 
	case (p, v) of
	 (Wildcard, _) => SOME []
	| (UnitP, Unit) => SOME []
	| (Variable s, tv) => SOME [(s, tv)]
	| (ConstP tp, Const tv) => if tp = tv then SOME [] else NONE
	| (TupleP ps, Tuple vs) => if List.length ps = List.length vs
							   then all_answers match (ListPair.zip(vs, ps))
							   else NONE
	| (ConstructorP (s1, p1), Constructor (s2, p2)) => if s1 = s2
													   then match(p2, p1)
													   else NONE
	| (_, _) => NONE

fun first_match v ps = 
	SOME (first_answer (fn x => match(v, x)) ps) 
	handle NoAnswern => NONE