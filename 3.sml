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



infix ~>
fun x ~> f = f(x)

fun first_char str = String.sub(str, 0)


(* 1 *)
(* fun only_capitals(strs) = List.filter (fn str => Char.isUpper(String.sub(str,0))) strs
*)

fun only_capitals(strs) = strs ~> List.filter (fn str => str ~> first_char ~> Char.isUpper)

(*
fun helper(str) = str ~> first_char ~> Char.isUpper

fun sqrt_of_abs x = x ~> abs ~> Real.fromInt ~> Math.sqrt
*)
(* 2 *)
fun longest_string1 (strs) = 
	List.foldl (
		fn (str1, str2) => 
			if (size(str2) >= size(str1)) then
				str2
			else 
				str1
		)
		"" strs

(* 3 *)
fun longest_string2 (strs) = 
	List.foldl (
		fn (str1, str2) => 
			if (size(str2) > size(str1)) then
				str2
			else 
				str1
		)
		"" strs


(* 4 *)


fun longest_string_helper comparator =
	let
		fun intern_helper ([]) = ""
		  | intern_helper (str::[]) = str
		  | intern_helper (str1::str2::strs) = 
			 	if (comparator(size str1, size str2)) then
	 				intern_helper(str2::strs)
	 			else
	 				intern_helper(str1::strs)
	 in 
	 	intern_helper
	 end


val longest_string3 = longest_string_helper (fn (len1, len2) => len2 > len1)

val longest_string4 = longest_string_helper (fn (len1, len2) => len2 >= len1)


(*

fun comparator (strlen1, strlen2) =
	strlen1 > strlen2
*)


(* 5 *)
val longest_capitalized = longest_string2 o only_capitals

(* 6 *)
val rev_string = implode o List.rev o explode



(* 7 *)



fun first_answer somefun =
	let
		fun second_fun [] = raise NoAnswer
		  | second_fun (el::alist) = 
		  		if (isSome(somefun(el))) then 
		  			valOf(somefun(el))
		  		else 
		  			second_fun alist
	in
		second_fun	
	end

	

val a = 0;
val b = 1;	
val a' = 0;
val b' = 1;	

print("______________________________________________________\n");

fun all_answers f1 [] = SOME[]
  | all_answers f1 alist = 
	let
		fun iterate ([], acc) = acc
		  | iterate (el::lst, acc) =
				if (isSome(f1 el)) then
					iterate(lst, acc @ valOf(f1 el) )
				else
					[]
		val ret = iterate (alist, [])
	in
		case ret of
			[] => NONE
		  | _  => SOME ret
	end





(* 9 *)
(* a *)

(* 

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
    
    *)


fun count_wildcards patrn =
	let
		fun wildcard_matcher _ = 1
		fun second_matcher _ = 0
	in
		g wildcard_matcher second_matcher patrn
	end


(* b *)

fun count_wild_and_variable_lengths patrn = 
	let 
		fun wildcard_matcher _ = 1
		fun var_matcher str = String.size(str)
	in 
		g wildcard_matcher var_matcher patrn
	end 


(* c *)

fun count_some_var (str, patrn) = 
	let
		fun wildcard_matcher _ = 0
		fun var_matcher str1 = 
			if (str = str1) then
				1
			else
				0
	in
		g wildcard_matcher var_matcher patrn		
	end

(* 10 *)































