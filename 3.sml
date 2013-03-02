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


(* 1 *)

fun first_char str = String.sub(str, 0)

fun only_capitals(strs) = strs ~> List.filter (fn str => str ~> first_char ~> Char.isUpper)


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


(* 8 *)

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

(* get all the variable strings from a list of patterns *)

fun strings_from_lst (Variable(str)::tail) = str:: strings_from_lst(tail)
  | strings_from_lst (TupleP(lst)::tail) = strings_from_lst(lst) @ strings_from_lst(tail)
  | strings_from_lst (_::tail) = strings_from_lst(tail)
  | strings_from_lst (_) = []
  	
(* get all var strings from a pattern using the helper above *)  	

fun strings_from_pat (Variable(str)) = [str]
  | strings_from_pat (TupleP(lst)) = strings_from_lst(lst)
  | strings_from_pat (_) = []
  
(* check if the list contains only unique entries. Not optimal. In worst case it is almost
   quadratic, might be better to sort the list beforehand. Basically, we could find
   identical entries at the sorting stage. Would be n*log(n) *)

fun unique_in_list([]) = true
  | unique_in_list(str::lst) = 
  	if (List.exists (fn (str_from_list) => str = str_from_list) lst) then
  		false
  	else
  		unique_in_list(lst)

(* now lets pipe pattern to get the strings from it and then check if they are unique *)

fun check_pat(ptrn) = ptrn ~> strings_from_pat ~> unique_in_list  	

(* this is 14 lines of code :*)

	
(* 11 *)

exception DoesntMatch

fun match_list (_, Wildcard) = []
  | match_list (a_value, Variable str) = [(str, a_value)]
  | match_list (Unit, UnitP) = []
  | match_list (Const x, ConstP y) = 
  	if (x = y) then 
  		[]
  	else
  		raise DoesntMatch
  | match_list (Tuple([]), TupleP ps) = raise DoesntMatch
  | match_list (Tuple vs, TupleP([])) = raise DoesntMatch
  | match_list (Tuple (v::vs), TupleP (p::ps)) = 
  	match_list (v, p) @ match_list (Tuple(vs), TupleP(ps))
  | match_list (Constructor (str1, v), ConstructorP(str2, p)) = 
  	if (str1 <> str2) then
  		raise DoesntMatch
  	else
  		match_list(v, p)
  | match_list (_, _) = raise DoesntMatch
  
  
fun match (a_value, patrn) = (
		SOME (match_list(a_value, patrn))
	) 
		handle DoesntMatch => NONE


(* 12 *)

fun first_match a_value ([]) = NONE
  | first_match a_value (patrn::tail) =
  	let 
  		val match_result = match(a_value, patrn)
  	in 
  		if isSome(match_result) then
  			match_result
  		else
  			first_match a_value tail
  	end
  		































