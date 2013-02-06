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

exception NoAnswer


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
		
  
	










