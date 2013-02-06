use "3.sml" ;
			

fun check [] = "ok"
  | check (false::xs) = "<----------------------------FAIL---------<<<"
  | check (true::xs) = "." ^ check xs

(* 1 *)
fun test_only_capitals() =
	let
		val test = [
			only_capitals(["A", "b", "AB", "aB"]) = ["A", "AB"], 
			only_capitals(["Hello", "hello"]) = ["Hello"], 
			only_capitals([]) = []

		]
	in
		check(test)	
	end


(* 2 *)
fun test_longest_string1() =
	let
		val test = [
			longest_string1(["One", "two", "three", "four", "five", "six", "Seven"]) = "three",
			longest_string1(["One", "two", "four", "five", "six", "Seven"]) = "Seven",
			longest_string1([]) = ""
			
		]
	in
		check(test)	
	end

(* 3 *)
fun test_longest_string2() =
	let
		val test = [
			longest_string2(["One", "two", "three", "four", "five", "six", "Seven"]) = "Seven",
			longest_string2(["One", "two", "four", "five", "six", "Seven"]) = "Seven",
			longest_string2([]) = ""
			
		]
	in
		check(test)	
	end


(* 4.2 *)
fun test_longest_string3() =
	let
		val test = [
			longest_string3(["One", "two", "three", "four", "five", "six", "Seven"]) = "three",
			longest_string3(["One", "two", "four", "five", "six", "Seven"]) = "Seven",
			longest_string3([]) = ""
			
		]
	in
		check(test)	
	end

(* 4.3 *)
fun test_longest_string4() =
	let
		val test = [
			longest_string4(["One", "two", "three", "four", "five", "six", "Seven"]) = "Seven",
			longest_string4(["One", "two", "four", "five", "six", "Seven"]) = "Seven",
			longest_string4([]) = ""
			
		]
	in
		check(test)	
	end





(* 5 *)
fun test_longest_capitalized() =
	let
		val test = [
			longest_capitalized ["A", "vely rong stling", "Not so long"] = "Not so long",
			longest_capitalized ["A", "vely rong stling", "not so long"] = "A",
			longest_capitalized ["aA", "vely rong stling", "not so long"] = ""
			
		]
	in
		check(test)	
	end

(* 6 *)
fun test_rev_string() =
	let
		val test = [
			rev_string ("Hello, brother") = "rehtorb ,olleH"
		]
	in
		check(test)	
	end



	
fun test_all_answers() =
	let
		fun test_function arg = 
			if (arg < 5) then
				SOME[Int.toString(arg) ^ "--"]
			else 
				NONE


		val test = [
				all_answers test_function [1,2,3] = SOME["1--","2--","3--"],
				all_answers test_function [3,4,5] = NONE,
				all_answers test_function [] = SOME[]
			]
	in
		check(test)	
	end


	
	
(*
fun test_() =
	let
		val test = [
			true
		]
	in
		check(test)	
	end
*)
	
	
(* 7 *)




(* 8 *)

	
	
	
	
	
	
	
	
;
print "\n\n\n";


val test_only_capitals_ = test_only_capitals();
val test_longest_string1_ = test_longest_string1();
val test_longest_string2_ = test_longest_string2();
val test_longest_string3_ = test_longest_string3();
val test_longest_string4_ = test_longest_string4();
val test_longest_capitalized_ = test_longest_capitalized();
val test_rev_string_ = test_rev_string();
val test_all_answers_ =  test_all_answers();



(*


val test_ =  ; 

*)