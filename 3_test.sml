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





(* 8 *)
fun test_all_answers() =
	let
		fun test_function arg = 
			if ((arg mod 5) > 0) then
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


	
	
	
(* 9 *)	
fun test_count_whildcards() =
	let
		val wildcards = Wildcard
		val wildcards_2 = TupleP ([Wildcard, TupleP([Wildcard, ConstP(10)])])
		val test = [
			count_wildcards wildcards = 1,
			count_wildcards wildcards_2 = 2
		]
	in
		check(test)	
	end
	
(* b *)	
fun test_count_whild_and_variable_lengths() =
	let
		val wildcards = Wildcard
		val wildcards_2 = TupleP ([Wildcard, TupleP([Wildcard, ConstP(10)])])
		val wildcards_6 = TupleP ([Wildcard, Variable("Hello")])
		val wildcards_5 = Variable("Hello")
		val test = [
			count_wild_and_variable_lengths wildcards = 1,
			count_wild_and_variable_lengths wildcards_2 = 2,
			count_wild_and_variable_lengths wildcards_6 = 6, 
			count_wild_and_variable_lengths wildcards_5 = 5
		]
	in
		check(test)	
	end


(* c *)	
fun test_count_some_var() =
	let
		val wildcards = Wildcard
		val wildcards_2 = TupleP ([Wildcard, TupleP([Wildcard, ConstP(10)])])
		val wildcards_6 = TupleP ([Wildcard, Variable("Hello"), Variable("a"), Variable("b")])
		val wildcards_5 = Variable("Hello")
		val wildcards_0 = TupleP([Variable("a"), Variable("b")])
		val test = [
			count_some_var ("Hello", wildcards) = 0,
			count_some_var ("Hello", wildcards_2) = 0,
			count_some_var ("Hello", wildcards_6) = 1, 
			count_some_var ("Hello", wildcards_5) = 1,
			count_some_var ("Hello", wildcards_0) = 0
		]
	in
		check(test)	
	end

	
	
(* 10 *)
fun test_strings_from_pat() =
	let
		val wildcards = Wildcard
		val wildcards_2 = TupleP ([Wildcard, TupleP([Wildcard, ConstP(10)])])
		val wildcards_6 = TupleP ([Wildcard, Variable("Hello"), Variable("a"), Variable("b")])
		val wildcards_5 = TupleP ([Variable("Hello"), TupleP ([Wildcard, Variable("Hello"), Variable("a"), Variable("b")]), Variable("Bye")])
		val test = [
			strings_from_pat (wildcards) = [],
			strings_from_pat (wildcards_2) = [],
			strings_from_pat (wildcards_6) = ["Hello", "a", "b"], 
			strings_from_pat (wildcards_5) = ["Hello", "Hello", "a", "b", "Bye"]
		]
	in
		check(test)	
	end

	
fun test_unique_in_list() =
	let
		val test = [
			unique_in_list(["Hello", "Hello", "a", "b", "Bye"]) = false,
			unique_in_list(["Hello", "a", "b", "Bye"]) = true
		]
	in
		check(test)	
	end
	
	
fun test_check_pat() =
	let
		val wildcards = Wildcard
		val wildcards_2 = TupleP ([Wildcard, TupleP([Wildcard, ConstP(10)])])
		val wildcards_6 = TupleP ([Wildcard, Variable("Hello"), Variable("a"), Variable("b")])
		val wildcards_5 = TupleP ([Variable("Hello"), TupleP ([Wildcard, Variable("Hello"), Variable("a"), Variable("b")]), Variable("Bye")])
		val test = [
			check_pat (wildcards) = true,
			check_pat (wildcards_2) = true,
			check_pat (wildcards_6) = true, 
			check_pat (wildcards_5) = false
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
val test_count_whildcards_ = test_count_whildcards();
val test_count_whild_and_variable_lengths_ = test_count_whild_and_variable_lengths();
val test_count_some_var_ = test_count_some_var();
val test_strings_from_pat_ = test_strings_from_pat();
val test_unique_in_list_ = test_unique_in_list();
val test_check_pat_ = test_check_pat();

(*


val test_ =  ; 

*)