(* use "/Users/dtv/Documents/Coursera_Programming/2.sml" ;
*)			

(* use "C:/Users/danshin/Documents/Misc/ml/2.ml"; *)


fun check [] = "ok"
  | check (false::xs) = "<----------------------------FAIL---------<<<"
  | check (true::xs) = "." ^ check xs


			
fun test_compare_lists() = 
	check([true,true,false])
			
			
fun test_all_except_option() =
	let
		val test = [
			all_except_option ("hello", ["hello", "dude", "one", "two", "three"]) = SOME ["dude", "one", "two", "three"],
			all_except_option ("one", ["dude", "one", "two", "three"]) = SOME ["dude", "two", "three"],
			all_except_option ("one", ["dude", "two", "three"]) = NONE
		]
	in
		check(test)	
	end

	
(*
fun test_aeo_helper	() =
	let
		val test = [
			aeo_helper (["hello", "dude", "one", "two", "three"], [], "hello") = ["dude", "one", "two", "three"],
			aeo_helper (["dude", "one", "two", "three"], [], "one") = ["dude", "two", "three"],
			aeo_helper (["dude", "two", "three"], [], "one") = []		
			]
	in
		check(test)	
	end	
*)	
fun test_aeo_helper_opt	() =
	let
		val test = [
			aeo_helper_opt (SOME["hello", "dude", "one", "two", "three"], [], "hello") = SOME["dude", "one", "two", "three"],
			aeo_helper_opt (SOME["dude", "one", "two", "three"], [], "one") = SOME["dude", "two", "three"],
			aeo_helper_opt (SOME["dude", "two", "three"], [], "one") = NONE		
			]
	in
		check(test)	
	end	
	
	
	
(*
fun test_i_all_except_option() =
	let
		val test = [
			i_all_except_option ("hello", ["hello", "dude", "one", "two", "three"]) = ["dude", "one", "two", "three"],
			i_all_except_option ("one", ["dude", "one", "two", "three"]) = ["dude", "two", "three"],
			i_all_except_option ("one", ["dude", "two", "three"]) = []
		]
	in
		check(test)	
	end	
*)	
	
	
	
(* b *) 
fun test_get_substitutions1() =
	let
		val test = [
			get_substitutions1([["Fred", "Frederick"], ["Elilzabeth", "Betty"], ["Freddie", "Fred", "F"]], "Fred") = ["Frederick", "Freddie", "F"],
			get_substitutions1([["Fred", "Frederick"], ["Jeff", "Jeffrey"], ["Geoff", "Jeff", "Jeffrey"]], "Jeff") = ["Jeffrey", "Geoff", "Jeffrey"],
			true
		]
	in
		check(test)	
	end
	
	

(* 2 *)

fun test_card_color() =
	let
		val test = [
			card_color(Hearts, Queen) = Red,
			card_color(Diamonds, Queen) = Red,
			card_color(Clubs, Queen) = Black,
			card_color(Spades, Queen) = Black
		]
	in
		check(test)	
	end	
	
fun test_card_value() =
	let
		val test = [
			card_value(Hearts, Num (2)) = 2,
			card_value(Hearts, Num (3)) = 3,
			card_value(Hearts, Num (4)) = 4,
			card_value(Hearts, Num (5)) = 5,
			card_value(Hearts, Num (6)) = 6,
			card_value(Hearts, Num (7)) = 7,
			card_value(Hearts, Num (8)) = 8,
			card_value(Hearts, Num (9)) = 9,
			card_value(Hearts, Num (10)) = 10,
			card_value(Hearts, Jack) = 10,
			card_value(Hearts, Queen) = 10,
			card_value(Hearts, King) = 10,
			card_value(Hearts, Ace) = 11
(*			
			card_value(Hearts, 1) = 10,
			card_value(Hearts, 11) = 10
*)
		]
	in
		check(test)	
	end
	
(* c *)
fun test_rc_helper() =
	let
		val test = [
			rc_helper([(Hearts, Jack), (Hearts, Queen)], [], (Hearts, Queen)) = [(Hearts, Jack)],
			rc_helper([(Hearts, Jack), (Hearts, Queen)], [], (Hearts, Jack)) = [(Hearts, Queen)]
		]
	in
		check(test)	
	end
	
fun test_remove_card() =
	let
		val test = [
			remove_card([(Hearts, Jack), (Hearts, Queen)], (Hearts, Queen), IllegalMove) = [(Hearts, Jack)],
			remove_card([(Hearts, Jack), (Hearts, Queen), (Diamonds, Num(2))], (Hearts, Queen), IllegalMove) = [(Hearts, Jack), (Diamonds, Num(2))],
			remove_card([(Hearts, Jack), (Hearts, Queen)], (Hearts, Jack), IllegalMove) = [(Hearts, Queen)]
		]
	in
		check(test)	
	end



fun test_all_same_color() =
	let
		val test = [
			all_same_color([(Hearts, Jack), (Hearts, Queen), (Hearts, Queen), (Diamonds, Queen)]) = true,
			all_same_color([(Spades, Jack), (Hearts, Queen), (Hearts, Queen), (Diamonds, Queen)]) = false
		]
	in
		check(test)	
	end




(*
fun test_sum_cards() =
	let
		val test = [
			sum_cards([(Hearts, Jack), (Hearts, Queen), (Hearts, Queen), (Diamonds, Queen)]) = 40,
			sum_cards([(Hearts, Num(2)), (Hearts, Num(3)), (Hearts, Num(4)), (Diamonds, Num(5))]) = 14,
			sum_cards([(Hearts, Num(2)), (Hearts, Num(3)), (Hearts, Num(4)), (Diamonds, Num(5)),
			           (Hearts, Num(6)), (Hearts, Num(7)), (Hearts, Num(8)), (Diamonds, Num(9)),
					   (Hearts, Num(10)), (Hearts, Jack), (Hearts, Queen), (Diamonds, King), (Diamonds, Ace)]) = 95
			
		]
	in
		check(test)	
	end
*)


fun test_sum_cards_helper() =
	let
		val test = [
			sum_cards_helper([(Hearts, Jack), (Hearts, Queen), (Hearts, Queen), (Diamonds, Queen)], 0) = 40,
			sum_cards_helper([(Hearts, Num(2)), (Hearts, Num(3)), (Hearts, Num(4)), (Diamonds, Num(5))],0) = 14,
			sum_cards_helper([(Hearts, Num(2)), (Hearts, Num(3)), (Hearts, Num(4)), (Diamonds, Num(5)),
			           (Hearts, Num(6)), (Hearts, Num(7)), (Hearts, Num(8)), (Diamonds, Num(9)),
					   (Hearts, Num(10)), (Hearts, Jack), (Hearts, Queen), (Diamonds, King), (Diamonds, Ace)], 0) = 95
			
		]
	in
		check(test)	
	end


fun test_sum_cards() =
	let
		val test = [
			sum_cards([(Hearts, Jack), (Hearts, Queen), (Hearts, Queen), (Diamonds, Queen)]) = 40,
			sum_cards([(Hearts, Num(2)), (Hearts, Num(3)), (Hearts, Num(4)), (Diamonds, Num(5))]) = 14,
			sum_cards([(Hearts, Num(2)), (Hearts, Num(3)), (Hearts, Num(4)), (Diamonds, Num(5)),
			           (Hearts, Num(6)), (Hearts, Num(7)), (Hearts, Num(8)), (Diamonds, Num(9)),
					   (Hearts, Num(10)), (Hearts, Jack), (Hearts, Queen), (Diamonds, King), (Diamonds, Ace)]) = 95
		]
	in
		check(test)	
	end


fun test_score() =
	let
		val test = [
			score([(Hearts, Jack), (Hearts, Queen), (Hearts, Queen), (Diamonds, Queen)], 20) = 30,
			score([(Hearts, Num(2)), (Hearts, Num(3)), (Spades, Num(4)), (Diamonds, Num(5))], 20) = 6,
			score([(Hearts, Num(2)), (Hearts, Num(3)), (Spades, Num(4)), (Diamonds, Num(5))], 10) = 12,
			score([(Hearts, Num(2)), (Hearts, Num(3)), (Diamonds, Num(4)), (Diamonds, Num(5))], 10) = 6,
			score([(Hearts, Num(2)), (Hearts, Num(3)), (Hearts, Num(4)), (Diamonds, Num(5)),
			           (Hearts, Num(6)), (Hearts, Num(7)), (Hearts, Num(8)), (Diamonds, Num(9)),
					   (Hearts, Num(10)), (Hearts, Jack), (Hearts, Queen), (Diamonds, King), (Diamonds, Ace)], 95) = 0
		]
	in
		check(test)	
	end


fun test_helper_officiate() =
	let
		val cards = [(Hearts, Num(2)), (Hearts, Num(3)), (Hearts, Num(4)), (Diamonds, Num(5)),
				(Hearts, Num(6)), (Hearts, Num(7)), (Hearts, Num(8)), (Diamonds, Num(9)),
				(Hearts, Num(10)), (Hearts, Jack), (Hearts, Queen), (Diamonds, King), (Diamonds, Ace)]
		val moves  = [Draw, Discard (Hearts, Num(2)), Draw, Draw]
		val moves2 = [Draw, Draw, Draw, Discard (Hearts, Num(2)), Draw, Draw]

		val cards2 = [(Hearts, Num(2)), (Spades, Num(3)), (Clubs, Num(4)), (Diamonds, Num(5)),
				(Hearts, Num(6)), (Hearts, Num(7)), (Hearts, Num(8)), (Diamonds, Num(9)),
				(Hearts, Num(10)), (Hearts, Jack), (Hearts, Queen), (Diamonds, King), (Diamonds, Ace)]

		val moves3 = [Draw, Discard (Hearts, Num(2)), Draw, Draw]
		val test = [
			helper_officiate(cards, [], moves, 50) = [(Hearts, Num(4)), (Hearts, Num(3))],
			helper_officiate(cards, [], moves2, 50) = [(Hearts, Num(6)), (Diamonds, Num(5)), (Hearts, Num(3)),
				(Hearts, Num(4))],
			helper_officiate(cards2, [], moves2, 50) = [(Hearts,Num 6),(Diamonds,Num 5),(Spades,Num 3),(Clubs,Num 4)]
		]
	in
		check(test)
	end


fun test_officiate() =
	let
		val cards = [(Hearts, Num(2)), (Hearts, Num(3)), (Hearts, Num(4)), (Diamonds, Num(5)),
				(Hearts, Num(6)), (Hearts, Num(7)), (Hearts, Num(8)), (Diamonds, Num(9)),
				(Hearts, Num(10)), (Hearts, Jack), (Hearts, Queen), (Diamonds, King), (Diamonds, Ace)]
		val cards2 = [(Hearts, Num(2)), (Hearts, Num(3)), (Hearts, Num(4)), (Diamonds, Num(5)),
				(Clubs, Num(6)), (Hearts, Num(7)), (Hearts, Num(8)), (Diamonds, Num(9)),
				(Hearts, Num(10)), (Hearts, Jack), (Hearts, Queen), (Diamonds, King), (Diamonds, Ace)]
		val moves  = [Draw, Discard (Hearts, Num(2)), Draw, Draw]
		val moves2 = [Draw, Draw, Draw, Discard (Hearts, Num(2)), Draw, Draw]
(*		val moves3 = [Draw, Discard (Hearts, Num(2)), Draw, Draw] *)
		val test = [
			officiate(cards, moves, 50) = ((50 - 7) div 2),
			officiate([], moves, 50) = (50),
			officiate(cards, [], 50) = (50),
			officiate(cards, moves2, 10) = ((18 - 10) * 3 div 2),
			officiate(cards2, moves2, 10) = ((18 - 10) * 3)
		]
	in
		check(test)
	end

	
fun test_sum_cards_ch() =
	let
		val test = [
			sum_cards_ch([(Hearts, Num(2)), (Hearts, Num(3)), (Hearts, Num(4)), (Diamonds, Num(5)),
				(Hearts, Num(6)), (Hearts, Num(7)), (Hearts, Num(8)), (Diamonds, Num(9)),
				(Hearts, Num(10)), (Hearts, Jack), (Hearts, Queen), (Diamonds, King), (Diamonds, Ace)], 0, 0, 0) = (95, 1, 0),
			sum_cards_ch([(Hearts, Num(2)), (Hearts, Num(3)), (Hearts, Num(4)), (Diamonds, Num(5)),
				(Hearts, Num(6)), (Hearts, Num(7)), (Hearts, Num(8)), (Diamonds, Num(9)),
				(Hearts, Num(10)), (Hearts, Jack), (Hearts, Ace), (Spades, Ace), (Clubs, Ace)], 0, 0, 0) = (97, 3, 2)
		]
	in
		check(test)	
	end
	
(*

fun test_score_challenge() =
	let
		val test = [
		
		score_challenge([(Hearts, Num(2)), (Hearts, Num(3)), (Hearts, Num(4)), (Diamonds, Num(5)),
				(Hearts, Num(6)), (Hearts, Num(7)), (Hearts, Num(8)), (Diamonds, Num(9)),
				(Hearts, Num(10)), (Hearts, Jack), (Hearts, Ace), (Diamonds, Ace), (Diamonds, Ace)], 50) = 4,
			true
		]
	in
		check(test)	
	end

*)




	
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
	
	
	
	
	
	
	
	
	
	
;
print "\n\n\n";

val test_all_except_option_ =  test_all_except_option(); 
val test_get_substitutions1_ = test_get_substitutions1();
(* val test_aeo_helper_ = test_aeo_helper (); *)
val test_aeo_helper_opt_ = test_aeo_helper_opt ();
val test_card_color_ = test_card_color();
val test_card_value_ = test_card_value();
val test_rc_helper_ = test_rc_helper();
val test_remove_card_ = test_remove_card();
val test_all_same_color_ = test_all_same_color();
val test_sum_cards_helper_ = test_sum_cards_helper();
val test_sum_cards_ = test_sum_cards();
val test_score_ = test_score(); 
val test_helper_officiate_ = test_helper_officiate();
val test_officiate_ = test_officiate();
val test_sum_cards_ch_ = test_sum_cards_ch();
(*
val test_score_challenge_ = test_score_challenge();
*)




(*
val test_i_all_except_option_ = test_i_all_except_option();

*)


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

