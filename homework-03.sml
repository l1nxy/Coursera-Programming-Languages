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

fun only_capitals (l : string list) =
  List.filter (fn str => Char.isUpper(String.sub(str,0))) l



fun longest_string1 (str_list :string list) =
    List.foldl (fn (x,init) => if String.size(x) > String.size(init) then x else init) "" str_list

fun longest_string2 (str_list :string list) =
    List.foldl (fn (x,init) => if String.size(x) >= String.size(init) then x else init) "" str_list



fun longest_string_helper f x = List.foldl(fn (str,init) => if f(String.size(str),String.size(init)) then str else init) "" x


fun longest_string3 (str_list: string list) =
    let val f = longest_string_helper (fn (x,y) => if x > y then true else false) 
    in
        f str_list
    end

fun longest_string4 (str_list:string list) =
    let val f = longest_string_helper (fn (x,y) => if x >= y then true else false) 
    in
        f str_list
    end

fun  =
