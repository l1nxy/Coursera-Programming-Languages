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

fun longest_capitalized (str_list: string list) =
    let val f = longest_string1  o only_capitals
    in
        f str_list
    end
fun rev_string (str : string) =
    implode (List.rev(explode(str)))

fun first_answer f l =
    case l of
        [] => raise NoAnswer
     |  x::xs => case f x of
                    SOME v => v
                  | NONE => first_answer f xs

fun all_answers f l =
    case l of
        [] => SOME[]
      | x::xs => case f x of
                     NONE => NONE
                   | SOME lst => case  all_answers f xs of
                                     NONE => NONE
                                  | SOME l => SOME(l @ lst)

val count_wildcards = g (fn _ => 1 ) (fn _ => 0)
val count_wild_and_variable = g (fn _ => 1) (fn x => String.size(x))
fun count_wild_and_variable_lengths x = count_wildcards x + count_wild_and_variable x


fun count_some_var (str, p) = g (fn _ => 1) (fn x =>
                                                if String.isSubstring str x
                                                then 1
                                                else 0) p

fun check_pat(p :pattern) =
    let fun filter_str p acc =
                case p of
                    Variable x => x::acc
                  | ConstructorP (str,p) => str::acc
                  | TupleP l => List.foldl (fn (p1,acc) => (filter_str p1 []) @ acc ) [] l
                             | _ => []
    in
        let val l = filter_str p []
            fun dedup li=
            case li of
                [] => true
             | x::xs => (List.exists (fn z => x = z ) l) andalso dedup xs
        in
            dedup l
        end
    end

fun match (v,p) =
    case p of
        Wildcard => SOME []
      | UnitP => (case v of Unit => SOME []
                          | _ =>NONE )
      | Variable str => SOME[(str,v)]
      | ConstP i => (case v of Const j => if i=j then SOME []
                                          else NONE
                            | _ => NONE
                    )
      | TupleP plist => (case v of
                             Tuple vlist => if List.length plist = List.length vlist
                        then all_answers match (ListPair.zip(vlist,plist))
                        else NONE
                           | _ => NONE)
      | ConstructorP (str, pt)=> (case v of Constructor (vstr,vval) => if str = vstr then match ( vval,pt) else NONE
                                         | _ => NONE)
fun first_match v lp =
    SOME(first_answer (fn p => match ( v,p)) lp)
        handle NoAnswer => NONE
