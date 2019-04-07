(* Dan Grossman, Coursera PL, HW2 Provided Code *)

(* if you use this function to compare two strings (returns true if the same
   string), then you avoid several of the functions in problem 1 having
   polymorphic types that may be confusing *)
fun same_string(s1 : string, s2 : string) =
    s1 = s2

(* put your solutions for problem 1 here *)

(*function 1*)
fun all_except_option(s: string, l :string list)=
    let fun aux(l1:string list,l2:string list)=
            case l1 of
                []=>NONE
              | head::tail_list => if same_string(head,s)
                            then SOME(l2 @ tail_list)
                            else aux(tail_list, head::l2)
    in
        aux(l,[])
    end
(* function 2 *)
fun get_substitutions1(l:string list list, s : string) =
    let fun aux(l1: string list list,ret_list: string list) =
            case l1 of
                [] => []
              | x::xs => let val y = all_except_option(s,x)
                         in
                             case y of
                                 NONE => ret_list@aux(xs, ret_list)
                               | SOME value => ret_list @ value @ aux(xs, ret_list)
                         end
    in
        aux(l,[])
    end

(* function 3*)
fun get_substitutions2(l: string list list, s : string) =
    case l of
        [] => []
      | x::xs => case all_except_option(s,x) of
                     NONE => get_substitutions2(xs,s)
                          | SOME value =>  value @ get_substitutions2(xs,s)

(* you may assume that Num is always used with values 2, 3, ..., 10
   though it will not really come up *)
datatype suit = Clubs | Diamonds | Hearts | Spades
datatype rank = Jack | Queen | King | Ace | Num of int 
type card = suit * rank

datatype color = Red | Black
datatype move = Discard of card | Draw 

exception IllegalMove

(* put your solutions for problem 2 here *)
