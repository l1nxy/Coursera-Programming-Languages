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
fun get_substitutions1(l: string list list, s : string) =
    case l of
        [] => []
      | x::xs => case all_except_option(s,x) of
                     NONE => get_substitutions1(xs,s)
                    | SOME value =>  value @ get_substitutions1(xs,s)
(* function 3*)
fun get_substitutions2(l:string list list, s : string) =
    let fun aux(l1: string list list,ret_list: string list) =
            case l1 of
                [] => ret_list
              | x::xs => let val y = all_except_option(s,x)
                         in
                             case y of
                                NONE =>  aux(xs, ret_list)
                               | SOME value => aux(xs, ret_list @ value)
                         end
    in
        aux(l,[])
    end
(* functions 4*)
fun similar_names(l: string list list, name:{first:string,middle:string,last:string}) =
    case name of
        {first = x,middle = y, last = z} => let fun aux(l,ret_list)=
                                                    case l of
                                                        [] => ret_list
                                                      | h::xs => aux(xs,ret_list @ [{first = h,middle = y,last=z}])
                                            in
                                                let val sub_list = [x] @ get_substitutions2(l,x)
                                                in
                                                    aux(sub_list,[])
                                                end
                                            end
(* you may assume that Num is always used with values 2, 3, ..., 10
   though it will not really come up *)
datatype suit = Clubs | Diamonds | Hearts | Spades
datatype rank = Jack | Queen | King | Ace | Num of int 
type card = suit * rank

datatype color = Red | Black
datatype move = Discard of card | Draw 

exception IllegalMove

(* put your solutions for problem 2 here *)

(*function a*)
fun card_color (c:card)=
    case c of
        (Spades,_)=> Black
     |  (Clubs,_) => Black
     |(Diamonds,_) =>Red
     |(Hearts,_) => Red

(* function b*)
fun card_value (c:card) =
    case c of
        (_,Ace) => 11
        |(_, Num i) => i
        | _ => 10

(* function c*)
fun remove_card (cs :card list, c : card,e) =
    let fun aux(cs,l)=
            case cs of
                [] => l
              | x::xs => if c=x
                         then l @ xs
                         else aux(xs,x::l)
    in
        let val new_cs = aux(cs,[])
        in
            if new_cs = cs
            then raise e
            else new_cs
        end
    end

(* function d*)
fun all_same_color(cl:card list) =
    case cl of
        [] => true
      | x::[] => true
      | head::(neck::rest) => card_color(head) = card_color(neck) andalso all_same_color(neck::rest)

(* function e*)
fun sum_cards(cs:card list)=
    let fun aux(l,acc) =
            case l of

                [] => acc
             | x::xs => aux(xs,acc+card_value(x))
    in
        aux(cs,0)
    end

(* function f*)
fun score(cs:card list,goal:int) =
    let val sum = sum_cards(cs)
        val pre_goals =  if sum > goal then 3*(sum - goal) else goal-sum
    in
        if all_same_color(cs)
        then pre_goals div 2
        else pre_goals
    end


(* function g*)
fun officiate(cs:card list,ml :move list,goal:int) =
    let fun aux(l:move list,hc:card list,cl:card list) =
            case l of
                [] => score(hc,goal)
              | x::xs => if sum_cards(hc) > goal
                         then score(hc,goal)
                         else
                         (case x of
                             Discard(c) => aux(xs,remove_card(hc,c,IllegalMove),cl)
                          |  Draw => (case cl of
                                         [] => score(hc, goal)
                                       |  y::ys => aux(xs,y::hc,ys))
                          )
    in
        aux(ml,[],cs)
    end
