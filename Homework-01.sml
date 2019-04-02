fun is_older(d1:int*int*int,d2 : int*int*int)=
    if #1 d1 > #1 d2
    then false
    else if #1 d1 < #1 d2 then true
    else if #1 d1 = #1 d2 andalso #2 d2 < #2 d2 then true
    else if #1 d1 = #1 d1 andalso #2 d1 = #2 d2 andalso #3 d1 < #3 d2 then true else false

fun number_in_month(list_date:(int * int * int) list, mon:int) =
    if null list_date
    then 0
    else
        if #2 (hd list_date) = mon
        then 1 + number_in_month(tl list_date,mon)
        else number_in_month(tl list_date,mon);

fun number_in_months(ld:(int*int*int) list,mon:int list) =
    if null mon
    then 0
    else
        number_in_month(ld,hd mon) + number_in_months(ld,tl mon)

fun dates_in_month(ld: (int * int * int) list,mon: int) =
    if null ld
    then []
    else
        if #2 (hd ld) = mon
        then hd ld::dates_in_month(tl ld,mon)
        else dates_in_month(tl ld,mon)

fun dates_in_months(ld:(int * int *int) list, mon : int list) =
    if null mon
    then []
    else
        dates_in_month(ld, hd mon) @ dates_in_months(ld, tl mon)

fun get_nth(str : string list,index :int) =
    if index <= 1
    then hd str
    else get_nth(tl str, index - 1)

fun date_to_string(date:int*int*int) =
    let val list_of_months=["January", "February", "March", "April","May", "June", "July", "August", "September", "October", "November", "December"]
    in get_nth(list_of_months, #2 date) ^ " " ^ Int.toString(#3 date) ^ ", " ^ Int.toString(#1 date)
    end

fun number_before_reaching_sum(sum:int, l1:int list)=
    if sum <= 0
    then 0-1
    else 1+number_before_reaching_sum(sum-(hd l1),tl l1)

fun what_month(day:int) =
    let val list_of_days = [31,28,31,30,31,30,31,31,30,31,30,31]
    in
        number_before_reaching_sum(day, list_of_days) + 1
    end

fun month_range(day1:int, day2:int) =
    if day2 < day1
    then []
    else
        if day1 <= day2
        then what_month(day1)::month_range(day1+1,day2)
        else []


fun oldest(date:(int * int * int) list) =
    if null date
    then NONE
    else
        let val oldest_date = oldest(tl date)
        in
            if isSome oldest_date andalso is_older(valOf oldest_date,hd date)
            then oldest_date
            else SOME(hd date)
        end
