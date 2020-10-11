fun bool_string(x: bool) = 
    if x
        then "true"
    else
        "false";

fun is_older(date1: (int * int * int), date2: (int * int * int)) = 
    if (#1 date1) > (#1 date2)
        then true
    else if (#1 date1) < (#1 date2)
        then false
    else if (#2 date1) > (#2 date2)
        then true
    else if (#2 date1) < (#2 date2)
        then false
    else if (#3 date1) > (#3 date2)
        then true
    else if (#3 date1) < (#3 date2)
        then false
    else 
        false;

fun number_in_month(month: int, dates: (int * int * int) list) =
    if null dates
        then 0
    else if #2 (hd dates) = month
        then 1 + number_in_month(month, tl dates)
    else
        number_in_month(month, tl dates);

fun number_in_months(months: int list, dates: (int * int * int) list) =
    if null months
        then 0
    else
        let val in_month = number_in_month(hd months, dates)
        in 
            in_month + number_in_months(tl months, dates)
        end;

fun numbers_before_reaching_sum(sum: int, nums: int list) = 
    if hd nums >= sum
        then 0
    else
        1 + numbers_before_reaching_sum(sum - hd nums, tl nums);

fun oldest(dates: (int * int * int) list) =
    if null dates
        then NONE
    else if null (tl dates)
        then SOME (hd dates)
    else
        let val max_tail = oldest(tl dates)
        in
            if is_older(hd dates, valOf max_tail)
                then SOME (hd dates)
            else 
                max_tail
        end;

oldest([(1,1,1),(1,1,1),(100,100,100),(99, 99, 99)])