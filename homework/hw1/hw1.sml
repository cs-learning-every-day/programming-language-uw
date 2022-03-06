
fun get_year(d: int*int*int) = 
    #1 d

fun get_month(d: int*int*int) = 
    #2 d

fun get_day(d: int*int*int) = 
    #3 d

fun is_older(d1: int*int*int, d2: int*int*int) =
    let 
        val y1 = get_year(d1)
        val m1 = get_month(d1)
        val y2 = get_year(d2)
        val m2 = get_month(d2)
    in
        (* year *)
        if not (y1 = y2)
        then y1 < y2
        (* month *)
        else if not (m1 = m2)
        then m1 < m2
        (* day *)
        else get_day(d1) < get_day(d2)
    end


fun number_in_month(dates: (int*int*int) list, month: int) =
    if null dates
    then 0
    else number_in_month(tl(dates), month) + (if (get_month(hd(dates)) = month) then 1 else 0)


fun number_in_months(dates: (int*int*int) list, months: int list) =
    if null months
    then 0
    else number_in_month(dates, hd(months)) + number_in_months(dates, tl(months))

fun dates_in_month(dates: (int*int*int) list, month: int) = 
    if null dates
    then []
    else  if get_month(hd(dates)) = month
    then hd(dates) :: dates_in_month(tl(dates), month)
    else dates_in_month(tl(dates), month)

fun dates_in_months(dates: (int*int*int) list, months: int list) =
    if null months
    then []
    else dates_in_month(dates, hd(months)) @ dates_in_months(dates, tl(months))


fun get_nth(strs: string list, n: int) = 
    if n = 1
    then hd(strs)
    else get_nth(tl(strs), n - 1)

fun date_to_string(date : int*int*int) = 
    let
        val month_names = ["January", "February", "March", 
            "April", "May", "June", 
            "July", "August", "September", 
            "October", "November", "December"]
    in
        get_nth(month_names, get_month(date)) ^ " " ^ 
        Int.toString(get_day(date)) ^ ", " ^ 
        Int.toString(get_year(date))
    end

fun number_before_reaching_sum(sum: int, nums: int list) = 
    if sum <= hd(nums)
    then 0
    else 1 + number_before_reaching_sum(sum - hd(nums), tl(nums))

fun what_month(day: int) = 
    let
        val l = [31, 28, 31, 30, 31, 30, 31, 31, 30, 31, 30, 31]
    in
        number_before_reaching_sum(day, l) + 1
    end

fun month_range(day1: int, day2: int) = 
    if day1 > day2 
    then []
    else what_month(day1) :: month_range(day1 + 1, day2)

fun oldest(dates: (int*int*int) list) = 
    if null dates
    then NONE
    else let
        fun help(l : (int*int*int) list) =
            if null (tl(l))
            then hd(l)
            else let 
                val a = hd(l)
                val b = help(tl(l))
            in
                if is_older(a, b) then a else b
            end
    in
        SOME (help(dates))
    end
