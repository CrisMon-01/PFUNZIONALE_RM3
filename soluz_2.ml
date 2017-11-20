let ultime2cifre k =
	if k > 0 then (k mod 100/10 , k mod 10)
	else ((-k) mod 100/10, (-k) mod 10)  ;;

let belle_cifre k = match (abs(k) mod 10) with
 1|2|4|5|6|8|9 -> false
| _ -> match ((abs(k) mod 100)/10) with
	3|7|0 -> false
	| _ -> true

let data_ammissibile num mese = match mese with
1|3|5|7|8|10|12-> num >=1 && num < 32
|4|6|9|11 ->	num >= 1 && num < 31
|2-> num>=1 && num < 29
|_-> false
