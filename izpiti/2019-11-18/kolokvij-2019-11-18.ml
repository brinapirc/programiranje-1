(* =================== *)
(* 1. naloga: funkcije *)
(* =================== *)

let is_root int1 int2 =
	if int1 < 0 then false
	else if int1 * int1 = int2 then true
	else false 

let pack3 stevilo vrednost seznam = (stevilo , vrednost, seznam)


(*to ne dela kot bi moralo*)
let rec sum_if_not znak int3 list =
	match list with
	| [] -> 0
	| x::xs -> if int3 znak x then 0 else x + (sum_if_not znak int3 xs)


(*to tudi ne dela kot bi moralo*)
let rec apply list1 list2 =
	let rec apply' list1 list2 acc =
		match list1, list2 with
		| (_::_, []) -> acc
		|([], _) -> acc
		| x::xs, y::ys -> apply' xs ys ((y x) :: acc )
	in apply' [] []


(* ======================================= *)
(* 2. naloga: podatkovni tipi in rekurzija *)
(* ======================================= *)

type vrsta_srecanja = | Predavanja | Vaje

type srecanje = {predmet : string; vrsta : string; trajanje: int}

type urnik = srecanje list list

let vaje = {predmet = "Analiza 2a"; vrsta = "vaje"; trajanje = 3}

let predavanje = {predmet = "Programiranje 1"; vrsta = "predavanja"; trajanje = 2}

let urnik_profesor urnik = srecanje [2;0;1;0;0;1;0] ["vaje"; ""; "predavanja"; ""; ""; "vaje" ""]

let je_preobremenjen () = failwith "dopolni me"

let bogastvo () = failwith "dopolni me"
