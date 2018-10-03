// Task 2
// Given declarations
type Species = string;;
type Location = string;;
type Time = int;;
type Observation = Species * Location * Time;;

let os = [("Owl","L1",3); ("Sparrow","L2",4); ("Eagle","L3",5);
         ("Falcon","L2",7); ("Sparrow","L1",9); ("Eagle","L1",14)];;

// Part 1 - locationOf function.
let rec locationsOf s = function
    | [] -> []
    | (sp,lc,_)::ys when sp=s -> lc::locationsOf s ys
    | x::ys -> locationsOf s ys;;

// Tests
locationsOf "Falcon" os;; //Expected "L2" is list
locationsOf "Eagle" os;; //Expected "L3" and "L1" in list
locationsOf "sparrow" os;; //Expected no match. Empty list

// Part 2 - insert function
type Count<'a when 'a:equality> = ('a*int) list;;

let rec insert a = function
    | (x,y)::ys -> if a=x then (x,y+1)::ys
                   else (x,y)::(insert a ys)
    | [] -> [(a,1)];;

// Tests
insert "a" [("a",1); ("b",3); ("c",2)];; //Expected to increment c as such: (a,c+1)
insert "d" [("a",1); ("b",3); ("c",2)];; //Expected to add a new element in list as such: (a,1)

// Part 3
let rec toCount ys =
    match ys with
    | (sp,_,_)::ys' -> insert sp (toCount ys')
    | [] -> [];;

// Tests
toCount os;; 
(*
    Expected to make an occurence count list for the animals in os,
    given in the specified form.
*)
toCount ["a";"b";"c"];; //Excepted error, given the type of the list is not of type Observation

// Part 4
type Interval = Time * Time;;

let rec select f intv os = 
    match intv, os with
    | (t1,t2), (a,b,c)::xs -> if (t1 <= c && c<= t2) 
                              then [f (a,b)]@(select f intv xs) 
                              else select f intv xs
    | _,xs -> [];;

// Part 5
let e = select (fun x -> x) (4,9) os;;
(*
    Expected output: List of tuples containing observations within the interval
    4 <= x <=9 from os. 
*)

// Task 3 High-order functions from the List library.
// Part 1 
let locationsOf2 s os = List.map(fun (a,b,c) -> b) (List.filter (fun (sp,l,t) -> sp=s) os);;

// Tests
locationsOf2 "Owl" os;; //Expected "L2" is list
locationsOf2 "Sparrow" os;; //Expected "L3" and "L1" in list
locationsOf2 "owl" os;; //Expected no match. Empty list

let rec toCount2 os = List.foldBack insert (List.map (fun (a,b,c) -> a) os) [];;

// Tests
toCount2 os;; 
(*
    Expected to make an occurence count list for the animals in os,
    given in the specified form.
*)
toCount2 ["a";"b";"c"];; //Excepted error, given the type of the list is not of type Observation