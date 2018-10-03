//Task 2
//Given declarations
type Species = string;;
type Location = string;;
type Time = int;;
type Observation = Species * Location * Time;;

let os = [("Owl","L1",3); ("Sparrow","L2",4); ("Eagle","L3",5);
         ("Falcon","L2",7); ("Sparrow","L1",9); ("Eagle","L1",14)];;

//Part 1
let rec locationsOf s = function
    | (x,l,_):Observation::xs -> if x=s then l::(locationsOf s xs)
                                 else locationsOf s xs
    | [] -> [];;
//tests
locationsOf "Falcon" os;; //Expected "L2" is list
locationsOf "Eagle" os;; //Expected "L3" and "L1" in list
locationsOf "sparrow" os;; //Expected no match. Empty list

//Part 2
type Count<'a when 'a:equality> = ('a*int) list;;

//Version 1.2

let rec insert a = function
    | [] -> [(a,1)]
    | (x,y)::ys -> if a=x then (x,y+1)::ys
                   else (x,y)::(insert a ys);;

insert "a" [("a",1); ("b",3); ("c",2)];; //Expected to increment c as such: (a,c+1)
insert "d" [("a",1); ("b",3); ("c",2)];; //Expected to add a new element in list as such: (a,1)
insert 1 [("a",1); ("b",3); ("c",2)];; //Expected error, given 1 is not same type as first element in tupels

//Part 3
let rec toCount xs =
    match xs with
    | [] -> []
    | (st,_,_)::xs' -> insert st (toCount xs');;

toCount os;; //Expected to make an occurence count list for the animals in os
toCount ["a";"b";"c"];; //Excepted error, given the type of the list is not of type Observation

//Part 4

type Interval = Time * Time;;
let rec select f intv os =
    match (intv,os) with
    | ((t1,t2),((st,lo,t)::ts)) -> if t>=t1 && t<=t2
                                   then f (st,lo) :: select f intv ts
                                   else select f intv ts
    | (_,[]) -> [];;

//Part 5
let e = select (fun y -> y) (4,9) os;;

//Task 3

//Part 1


//Part 3
let unique = os |> Seq.distinct |> List.ofSeq;;