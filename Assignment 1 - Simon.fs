type Species = string
type Location = string
type Time = int
type Observation = Species*Location*Time
let os = [("Owl","L1",3);("Sparrow","L2",4);("Eagle","L3",5);("Falcon","L2",7);("Sparrow","L1",9);("Eagle","L1",14)]
//Task 2

//1) locationsof: Species -> Obsrvation list -> Location list

let rec locationsOf s = function
    | [] -> []
    | (sp,lc,_)::ys when sp=s -> lc::locationsOf s ys
    | x::ys -> locationsOf s ys

//2) 
type Count<'a when 'a:equality> = ('a*int) list


let rec insert a = function
    | (sp,y)::ys -> if sp=a then (sp,y+1)::ys 
                    else (sp,1)::insert a ys
    | [] -> [(a,1)]

//Tests
insert 'b' [('a',1);('b',2)];;
insert 'a' [('a',1);('b',2)];;
insert 1 [('a',1);('b',2)];;

//3) Observation list -> Count<Species>

let rec toCount l =
    match l with
    | (sp,_,_)::ys -> insert sp (toCount ys)
    | [] -> []

//4) 
type Interval = Time*Time

let rec select f intv os = 
    match (intv,os) with
    | ((t1,t2),(sp,lo,t)::ys) -> if t1<=t && t2>=t
                                 then f (sp,lo) :: select f intv ys
                                 else select f intv ys
    | (_,[]) -> []

//5) 

let e = select (fun x -> x) (4,9) os;;

//Task 3

//1)

(* let rec locationsOf s = function
    | [] -> []
    | (sp,lc,_)::ys when sp=s -> lc::locationsOf s ys
    | x::ys -> locationsOf s ys
*)

let rec locationsOf s os = List.filter (fun y -> y = s) os
