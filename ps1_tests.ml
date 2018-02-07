(*
			 CS 51 Problem Set 1
		Core Functional Programming -- Testing
			     Spring 2017
 *)			     

open Ps1 ;;

(* Reversed tests *)
let () = assert ((reversed []) = true);;
let () = assert ((reversed [1]) = true);;
let () = assert ((reversed [2; 1]) = true);;
let () = assert ((reversed [1; 2]) = false);;
let () = assert ((reversed [1; 1]) = true);;
let () = assert ((reversed [5; 3; 4; 2; 1]) = false);;
let () = assert ((reversed [5; 5; 5; 5; 5; 1]) = true);;
let () = assert ((reversed [5; 5; 5; 5; 5; 1; 5]) = false);;

(* Merge tests from distribution code *)
let () = assert ((merge [1;2;3] [4;5;6;7]) = [1;2;3;4;5;6;7]);;
let () = assert ((merge [4;5;6;7] [1;2;3]) = [1;2;3;4;5;6;7]);;
let () = assert ((merge [4;5;6;7] [1;2;3]) = [1;2;3;4;5;6;7]);;
let () = assert ((merge [2;2;2;2] [1;2;3]) = [1;2;2;2;2;2;3]);;
let () = assert ((merge [1;2] [1;2]) = [1;1;2;2]);;
let () = assert ((merge [-1;2;3;100] [-1;5;1001]) = [-1;-1;2;3;5;100;1001]);;
let () = assert ((merge [] []) = []);;
let () = assert ((merge [1] []) = [1]);;
let () = assert ((merge [] [-1]) = [-1]);;
let () = assert ((merge [1] [-1]) = [-1;1]);;

(* Unzip unit tests *)
let () = assert ((unzip []) = ([], []));;
let () = assert ((unzip [(1, 2)]) = ([1], [2]));;
let () = assert ((unzip [(1, -1)]) = ([1], [-1]));;
let () = assert ((unzip [(2, 1); (2, 1)]) = ([2; 2], [1; 1]));;
let () = let ans = ([2; 2; 3; 5], [1; 1; 4; 6]) in
	assert ((unzip [(2, 1); (2, 1); (3, 4); (5, 6)]) = ans);;

(* Variance unit tests. The delta is used to deal with floating point 
impercision *)
let delta = 0.01;;
let () = assert ((variance []) = None);;
let () = assert ((variance [17.]) = None);;

let () = let x = variance [17.; 17.] in 
	assert ((x < (Some (0. +. delta))) && (x > (Some (0. -. delta))));;
let () = let x = variance [1.; 2.; 3.; 4.; 5.] in 
	assert((x < (Some (2.5 +. delta))) && (x > (Some (2.5 -. delta))));;
let () = let x = variance [-5.; 0.; 5.;] in 
	assert((x < (Some (25. +. delta))) && (x > (Some (25. -. delta))));;


(* Few Divisors unit tests *)
let () = assert ((few_divisors 1 1) = false);;
let () = assert ((few_divisors 1 2) = true);;
let () = assert ((few_divisors 4 2) = false);;
let () = assert ((few_divisors 4 3) = false);;
let () = assert ((few_divisors 4 4) = true);;
let () = assert ((few_divisors 24 9) = true);;
let () = assert ((few_divisors 24 8) = false);;
let () = assert ((few_divisors 24 7) = false);;

(* Concat list unit tests *)
let () = assert ((concat_list ", " []) = "");;
let () = assert ((concat_list ", " ["Shieber"]) = "Shieber");;
let () = assert ((concat_list ", " ["Hello"; "World"]) = "Hello, World");;
let () = assert ((concat_list ", " ["Hello"; ""; "World"]) = "Hello, , World");;
let () = assert ((concat_list " " ["I"; "love"; "dogs"]) = "I love dogs");;


(* to_run_length unit tests *)
let () = assert ((to_run_length []) = []);;
let () = let lst = ['z'] in
	assert ((to_run_length lst) = [(1, 'z')]);;
let () = let lst = ['a'; 'a'; 'a'] in
	assert ((to_run_length lst) = [(3, 'a')]);;
let () = let lst = ['a'; 'a'; 'a'; 'a'; 'b'; 'b'; 'a'; 'd'; 'd'; 'd'; 'd'] in
	assert ((to_run_length lst) = [(4, 'a'); (2, 'b'); (1, 'a'); (4, 'd')]);;
let () = let lst = ['a'; 'c'; 'e'; 'e'; 'e'; 'd'; 'd'] in
	assert ((to_run_length lst) = [(1, 'a'); (1, 'c'); (3, 'e'); (2, 'd')]);;

(* from_run_length unit tests *)
let () = assert ((from_run_length []) = []);;
let () = let lst = [(1, 'z')] in
	assert ((from_run_length lst) = ['z']);;
let () = let lst = [(3, 'a')] in
	assert ((from_run_length lst) =  ['a'; 'a'; 'a']);;
let () = let lst = [(3, 'a'); (2, 'b'); (1, 'a'); (2, 'd')] in 
	assert ((from_run_length lst) = ['a'; 'a'; 'a'; 'b'; 'b'; 'a'; 'd'; 'd']);;
let () = let lst = [(1, 'a'); (1, 'c'); (3, 'e'); (2, 'd')] in
	assert ((from_run_length lst) = ['a'; 'c'; 'e'; 'e'; 'e'; 'd'; 'd']);;

