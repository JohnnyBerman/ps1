(* 
                         CS 51 Problem Set 1
                     Core Functional Programming
                             Spring 2018
 *)

(*======================================================================
Problem 1: Dealing with types

........................................................................
For each of the expressions below (prob0, prob1a, etc.), figure out
the type of the expression. Then write the type in as a string as the
value for prob0_answer, prob1a_answer, etc. The first one, prob0, is
done for you.

  prob0        42
  prob1a       let greet y = "Hello " ^ y in greet "World!"
  prob1b       [Some 4; Some 2; None; Some 3]
  prob1c       ((None, Some 42.0), true)

......................................................................*)

let prob0_answer = "int" ;;

let prob1a_answer = "string" ;;

let prob1b_answer = "int option list" ;;

let prob1c_answer = "'(a option * flot option) * bool" ;;

(*......................................................................
There are several values defined below that do not type check. 

Explain in a comment above each corresponding value why the following
definitions will not type check, and then provide a fixed version of 
each definition as an OCaml value (outside of a comment). Your fix should
change the code minimally.

(Note that the variable names begin with underscore (_) to disable the
warning noting that the values are otherwise unused. You'll want to
leave the underscores in as well.)
......................................................................*)

(*
In Ocaml, the type string * int list is the same as string * (int list), or a 
tuple of a string and an int list. However, we are trying to store a list of 
tuples of type string * int so we should declare it as *)
let _prob1d : (string * int) list = [("CS", 51); ("CS", 50)] ;;
(*
let _prob1d : string * int list = [("CS", 51); ("CS", 50)] ;;
*)
 
(*
	The issue with the code is that add is supposed to take two integers and 
	adds them, but the code below feeds a float and an int into the add 
	function. To fix this issue, we can make  add add together two floats, and 
	turn the 4 into a float either using float_of_int, or just making it 4. . I 
	will use the latter option. This runs us into the final issue that we are 
	compare if the float result of add is equal to the int 10, so we can turn 10 
	into a float as well.
*)
let _prob1e : int =
  let add (x, y) = x +. y in
  if add (4., 3.9) = 10. then 4 else 2 ;;
(*
let _prob1e : int =
  let add (x, y) = x + y in
  if add (4, 3.9) = 10 then 4 else 2 ;;
*)

(*
	The type of the second index in the tuple isn't a string, it is an option 
	due to the None. Since in the non, None cases it is an int, the second type 
	should be int options. We will need to change the ints to int options. 
	Therefore the declaration should be
*)
let _prob1f : (string * int option) list =
  [("January", None); ("February", Some 1); ("March", None); 
   ("April", None); ("May", None); ("June", Some 1); 
   ("July", None); ("August", None); ("September", Some 3);
   ("October", Some 1); ("November", Some 2); ("December", Some 3)] ;;
(*
let _prob1f : (string * string) list =
  [("January", None); ("February", 1); ("March", None); 
   ("April", None); ("May", None); ("June", 1); 
   ("July", None); ("August", None); ("September", 3);
   ("October", 1); ("November", 2); ("December", 3)] ;;
*)

(*======================================================================
Problem 2 - Writing functions

........................................................................
For each subproblem, you must implement a given function, providing
appropriate unit tests in the accompanying file pset1_tests.ml. You
are provided a high level description as well as a type signature of
the function you must implement. Keep in mind the CS51 style guide and
what you've learned so far about efficiency and elegance. You are
*not* allowed to use library functions (i.e., the List module) for
*this* problem unless you implement the functionality yourself.
......................................................................*)

(*......................................................................
Problem 2a: The function "reversed" takes a list of integers and
returns true if the list is in nonincreasing order. The empty list is
considered to be reversed in this sense. Consecutive elements of the
same value are allowed in a reversed list.

For example:

# reversed [1;2;3] ;;
- : bool = false
# reversed [3;2;1] ;;
- : bool = true
# reversed [5;2;2;2;1;1] ;;
- : bool = true

Here is its signature: 

  reversed : int list -> bool

Replace the line below with your own definition of "reversed".
......................................................................*)

(* Determines if a list is in reversed order *)
let rec reversed (lst : int list) : bool =
	match lst with
	| hd :: idx2 :: tail -> hd >= idx2 && (reversed (idx2 :: tail)) 
	| _ -> true;;

(*......................................................................
Problem 2b: The function "merge" takes two integer lists, each
*sorted* in increasing order, and returns a single merged list in
sorted order.  For example:

merge [1;3;5] [2;4;6] ;;
- : int list = [1; 2; 3; 4; 5; 6]
merge [1;2;5] [2;4;6] ;;
- : int list = [1; 2; 2; 4; 5; 6]
merge [1;3;5] [2;4;6;12] ;;
- : int list = [1; 2; 3; 4; 5; 6; 12]
merge [1;3;5;700;702] [2;4;6;12] ;;
- : int list = [1; 2; 3; 4; 5; 6; 12; 700; 702]

Here is its signature:

  merge : int list -> int list -> int list

Replace the line below with your own definition of "merge".
......................................................................*)

(* Merges two ordered lists in order*)
let rec merge (x : int list) (y : int list) =
	match x, y with
	| xhd :: xtl, yhd :: ytl -> 
		if xhd < yhd then xhd :: (merge xtl y) 
		else yhd :: (merge x ytl)
	| _, _ -> x @ y;;

(* I remembered the implementation of merge from the textbook reading, so this 
is probably quite similar*)
(*......................................................................
Problem 2c: The function "unzip", given a list of integer pairs,
returns a pair of lists, the first of which contains each first
element of each pair, and the second of which contains each second
element.  The returned list should have elements in the order in which
they were provided. For example:

unzip [(6,2);(2,4);(5,6)] ;;
- : int list * int list = ([6;2;5],[2;4;6])

Here is its signature:

  unzip : (int * int) list -> int list * int list)

Replace the line below with your own definition of "unzip".
......................................................................*)

(* Unzips a list of tuples into a tuple of lists*)
let rec unzip ls =
	(* Adds elements from a single tuple to a tuple of lists*)
	let add ((lft, rght) : int * int) ((l_lst, r_lst) : int list * int list) =
	 	(lft :: l_lst, rght :: r_lst) in

	match ls with
	| hd :: tl -> add hd (unzip tl)
	| _ -> ([], []);;


(*......................................................................
Problem 2d: The function "variance" takes a float list and returns
None if the list has fewer than two elements. Otherwise, it should
return Some of the variance of the floats. Recall that the variance of
a sequence of numbers is given by the following equation:
                                                
        1/(n-1) * sum (x_i - m)^2

where n indicates the number of elements in the list, m is the
arithmetic mean of the list, and x_i is element in the ith index of
the list. If you want to compare your output with an online
calculator, make sure you find one that calculates the (unbiased)
sample variance.  For example:

variance [1.0; 2.0; 3.0; 4.0; 5.0] ;;
- : float option = Some 2.5
variance [1.0] ;;
- : float option = None

Remember to use the floating point version of the arithmetic operators
when operating on floats (+., *., etc). The function "float" can
convert ("cast") an int to a float.

Here is its signature:

  variance : float list -> float option

Replace the line below with your own definition of "variance".
......................................................................*)

let variance (lst : float list) = 
	(* Fold right could be used for length and sum so I wrote it to avoid
	extra code being written*)
	let rec fold_right f lst = 
		match lst with
		| [] -> 0.
		| hd :: tl -> f hd (fold_right f tl) in

	(* The length of a list *)
	let length lst = fold_right (fun _hd tl -> 1. +. tl) lst in

	(* The sum of a list *)
	let sum (lst : float list) = fold_right (+.) lst in

	(* The mean of a list *)
	let mean (lst : float list) = (sum lst) /. (length lst) in

	(* The square of a float *)
	let square (num : float) = num *. num in

	(* Recursively goes through each element in a list and finds the variance
	of it, and adds it to the variance of the rest of the list. I did it this
	way instead of using variance for it so I could pass in the length and mean
	of the list to avoid extra computation time *)
	let rec vary (lst : float list) (len : float) (mean : float) = 
		match lst with
		| [] -> 0.
		| hd :: tl -> 
			(square (hd -. mean)) /. (len -. 1.) +. (vary tl len mean) in

	if (length lst) > 1. then Some (vary lst (length lst) (mean lst))
	else None;;

(*......................................................................
Problem 2e: The function "few divisors" takes two integers, x and y, and
returns true if x has fewer than y divisors (including 1 and x). Note:
this is *not* the same as x having fewer divisors than y does. For
example: 

few_divisors 17 3 ;;
- : bool = true
few_divisors 4 3 ;;
- : bool = false
few_divisors 4 4 ;;
- : bool = true

Do not worry about zero or negative integers at all. We will not test
your code using zero or negative values for x and y. Do not consider
negative integers for divisors (i.e. -2 being a divisor for 4).

Here is its signature:

  few_divisors : int -> int -> bool 

Replace the line below with your own definition of "few_divisors".
......................................................................*)

(* Returns if the first number has fewer divisors than the second number *)
let few_divisors (x : int) (y : int) = 
	(* Goes from n -> 1 to find the number of divisors n has *)
	let rec find_divisors (num : int) (idx : int) = 
		if idx = 1 then 1 
		else if num mod idx = 0 then 1 + find_divisors num (idx-1) 
			else find_divisors num (idx-1) in

	(find_divisors x x) < y;;

(*......................................................................
Problem 2f: The function "concat_list" takes two arguments: sep, a
string, and lst, a string list. It returns one string with all the
elements of lst concatenated together but separated by the string
sep. For example:

concat_list ", " ["first"; "second"; "third"] ;;
- : string = "first, second, third"
concat_list "..." ["Moo"; "Baa"; "Lalala"] ;;
- : string = "Moo...Baa...Lalala"
concat_list ", " [] ;;
- : string = ""
concat_list ", " ["Moo"] ;;
- : string = "Moo"

Here is its signature:

  concat_list : string -> string list -> string

Replace the lines below with your own definition of "concat_list"
......................................................................*)

(* Concatenates a list of strings into one string with a concat in between *)
let rec concat_list (concat : string) (lst : string list) =
	match lst with
	| [] -> ""
	| [x] -> x
	| hd :: tl -> hd ^ concat ^ (concat_list concat tl);;
  

(*......................................................................
Problem 2g: One way to compress a list of characters is to use
run-length encoding. The basic idea is that whenever we have repeated
characters in a list such as

  ['a'; 'a'; 'a'; 'a'; 'a'; 'b'; 'b'; 'b'; 'a'; 'd'; 'd'; 'd'; 'd'] 

we can (sometimes) represent the same information more compactly as a
list of pairs like 

  [(5, 'a'); (3, 'b'); (1, 'a'); (4, 'd')]      . 

Here, the numbers represent how many times the character is
repeated. For example, the first character in the string is 'a' and it
is repeated 5 times, followed by 3 occurrences of the character 'b',
followed by one more 'a', and finally 4 copies of 'd'.

Write a function "to_run_length" that converts a list of characters
into the run-length encoding, and then write a function
"from_run_length" that converts back. Writing both functions will make
it easier to test that you've gotten them right.

Here are their signatures:

  to_run_length : char list -> (int * char) list
  from_run_length : (int * char) list -> char list

Replace the lines below with your own definitions of "to_run_length"
and "from_run_length".
......................................................................*)

(* Takes a list of characters and turns them into a run length list*)
let rec to_run_length (lst : char list) = 
	(* Encodes a single run of letters, such as all a's in a row, and then 
	returns the length of the run, the character in the run, and the remaining
	list after the elements in tne run were removed *)
	let rec encode_run (lst : char list) (ch : char) (len : int) = 
		match lst with
		| [] -> (len, ch, [])
		| hd :: tl as lst-> if hd = ch then (encode_run tl ch (len + 1))
		else (len, ch, lst) in

	match lst with 
	| [] -> []
	| hd :: tl -> match (encode_run tl hd 1) with
		| len, ch, ls -> (len, ch) :: to_run_length ls;;


(* Takes a run length list and turns it into a decompressed list*)
let rec from_run_length (lst : (int * char) list) =
	(* Turns a single tuple denoting a run into a list containing the characher
	the amount of times specified in the run *)
 	let rec decode_run (len : int) (ch : char) =
  		if len > 0 then ch :: (decode_run (len - 1) ch)
  		else [] in

 	match lst with
  	| [] -> []
  	| (len, ch) :: tl -> (decode_run len ch) @ (from_run_length tl);;

(*======================================================================
Problem 3: Challenge problem: Permutations

........................................................................
The function "permutations" takes a list of integers and should
return a list containing every permutation of the list. For example:

  permutations [1; 2; 3] =
  - : int list list = [[1; 2; 3]; [2; 1; 3]; [2; 3; 1]; [1; 3; 2]; 
  [3; 1; 2]; [3; 2; 1]]

It doesn't matter what order the permutations appear in the returned
list.  Note that if the input list is of length n, then the answer
should be of length n! (that is, the factorial of n).

Hint: One way to do this is to write an auxiliary function, interleave
: int -> int list -> int list list, that yields all interleavings of
its first argument into its second. For example:

  interleave 1 [2; 3] = 
  - : int list list = [ [1; 2; 3]; [2; 1; 3]; [2; 3; 1] ]

You may also use list module functions for this question and may find 
List.map and List.concat helpful. 

Here is the signature of permutations:

  permutations : int list -> int list list

Replace the line below with your own definition of "permutations".
......................................................................*)
(* We know that any call of insert will be within range of the array
due to the way we call it in interleave*)

let rec permutations (lst : int list) =
	(* Inserts an element into the idx'th index of lst. Since I call it
	internally and know the index will always be in the list, I do not need to
	check for the case where it isn't in my code*)
	let rec insert (ele : int) (idx : int) (lst : int list) = 
		match lst with
		| [] -> [ele]
		| hd :: tl -> if idx <= 0 then ele :: lst
			else hd :: insert ele (idx - 1) tl in

	(* Returns the length of the list *)
	let rec length (lst : int list) = 
		match lst with
		| [] -> 0
		| _ :: tl -> 1 + length (tl) in

	(* Takes a number and puts it into every single position in the list. When I
	call this function it starts at the length of the list, and based on the 
	recursion will get called every time until an invalid index (-1) is used *)
	let rec interleave (idx : int) (num : int) (lst : int list) = 
		if idx >= 0 then (insert num idx lst) :: (interleave (idx - 1) num lst)
		else [] in

	(* Recursively runs interleave on every element in a list of lists and 
	combines the result into a new list of lists *)
	let rec interweave (num : int) (lst : int list list) = 
		match lst with
		| hd :: tl -> (interleave (length hd) num hd) @ (interweave num tl)
		| _ -> [] in

	match lst with
	| hd :: tl -> interweave hd (permutations tl)
	| x -> [x];;

(*======================================================================
Time estimate

Please give us an honest (if approximate) estimate of how long (in
minutes) this part of the problem set took you to complete.  We care
about your responses and will use them to help guide us in creating
future assignments.
......................................................................*)

let minutes_spent_on_pset () : int = 360 ;;
