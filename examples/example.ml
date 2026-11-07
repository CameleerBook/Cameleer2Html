(* Implementation and specification by Pedro Gasparinho, advised by 
   Mário Pereira *)

module type LL = sig

  type elt
  type llist

  (*@ function len (l: llist) : int *)

  val length : llist -> int
  (*@ r = length l
      ensures r = len l *)

  val is_empty : llist -> bool [@@logic]
  (*@ r = is_empty l 
      ensures r <-> len l = 0 *)

  val get : llist -> int -> elt [@@logic]
  (*@ r = get l n
      requires 0 <= n < len l *)

  (*@ function mem (x: elt) (l: llist) : bool *)

  val mem' : elt -> llist -> bool
  (*@ r = mem x l
      ensures r <-> mem x l *)

  (*@ predicate reversed (l1 l2: llist) *)

	(*@ axiom reversed_length :
			forall l1 l2: llist. reversed l1 l2 -> len l1 = len l2 *)

	(*@ axiom reversed_elems :
	    forall l1 l2: llist. reversed l1 l2 ->
				forall i. 0 <= i < len l1 -> 
					get l1 i = get l2 (len l1 - 1 - i) *)

  val rev : llist -> llist
  (*@ r = rev l
      ensures reversed r l *)

  (*@ predicate appended (r l1 l2: llist) *)

	(*@ axiom appended_length : 
			forall r l1 l2: llist. appended r l1 l2 ->
				len r = len l1 + len l2 *)

	(*@ axiom appended_order : 
			forall r l1 l2: llist. appended r l1 l2 ->
        forall i. 0 <= i < len l1 -> get r i = get l1 i &&
        forall i. len l1 <= i < len l1 + len l2 ->
          get r i = get l2 (i - len l1) *)

  val append : llist -> llist -> llist
  (*@ r = append l1 l2
      ensures appended r l1 l2 *)

	val map : (elt -> elt) -> llist -> llist
	(*@ r = map f l
			ensures len r = len l
			ensures forall i. 0 <= i < len l -> (f (get l i)) = (get r i) *)

  (*@ predicate filtered (f: elt -> bool) (r l: llist) *)

	(*@ axiom filtered_length : 
			forall f: (elt -> bool), r l: llist. filtered f r l ->
				0 <= len r <= len l *)

	(*@ axiom filtered_valid :
			forall f: (elt -> bool), r l: llist. filtered f r l ->
				forall x: elt. mem x r -> f x *)

  (*@ axiom filtered_relative_pos : 
      forall f: (elt -> bool), r l: llist. filtered f r l ->
        forall i j. 0 <= i <= j < len r -> 
				exists i' j'. i <= i' <= j' < len l ->
				  (get r i) = (get l i') && (get r j) = (get l j') *)

	val filter : (elt -> bool) -> llist -> llist
	(*@ r = filter f l
			ensures filtered f r l *)

  val init : int -> (int -> elt) -> llist
  (*@ r = init s f
      requires s >= 0
      ensures len r = s
      ensures forall k. 0 <= k < s -> (get r k) = (f k) *)

end

module type ComparableType = sig
	type t

	val eq: t -> t -> bool [@@logic]
	(*@ b = eq x y
		ensures b <-> x = y *)
end

module LinkedList (E: ComparableType) : LL = struct

  type elt = E.t
  type llist = Nil | Cons of elt * llist

  (*@ function rec len (l: llist) : int =
      match l with
      | Nil -> 0
      | Cons _ t -> 1 + len t *)
  (*@ variant l 
      ensures l = Nil <-> result = 0
      ensures l <> Nil <-> result > 0 *)

  let length l = 
    let rec aux c = function
      | Nil -> c
      | Cons (_, t) -> aux (c+1) t
    (*@ r = aux c s
        variant s
        ensures c + len s = r *)
    in aux 0 l
  (*@ r = len l
      ensures r = len l *)

  let[@logic] is_empty = function
    | Nil -> true
    | _ -> false
  (*@ r = is_empty l 
      ensures r <-> l = Nil *)

  let[@logic] rec get l n = 
    match l with
    | Nil -> assert false
    | Cons (h, t) -> 
      if n = 0 then h 
      else get t (n-1)
  (*@ r = get l n
      variant n
      requires 0 <= n < len l *)

  (*@ function rec mem (x: elt) (l: llist) : bool =
      match l with
      | Nil -> false
      | Cons h t -> E.eq h x || mem x t *)
  (*@ variant l *)

  let[@logic] mem' x l =
    let rec aux x = function
      | Nil -> false
      | Cons (h, t) -> 
        if E.eq h x then true
        else aux x t
    (*@ r = aux x l'
        variant l'
        ensures r <-> mem x l' *)
    in aux x l
  (*@ r = mem' x l
      ensures r <-> mem x l *)

  (*@ predicate reversed_sub (r s: llist) = 
      len s <= len r &&
      forall i. 0 <= i < len s -> 
        (get r i) = (get s (len s - 1 - i)) *)

  (*@ predicate valid_accumulator (r acc s: llist) =
      len r = len acc + len s &&
      forall i. len s <= i < len r -> 
        (get r i) = (get acc (i - len s)) *)
    
  (*@ predicate reversed (l1 l2: llist) = 
      len l1 = len l2 && 
      forall i. 0 <= i < len l1 -> 
        get l1 i = get l2 (len l1 - 1 - i) *)

  let[@logic] rev l =
    let rec aux acc = function
      | Nil -> acc
      | Cons (h, t) -> aux (Cons (h, acc)) t
    (*@ r = aux acc s
        ensures len r = len acc + len s 
        ensures reversed_sub r s
        ensures valid_accumulator r acc s
        variant s *)
    in aux Nil l
  (*@ r = rev l
      ensures reversed r l *)

  (*@ predicate appended_sub (r s1: llist) =
      len s1 <= len r &&
      forall i. 0 <= i < len s1 -> 
        get r i = get s1 (len s1 - 1 - i) *)

  (*@ predicate appended_right (r l1 l2: llist) = 
      forall i. len l1 <= i < len l1 + len l2 ->
        get r i = get l2 (i - len l1) *)

  (*@ predicate appended (r l1 l2: llist) =
      len r = len l1 + len l2 &&
      forall i. 0 <= i < len l1 -> get r i = get l1 i &&
      appended_right r l1 l2 *)

  let[@logic] append l1 l2 =
    let rec aux s1 s2 =
      match s1 with
      | Nil -> s2
      | Cons (h, t) -> aux t (Cons (h, s2)) 
    (*@ r = aux s1 s2
        variant s1
        ensures len r = len s1 + len s2
        ensures appended_sub r s1
        ensures appended_right r s1 s2 *)
    in let r1 = rev l1 in aux r1 l2
  (*@ r = append l1 l2
      ensures appended r l1 l2 *)

  (*@ predicate mapped_sub (f: elt -> elt) (r s: llist) = 
      len s <= len r &&
      forall i. 0 <= i < len s -> 
        (get r i) = (f (get s (len s - 1 - i))) *)

	let map f l =
	let rec aux acc f = function
		| Nil -> acc
			| Cons (h, t) -> aux (Cons (f h, acc)) f t
		(*@ r = aux acc f s
				variant s
				ensures len r = len acc + len s
        ensures mapped_sub f r s
        ensures valid_accumulator r acc s *)
		in let l' = rev l in aux Nil f l'	
	(*@ r = map f l
			ensures len r = len l
			ensures forall i. 0 <= i < len l -> (f (get l i)) = (get r i) *)

  (*@ predicate filtered_relative_order (r l: llist) = 
      forall i j. 0 <= i <= j < len r -> 
				exists i' j'. i <= i' <= j' < len l ->
				(get r i) = (get l i') && (get r j) = (get l j') *)

  (*@ predicate filtered_sub (f: elt -> bool) (r acc s: llist) =
      0 <= len r <= len acc + len s &&
      forall x: elt. mem x r -> f x &&
      filtered_relative_order r s *)

  (*@ predicate filtered (f: elt -> bool) (r l: llist) = 
      0 <= len r <= len l &&
      forall x: elt. mem x r -> f x &&
      filtered_relative_order r l *)

	let filter f l =
		let rec aux acc f = function
			| Nil -> acc
			| Cons (h, t) -> 
				if f h then aux (Cons (h, acc)) f t
				else aux acc f t 
		(*@ r = aux acc f s 
				variant s
				requires forall x: elt. mem x acc -> f x
				ensures filtered_sub f r acc s *)	
		in let l' = rev l in aux Nil f l'
	(*@ r = filter f l
			ensures filtered f r l *)

  let init s f =
    let rec aux acc n f =
      if n = 0 then acc
      else aux (Cons (f (n-1), acc)) (n-1) f
    (*@ r = aux acc n f
        variant n
        requires 0 <= n <= s
        requires s = len acc + n 
        requires forall k. 0 <= k < len acc -> (get acc k) = (f (k+n))
        ensures len r = len acc + n
        ensures forall k. 0 <= k < len r -> (get r k) = (f k) *)
    in aux Nil s f
  (*@ r = init s f
      requires s >= 0
      ensures len r = s
      ensures forall k. 0 <= k < s -> (get r k) = (f k) *)

end
