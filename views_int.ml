(*Notes from Views: A way for pattern matching to cohabit with data abstraction, by Wadler.*)

type peano = Zero | Succ of peano

let rec power x = function Zero -> 1 | Succ n -> x * power x n

let rec sum a b =
  match (a, b) with Zero, b -> b | a, Zero -> a | Succ a, b -> Succ (sum a b)

let rec fib : peano -> peano = function
  | Zero -> Zero
  | Succ Zero -> Succ Zero
  | Succ (Succ n) -> sum (fib n) (fib (Succ n))

(*section 2*)
module ViewInt : sig
  type t = int

  type peano = Zero | Succ of t

  val viewin : t -> peano

  val viewout : peano -> t

  val fun_t : t -> t -> t
end = struct
  type t = int

  type peano = Zero | Succ of t

  let rec viewin n = if n = 0 then Zero else Succ (n - 1)

  let rec viewout = function Zero -> 0 | Succ n -> n + 1

  let fun_t a b = a + b
end

open ViewInt

let rec fib' : peano -> t = function
  | Zero -> viewout Zero
  | Succ n -> (
      match viewin n with
      | Zero -> viewout (Succ (viewout Zero))
      | Succ m -> fun_t m (viewout (Succ m)) )

(*section 3*)
module ViewIntAlt = struct
  type t = int

  type odd = Zero | Odd of t | Even of t

  let rec viewin n =
    if n = 0 then Zero
    else if n mod 2 = 0 then Even (n / 2)
    else Odd ((n - 1) / 2)

  let rec viewout = function
    | Zero -> 0
    | Odd n -> n * 2
    | Even n -> (n * 2) + 1
end

open ViewIntAlt

let rec power x = function
  | Zero -> 1
  | Even n -> power (x * x) (viewin n)
  | Odd n -> x * power (x * x) (viewin n)

(*section 4*)
module type View = sig
  type 'a concrete

  type 'a abstract

  val viewin : 'a concrete -> 'a abstract

  val viewout : 'a abstract -> 'a concrete
end

module ViewInt2 : sig
  include View

  val fun_t : 'a concrete -> 'a concrete -> 'a concrete
end = struct
  type 'a concrete = int

  type 'a abstract = Zero | Succ of t

  let rec viewin n = if n = 0 then Zero else Succ (n - 1)

  let rec viewout = function Zero -> 0 | Succ n -> n + 1

  let fun_t a b = a + b
end

(*section 5*)
module Lists : sig
  include View
end = struct
  type 'a concrete = Nilc | Cons of 'a * 'a abstract

  and 'a abstract = Nila | Snoc of 'a concrete * 'a

  let rec viewin : 'a concrete -> 'a abstract = function
    | Nilc -> Nila
    | Cons (head, Nila) -> Snoc (Nilc, head)
    | Cons (head, Snoc (rest, last)) -> Snoc (Cons (head, viewin rest), last)

  let rec viewout : 'a abstract -> 'a concrete = function
    | Nila -> Nilc
    | Snoc (Nilc, last) -> Cons (last, Nila)
    | Snoc (Cons (head, rest), last) -> Cons (head, Snoc (viewout rest, last))

  let head = function Nilc -> failwith "empty" | Cons (x, _) -> x

  let last = function Nila -> failwith "empty" | Snoc (_, x) -> x

  (*as for some cases in view, these definitions are symmetric *)
  let rotleft = function
    | Nilc -> Nila
    | Cons (head, rest) -> Snoc (viewout rest, head)

  let rotright = function
    | Nila -> Nilc
    | Snoc (rest, last) -> Cons (last, viewin rest)
end

(*section 7*)
module ZipList = struct
  type 'a concrete = Nil | Cons of 'a * 'a abstract

  and 'b abstract = Zip of 'b concrete * 'b concrete

  (* let rec viewin : type a. ('a, b) concrete -> ('a, b) abstract = function
     | Nil -> Zip (Nil, Nil)
     | Cons ((a, b), Zip (als, bls)) -> Zip (Cons (a, als), Cons (b, bls))
  *)
  (* let rec viewout : type a b. a abstract -> b concrete = function
     | Zip (Cons (a, als), Cons (b, bls)) -> Cons ((a, b), Zip (als, bls))
     | Zip (a, Nil) | Zip (Nil, a) -> a
     | Zip (Nil, Nil) -> Nil*)
end

(*section 8*)
module ViewInt3 = struct
  type t = int

  type odd = OddP of t | EvenP of t

  let rec viewin n = if n mod 2 = 0 then EvenP n else OddP n

  let rec viewout = function OddP n -> n | EvenP n -> n
end

open ViewInt3

let f1 n =
  match viewin n with
  | EvenP _ -> print_endline "even"
  | OddP _ -> print_endline "odd"

module ViewInt4 : sig
  type concrete = int

  type abstract = peano

  type a_s = concrete * abstract

  val viewin : concrete -> a_s

  val viewout : a_s -> concrete
end = struct
  type concrete = int

  type abstract = peano

  type a_s = concrete * abstract

  let rec viewin n = (n, ViewInt.viewin n)

  let rec viewout (n, _) = n
end
