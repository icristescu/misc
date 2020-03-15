(* Notes from Monads for functional programming, Wadler and https://xavierleroy.org/mpri/2-4/monads.2up.pdf *)

type term = Con of int | Div of term * term

let rec eval : term -> int = function
  | Con a -> a
  | Div (t, u) -> eval t / eval u

(*raise exceptions*)
type ('a, 'except) m_exc = Raise of 'except | Return of 'a

(*add state*)
type ('a, 'state) m_state = 'state -> 'a * 'state

type 'a int_state = ('a, int) m_state

let rec eval_state : term -> int int_state =
 fun term x ->
  match term with
  | Con a -> (a, x)
  | Div (t, u) ->
      let a, y = eval_state t x in
      let b, z = eval_state u y in
      (a / b, z + 1)

(*outputs*)
type ('a, 'outs) m_out = 'a * 'outs

type 'a string_out = ('a, string) m_out

module type MONAD = sig
  type 'a t

  val return : 'a -> 'a t

  val bind : 'a t -> ('a -> 'b t) -> 'b t

  (* infix for bind*)
  val ( >>= ) : 'a t -> ('a -> 'b t) -> 'b t
end

module Monad_with_eval (M : MONAD) = struct
  include M

  let rec eval : term -> int t = function
    | Con a -> return a
    | Div (t, u) ->
        eval t >>= fun a ->
        eval u >>= fun b -> return (a / b)
end

(* but I can't write this function outside of a module, i.e. by giving a module as argument to a function *)
(*let rec eval_monad (module M : MONAD) term =
  match term with
  | Con a -> M.return a
  | Div (t, u) ->
      let f a = M.bind (eval_monad u) (fun b -> return (a / b)) in
      M.bind (eval_monad t) (fun a -> f a)
*)

module Identity : MONAD = struct
  type 'a t = 'a

  let return a = a

  let bind a k = k a

  let ( >>= ) = bind
end

module Exc : sig
  include MONAD

  val exc_raise : string -> 'a t
end = struct
  type 'a t = Raise of string | Return of 'a

  let return a = Return a

  let bind m k = match m with Raise s -> Raise s | Return a -> k a

  let ( >>= ) = bind

  let exc_raise s = Raise s
end

module Exc_monad_with_eval = struct
  include Exc

  let rec eval : term -> int t = function
    | Con a -> return a
    | Div (t, u) ->
        eval t >>= fun a ->
        eval u >>= fun b ->
        if b = 0 then exc_raise "divide by zero" else return (a / b)
end

module State : sig
  include MONAD

  val tick : unit t
end = struct
  type state = int

  type 'a t = state -> 'a * state

  let return a x = (a, x)

  let bind m k x =
    let a, y = m x in
    let b, z = k a y in
    (b, z)

  let ( >>= ) = bind

  let tick x = ((), x + 1)

  (* In an impure language, an operation like tick would be represented by a
     function of type () -> (). The spurious argument () is required to delay the
     effect until the function is applied, and since the output type is () one may
     guess that the function’s purpose lies in a side effect. In contrast, here
     tick has type M (): no spurious argument is needed, and the appearance of M
     explicitly indicates what sort of effect may occur. *)
end

module State_monad_with_eval = struct
  include State

  let rec eval : term -> int t = function
    | Con a -> return a
    | Div (t, u) ->
        eval t >>= fun a ->
        eval u >>= fun b ->
        tick >>= fun () -> return (a / b)
end

(* monad laws *)

module Laws (M : MONAD) = struct
  open M

  let left a n =
    let eq1 = return a >>= fun b -> n b in
    let eq2 = n a in
    assert (eq1 = eq2)

  let right m =
    let eq1 = m >>= fun a -> return a in
    assert (eq1 = m)

  let assoc m n o =
    let eq1 =
      m >>= fun a ->
      n a >>= fun b -> o b
    in
    let eq2 = (m >>= fun a -> n a) >>= fun b -> o b in
    assert (eq1 = eq2)
end

module Ext_Monad (M : MONAD) : sig
  include MONAD

  val map : ('a -> 'b) -> 'a t -> 'b t

  val join : 'a t t -> 'a t
end = struct
  include M

  let map f m = m >>= fun a -> return (f a)

  let join z = z >>= fun m -> m
end

let id x = x

module Laws_ext (M : module type of Ext_Monad) (N : MONAD) = struct
  module M = M (N)
  include M

  let law1 x =
    let eq1 = map id x in
    let eq2 = id x in
    assert (eq1 = eq2)

  let law2 f g x =
    let eq1 = map (fun x -> f (g x)) x in
    let eq2 = (map f) (map g x) in
    assert (eq1 = eq2)
end

(* Arrays *)

module type ARR = sig
  type value

  type arr

  type ix

  val newarray : value -> arr

  val index : ix -> arr -> value

  val update : ix -> value -> arr -> arr
end

module Prog (Id : sig
  type t
end)
(A : ARR with type ix = Id.t and type value = int) =
struct
  type term = Var of Id.t | Con of int | Add of term * term

  type state = A.arr

  let rec eval : term -> state -> int =
   fun term x ->
    match term with
    | Var i -> A.index i x
    | Con a -> a
    | Add (t, u) -> eval t x + eval u x
end

module Arr_Monad (A : ARR) : sig
  type state = A.arr

  include MONAD with type 'a t = state -> 'a * state

  val block : A.value -> 'a t -> 'a

  val fetch : A.ix -> A.value t

  val assign : A.ix -> A.value -> unit t
end = struct
  type state = A.arr

  type 'a t = state -> 'a * state

  let return a x = (a, x)

  let bind m k x =
    let a, y = m x in
    let b, z = k a y in
    (b, z)

  let ( >>= ) = bind

  let block v m =
    let a, _x = m (A.newarray v) in
    a

  let fetch i x = (A.index i x, x)

  let assign i v x = ((), A.update i v x)
end

module Prog1 (Id : sig
  type t
end)
(A : ARR with type ix = Id.t and type value = int) =
struct
  module A = Arr_Monad (A)
  open A

  type term = Var of Id.t | Con of int | Add of term * term

  let rec eval : term -> int t = function
    | Var i -> fetch i
    | Con a -> return a
    | Add (t, u) ->
        eval t >>= fun a ->
        eval u >>= fun b -> return (a + b)
end

module Arr_Readers_Monad (A : ARR) : sig
  type state = A.arr

  include MONAD with type 'a t = state -> 'a

  val fetch : A.ix -> A.value t
end = struct
  type state = A.arr

  type 'a t = state -> 'a

  let return a _x = a

  let bind m k x =
    let a = m x in
    k a x

  let ( >>= ) = bind

  let fetch i x = A.index i x
end

module Morphism (A : ARR) = struct
  module AW = Arr_Monad (A)
  module AR = Arr_Readers_Monad (A)

  let coerce : 'a AR.t -> 'a AW.t =
   fun m x ->
    let a = m x in
    (a, x)

  let law1 a =
    let eq1 = coerce (AR.return a) in
    let eq2 = AW.return a in
    assert (eq1 = eq2)

  let law2 m n =
    let eq1 = coerce (AR.bind m (fun a -> n a)) in
    let eq2 = AW.bind (coerce m) (fun a -> coerce (n a)) in
    assert (eq1 = eq2)
end

module Nondet = struct
  type 'a t = 'a list

  let return a = [ a ]

  let rec bind m f = match m with [] -> [] | hd :: tl -> f hd @ bind tl f

  let ( >>= ) = bind

  let run m = match m with hd :: _tl -> hd | [] -> failwith "emptylist"

  let runall m = m

  let fail = []

  let either a b = a @ b
end

let rec insert x l =
  Nondet.(
    either
      (return (x :: l))
      ( match l with
      | [] -> fail
      | hd :: tl -> insert x tl >>= fun l' -> return (hd :: l') ))

let rec permut l =
  match l with
  | [] -> Nondet.return []
  | hd :: tl -> Nondet.(permut tl >>= fun l' -> insert hd l')

(*If monads encapsulate effects and lists form a monad, do lists correspond to
 *some effect? Indeed they do, and the effect they correspond to is choice. One
 *can think of a computation of type [a] as offering a choice of values, one for
 *each element of the list. *)

module ListMonad = struct
  type 'a t = 'a list

  let return a = [ a ]

  let rec bind m f = match m with [] -> [] | hd :: tl -> f hd @ bind tl f

  let ( >>= ) = bind
end

module ListComprehension = struct
  include ListMonad

  let unary t u = u >>= fun x -> return (t x)

  let binary t u v =
    u >>= fun x ->
    v >>= fun y -> return (t x y)
end

module Parser = struct
  type state = string

  type 'a p = state -> ('a * state) list

  let return a x = [ (a, x) ]

  let rec unary m f = match m with [] -> [] | hd :: tl -> f hd @ unary tl f

  let bind (m : 'a p) (k : 'a -> 'b p) (x : state) : 'b p =
    let u = m x in
    let v (a, y) = k a y in
    unary u v
end

(* Notes from https://xavierleroy.org/mpri/2-4/monads.2up.pdf *)

(* the continuation monad *)

module Cont (Answer : sig
  type t
end) =
struct
  type 'a t = ('a -> Answer.t) -> Answer.t

  let return a k = k a

  let bind m f k = m (fun v -> f v k)

  let ( >>= ) = bind

  let callcc f k = f k k

  let throw x y _k = x y
end

(* the concurrency monad *)
module Log : sig
  include MONAD

  type log = string list

  val run : 'a t -> 'a * log

  val log : string -> unit t
end = struct
  type log = string list

  type 'a t = log -> 'a * log

  let return a l = (a, l)

  let bind m f l =
    let a, k = m l in
    f a k

  let ( >>= ) = bind

  let run m =
    let a, k = m [] in
    (a, List.rev k)

  let log msg l = ((), msg :: l)
end

module Concur (M : sig
  include MONAD

  type log

  val run : 'a t -> 'a * log
end) =
struct
  type 'a t = Done of 'a | Step of 'a t M.t

  let rec perform (x : 'a t) : 'a M.t =
    match x with Done res -> M.return res | Step m -> M.bind m perform

  let run (x : 'a t) = M.run (perform x)

  let return a = Done a

  let act (m : 'a M.t) : 'a t = Step (M.bind m (fun a -> M.return (Done a)))

  let rec bind (m : 'a t) (f : 'a -> 'b t) =
    match m with
    | Done a -> f a
    | Step s -> Step (M.bind s (fun a -> M.return (bind a f)))

  let ( >>= ) = bind

  let rec par (m1 : 'a t) (m2 : 'b t) : ('a * 'b) t =
    match (m1, m2) with
    | Done a, Done b -> Done (a, b)
    | Done a, Step b -> Step (M.bind b (fun x -> M.return (par (Done a) x)))
    | Step a, Done b -> Step (M.bind a (fun x -> M.return (par x (Done b))))
    | Step a, Step b ->
        Step (M.bind a (fun xa -> M.bind b (fun xb -> M.return (par xa xb))))
end

module M = Concur (Log)

let rec loop n s =
  if n <= 0 then M.return ()
  else M.(act (Log.log s) >>= fun _ -> loop (n - 1) s)

let print_logs () =
  let _, logs =
    M.(run (act (Log.log "start:") >>= fun _ -> par (loop 6 "a") (loop 4 "b")))
  in
  List.iter (fun s -> print_string (s ^ ", ")) logs

module type APPLICATIVE = sig
  type 'a t

  val pure : 'a -> 'a t

  val ( <*> ) : ('a -> 'b) t -> 'a t -> 'b t

  (* More generally: return f <*> m1 <*> · · · <*> mn denotes the application of
     the pure function f to the results of the monadic computations m1, . . . ,
     mn.*)
end

module Applicative_from_monad (M : MONAD) : APPLICATIVE = struct
  type 'a t = 'a M.t

  let pure a = M.return a

  let ( <*> ) f x = M.bind f (fun vf -> M.bind x (fun vx -> M.return (vf vx)))
end

module type COMONAD = sig
  type 'a t

  val proj : 'a t -> 'a

  val cobind : 'a t -> ('a t -> 'b) -> 'b t
end

module Lazy : COMONAD = struct
  type 'a t = 'a status ref

  and 'a status = Evaluated of 'a | Suspended of (unit -> 'a)

  let proj x =
    match !x with
    | Evaluated a -> a
    | Suspended f ->
        let v = f () in
        x := Evaluated v;
        v

  let cobind x f = ref (Suspended (fun () -> f x))
end
