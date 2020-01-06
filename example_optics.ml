(** Working through the paper Profunctor Optics: Modular Data Accessors and the blog posts https://artyom.me/lens-over-tea-1 *)

type ('a, 'b, 's, 't) lens = 's -> 'a * ('b -> 't)

let lens f = f

let lens_get : ('a, 'b, 's, 't) lens -> 's -> 'a = fun lens x -> fst (lens x)

let lens_set : ('a, 'b, 's, 't) lens -> 's -> 'b -> 't =
 fun lens x -> snd (lens x)

(* examples with lenses *)
let view : 's -> 'a = fun x -> fst x

let update : 's -> 'b -> 't = fun x y -> (y, snd x)

let pi : ('a, 'b, 'a * 'c, 'b * 'c) lens = lens (fun x -> (view x, update x))

let pi_compact : ('a, 'b, 'a * 'c, 'b * 'c) lens =
  lens (fun x -> (fst x, fun s -> (s, snd x)))

let test1 =
  let x = (1, 2) in
  let y = lens_get pi_compact x in
  print_int y;
  let x' = lens_set pi_compact x 3 in
  print_int (lens_get pi_compact x')

let sign = lens (fun x -> (x >= 0, fun b -> if b then abs x else -abs x))

(* prisms *)
type ('a, 'b, 's, 't) prism = ('b -> 't) * ('s -> ('a, 't) result)

let prism f = f

(* examples with prisms *)
let match_prism : 'a option -> ('a, 'b option) result = function
  | Some x -> Ok x
  | None -> Error None

let build_prism (x : 'b) : 'b option = Some x

let maybe : ('a, 'b, 'a option, 'b option) prism =
  prism (build_prism, fun x -> match_prism x)

(* compose lenses : compose pi twice to wrap the inner element of a triple *)
let pi1 : ('a * 'c, 'b * 'c, ('a * 'c) * 'd, ('b * 'c) * 'd) lens =
  lens (fun x -> (fst x, fun s -> (x, snd s)))

let view x = lens_get pi (lens_get pi1 x)

let update x b =
  let xy = lens_get pi1 x in
  lens_set pi1 x (lens_set pi xy b)

let pi_triple : ('a, 'b, ('a * 'c) * 'd, ('b * 'c) * 'd) lens =
  lens (fun x -> (view x, update x))

(* adapters *)
type ('a, 'b, 's, 't) adapter = ('s -> 'a) * ('b -> 't)

let adapter f = f

let adapter_from : ('a, 'b, 's, 't) adapter -> 's -> 'a = fst

let adapter_to : ('a, 'b, 's, 't) adapter -> 'b -> 't = snd

let flatten :
    (('a * 'b) * 'c, ('e * 'f) * 'g, 'a * 'b * 'c, 'e * 'f * 'g) adapter =
  adapter ((fun (x, y, z) -> ((x, y), z)), fun ((x, y), z) -> (x, y, z))

let view x = lens_get pi_triple (adapter_from flatten x)

let update x b =
  let x' = adapter_from flatten x in
  let x'' = lens_set pi_triple x' b in
  adapter_to flatten x''

let pi_triple' : ('a, 'b, 'a * 'c * 'd, 'b * 'c * 'd) lens =
  lens (fun x -> (view x, update x))

(* traversals *)

module type Functor = sig
  type 'a f

  val fmap : ('a -> 'b) -> 'a f -> 'b f
end

module type Applicative = sig
  include Functor

  val pure : 'a -> 'a f

  val ( <*> ) : ('a -> 'b) f -> 'a f -> 'b f
end

type ('a, 'store) state = { state : 'store -> 'a * 'store }

let run u init =
  let f = u.state in
  f init

let inc b : (bool, int) state = { state = (fun s -> (b, s + 1)) }

module type S = sig
  type store
end

module State_Functor (S : S) : Functor with type 'a f = ('a, S.store) state =
struct
  type 'a f = ('a, S.store) state

  let fmap f m =
    {
      state =
        (fun s ->
          let x, s' = run m s in
          (f x, s'));
    }
end

module State_Applicative (S : S) : Applicative = struct
  include State_Functor (S)

  let pure x = { state = (fun s -> (x, s)) }

  let ( <*> ) m n =
    {
      state =
        (fun s ->
          let f, s' = run m s in
          let x, s'' = run n s' in
          (f x, s''));
    }
end

(* We say that container type S with elements of type A is traversable when there exist types B, T and a traversal function of type (A → F B) → (S → F T) for each applicative functor F *)

type 'a tree = Empty | Node of 'a tree * 'a * 'a tree

module Traverse (A : Applicative) = struct
  include A

  let rec inorder : ('a -> 'b f) -> 'a tree -> 'b tree f =
   fun m tree ->
    match tree with
    | Empty -> pure Empty
    | Node (l, v, r) ->
        let l' = inorder m l in
        let r' = inorder m r in
        let v' = m v in
        pure (fun x y z -> Node (x, y, z)) <*> l' <*> v' <*> r'
end

(* nested datatypes *)
type ('a, 'b, 't) funList =
  | Done of 't
  | More of 'a * ('a, 'b, 'b -> 't) funList

type ('a, 'b) either = Left of 'a | Right of 'b

let out : ('a, 'b, 't) funList -> ('t, 'a * ('a, 'b, 'b -> 't) funList) either =
  function
  | Done t -> Left t
  | More (x, l) -> Right (x, l)

let inn : ('t, 'a * ('a, 'b, 'b -> 't) funList) either -> ('a, 'b, 't) funList =
  function
  | Left t -> Done t
  | Right (x, l) -> More (x, l)

let id x = x

let single : 'a -> ('a, 'b, 'b) funList = fun x -> More (x, Done id)

(*
let rec fuse : 't. ('b, 'b, 't) funList -> 't = function
  | Done t -> t
  | More (x, l) -> fuse (l x)
stuck here : how to make types ('b, 'b, 't) funList and ('b, 'b, 'b -> 't) funList to match *)
