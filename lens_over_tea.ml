type ('a, 's) lens1 = Lens of { getter : 's -> 'a; setter : 'a -> 's -> 's }

let rec setIth : int -> 'a -> 'a list -> 'a list =
 fun index a ls ->
  match ls with
  | old :: rest ->
      if index = 0 then a :: rest else old :: setIth (index - 1) a rest
  | [] -> failwith "error setIth"

let _ix : int -> ('a, 'a list) lens1 =
 fun i -> Lens { getter = (fun x -> List.nth x i); setter = setIth i }

type ('a, 's) lens = ('a -> 'a) -> 's -> 'a * 's

let rec ix : int -> ('a, 'a list) lens =
 fun i f ls ->
  match ls with
  | old :: rest ->
      if i = 0 then (old, f old :: rest)
      else
        let old', rest' = ix (i - 1) f rest in
        (old', old :: rest')
  | [] -> failwith "error ix"

let id x = x

let test1 =
  let x = fst (ix 3 id [ 7; 4; 1; 8 ]) in
  print_int x

module type Functor = sig
  type 'a f

  val fmap : ('a -> 'b) -> 'a f -> 'b f
end

module LensF (F : Functor) = struct
  include F

  type ('a, 's) lens = ('a -> 'a f) -> 's -> 'a * 's f

  let rec ix : int -> ('a, 'a list) lens =
   fun i f ls ->
    match ls with
    | old :: rest ->
        if i = 0 then (old, fmap (fun x -> x :: rest) (f old))
        else
          let old', rest' = ix (i - 1) f rest in
          let rest'' = fmap (fun x -> old :: x) rest' in
          (old', rest'')
    | [] -> failwith "error ix"
end

type ('a, 'store) state = { state : 'store -> 'a * 'store }

let run u init =
  let f = u.state in
  f init

let inc b : ('b, int) state = { state = (fun s -> (b, s + 1)) }

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

module LensStorey (F : Functor) = struct
  include F

  type ('a, 's) lens = ('a -> 'a f) -> 's -> 's f

  let rec ix : int -> ('a, 'a list) lens =
   fun i f ls ->
    match ls with
    | old :: rest ->
        if i = 0 then fmap (fun x -> x :: rest) (f old)
        else
          let rest' = ix (i - 1) f rest in
          fmap (fun x -> old :: x) rest'
    | [] -> failwith "error ix"
end

module SF = State_Functor (struct
  type store = int
end)

module LS = LensStorey (SF)

let print_store (s : int list SF.f) =
  SF.fmap
    (fun ls ->
      List.iter
        (fun x ->
          print_int x;
          print_string ", ")
        ls)
    s

let test =
  let f x = { state = (fun ls -> (ls, x)) } in
  let x = LS.ix 3 f [ 7; 4; 1; 8 ] in
  print_store x

(* how do i get the saved value?*)

module LensExercise (F : Functor) = struct
  include F

  type ('s, 't, 'a, 'b) lens = ('a -> 'b f) -> 's -> 't f

  type ('s, 'a) lens' = ('s, 's, 'a, 's) lens

  (* _1 :: Functor f => (a -> f b) -> (a, x) -> f (b, x) *)
  let pi : ('a * 'x, 'b * 'x, 'a, 'b) lens =
   fun f (a, x) -> fmap (fun b -> (b, x)) (f a)

  (* _2 :: Functor f => (a -> f b) -> (x, a) -> f (x, b) *)
  let pi_2 : ('x * 'a, 'x * 'b, 'a, 'b) lens =
   fun f (a, x) -> fmap (fun b -> (x, b)) (f a)

  (* Make a lens out of a getter and a setter. *)
  let mklens : ('s -> 'a) -> ('s -> 'b -> 't) -> ('s, 't, 'a, 'b) lens =
   fun get set f s ->
    let a = get s in
    fmap (fun b -> set s b) (f a)
end
