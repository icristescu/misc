(*Notes from Type-Safe Modular Hash-Consing, by Jean-Christophe Filliatre and Sylvain Conchon. Depends on library https://github.com/backtracking/ocaml-hashcons *)

type term = Var of int | Lam of term | App of term * term

let hashcons : term -> term = fun x -> x

let var n = hashcons (Var n)

let lam u = hashcons (Lam u)

let app (u, v) = hashcons (App (u, v))

module Term = struct
  type t = term

  let equal x y =
    match (x, y) with
    | Var n, Var m -> n == m
    | Lam u, Lam v -> u == v
    | App (u1, u2), App (v1, v2) -> u1 == v1 && u2 == v2
    | _ -> false

  let hash = Hashtbl.hash
end

module H = Hashtbl.Make (Term)

let table = H.create 251

let hashcons x =
  try H.find table x
  with Not_found ->
    H.add table x x;
    x

open Hashcons

type term = term_node hash_consed

and term_node = Var of int | Lam of term | App of term * term

module Term_node = struct
  type t = term_node

  let equal t1 t2 =
    match (t1, t2) with
    | Var i, Var j -> i == j
    | Lam u, Lam v -> u == v
    | App (u1, v1), App (u2, v2) -> u1 == u2 && v1 == v2
    | _ -> false

  let hash = function
    | Var i -> i
    | Lam t -> abs ((19 * t.hkey) + 1)
    | App (u, v) -> abs ((19 * ((19 * u.hkey) + v.hkey)) + 2)
end

module Hterm = Hashcons.Make (Term_node)

let ht = Hterm.create 251

let var n = Hterm.hashcons ht (Var n)

let lam u = Hterm.hashcons ht (Lam u)

let app (u, v) = Hterm.hashcons ht (App (u, v))

type pclause = C of t * t | U of t * t | L of string * bool

and view = { pos : pclause; neg : pclause }

and t = view Hashcons.hash_consed

module View = struct
  type t = view

  let eqc c1 c2 =
    match (c1, c2) with
    | U (f1, f2), U (g1, g2) | C (f1, f2), C (g1, g2) ->
        (f1 == g1 && f2 == g2) || (f1 == g2 && f2 == g1)
    | L (x1, b1), L (x2, b2) -> x1 = x2 && b1 = b2
    | _ -> false

  let equal f1 f2 = eqc f1.pos f2.pos && eqc f1.neg f2.neg

  let hashc acc = function
    | U (f1, f2) | C (f1, f2) ->
        let min = min f1.tag f2.tag in
        let max = max f1.tag f2.tag in
        (((acc * 19) + min) * 19) + max
    | L _ as z -> Hashtbl.hash z

  let hash f = abs (hashc (hashc 1 f.pos) f.neg)
end

module H = Hashcons.Make (View)
