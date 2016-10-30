open Sharp_core
open Sharp_event

open Behaviour
open Network

type route = string list -> unit Network.t option

let to_chars str =
  let rec go acc i =
    if i < 0 then acc else go (str.[i] :: acc) (i - 1)
  in go [] (String.length str - 1)

let from_chars chars =
  let len = List.length chars in
  let str = Bytes.create len in
  let rec go i = function
    | [] -> ()
    | c :: cs -> Bytes.set str i c; go (i + 1) cs
  in go 0 chars; str

let rec split d xs =
  let rec split_pair acc = function
    | [] -> (List.rev acc, [])
    | x :: xs when x = d -> (List.rev acc, xs)
    | x :: xs -> split_pair (x :: acc) xs
  in match split_pair [] xs with
  | [], []   -> []
  | chunk, rest -> chunk :: split d rest

let to_parts str =
  let parts = split '/' (to_chars str) in
  let strs  = List.map (fun chars -> Url.urldecode (from_chars chars)) parts in
  List.filter (fun str -> not (str = "" || str = "#")) strs

let from_parts parts =
  let encparts = List.map Url.urlencode parts in
  let path     = List.fold_right (fun s acc -> s ^ "/" ^ acc) encparts "" in
  "/" ^ path

let push_state title path =
  Dom_html.window##.history##pushState () (Js.string title)
                                       (Js.some (Js.string path))

let rec search_routes routes parts = match routes with
  | [] -> None
  | route :: routes' ->
     match route parts with
     | Some net as res -> res
     | None -> search_routes routes' parts

let router ?(base_path="") routes =
  let open Network.Infix in
  hashchange ~prevent_default:false (fun _ ->
               let hash = Dom_html.window##.location##.hash in
               to_parts (Js.to_string hash)
             ) >>= fun path ->
  (last ~init:(fun t  -> ()) <$> event ()) >>= fun flush ->
  (last ~init:(fun () -> ()) <$> event ()) >>= fun stop  ->

  initially (fun () ->
      let hash = Js.to_string Dom_html.window##.location##.hash in
      let get_parts_from_hash () = to_parts hash in
      let get_parts_from_path () =
        to_parts (Js.to_string Dom_html.window##.location##.pathname)
      in
      if hash = ""
      then let _ = trigger path (get_parts_from_path ()) in ()
      else
        let parts_from_hash = get_parts_from_hash () in
        let parts = match search_routes routes parts_from_hash with
          | Some _ -> parts_from_hash
          | None   -> get_parts_from_path ()
        in let _ = trigger path parts in ()
    )

  >> perform (let open Behaviour.Infix in
              (fun x y -> (x, y)) <$> flush <*> Behaviour.time)
             (fun (flush, t) -> flush t)
  >> perform_state ~finally:(fun f -> f ()) ~init:(fun () -> ()) stop
                   ~f:(fun _ f -> f)

  >> react path stop (fun parts stop_ ->
             match search_routes routes parts with
             | None -> ()
             | Some net ->
                let str = from_parts parts in
                push_state str (base_path ^ str);
                stop_ ();
                let manager = Network.start net in
                let _ = trigger flush (Network.flush manager) in
                let _ = trigger stop (fun () -> Network.stop manager) in
                ()
            )

  >> Network.return path

module type Part = sig
  type t
  type parse_func
  type parse_opt_func
  type 'a generate_func

  val parse        : t -> parse_func     -> route
  val parse_opt    : t -> parse_opt_func -> route
  val generate     : t -> string list generate_func
  val generate_    : string list -> t -> string list generate_func
  val to_fragment  : t -> string generate_func
  val to_fragment_ : string -> t -> string generate_func
end

module Final = struct
  type t = Empty

  type parse_func       = unit Network.t
  type parse_opt_func   = unit Network.t option
  type 'a generate_func = 'a

  let empty = Empty

  let parse _ acc = function
    | [] -> Some acc
    | _  -> None

  let parse_opt _ acc = function
    | [] -> acc
    | _  -> None

  let generate_ acc _ = acc
  let generate _ = []

  let to_fragment_ acc _ = acc
  let to_fragment _ = "#/"
end

module Var (Rest : Part) = struct
  type t = Var of Rest.t

  type parse_func       = string -> Rest.parse_func
  type parse_opt_func   = string -> Rest.parse_opt_func
  type 'a generate_func = string -> 'a Rest.generate_func

  let var rest = Var rest

  let parse (Var rest) f = function
    | [] -> None
    | s :: parts -> Rest.parse rest (f s) parts

  let parse_opt (Var rest) f = function
    | [] -> None
    | s :: parts -> Rest.parse_opt rest (f s) parts

  let generate_ acc (Var rest) s = Rest.generate_ (acc @ [s]) rest
  let generate = generate_ []

  let to_fragment_ acc (Var rest) s = Rest.to_fragment_ (acc ^ "/" ^ s) rest
  let to_fragment = to_fragment_ "#"
end

module Const (Rest : Part) = struct
  type t = Const of string * Rest.t

  type parse_func       = Rest.parse_func
  type parse_opt_func   = Rest.parse_opt_func
  type 'a generate_func = 'a Rest.generate_func

  let const str rest = Const (str, rest)

  let parse (Const (str, rest)) f = function
    | s :: parts when s = str -> Rest.parse rest f parts
    | _ -> None

  let parse_opt (Const (str, rest)) f = function
    | s :: parts when s = str -> Rest.parse_opt rest f parts
    | _ -> None

  let generate_ acc (Const (s, rest)) = Rest.generate_ (acc @ [s]) rest
  let generate = generate_ []

  let to_fragment_ acc (Const (s, rest)) =
    Rest.to_fragment_ (acc ^ "/" ^ s) rest
  let to_fragment = to_fragment_ "#"
end

let empty = Final.empty
let ( ^/ ) x y = x y
let ( ^// ) x y = x (y empty)

module CF = Const(Final)
module VF = Var(Final)

module CCF = Const(CF)
module VCF = Var(CF)
module VVF = Var(VF)
module CVF = Const(VF)

module CCCF = Const(CCF)
module CVCF = Const(VCF)
module CVVF = Const(VVF)
module CCVF = Const(CVF)
module VCCF = Var(CCF)
module VVCF = Var(VCF)
module VVVF = Var(VVF)
module VCVF = Var(CVF)

module CCCCF = Const(CCCF)
module CCVCF = Const(CVCF)
module CCVVF = Const(CVVF)
module CCCVF = Const(CCVF)
module CVCCF = Const(VCCF)
module CVVCF = Const(VVCF)
module CVVVF = Const(VVVF)
module CVCVF = Const(VCVF)
module VCCCF = Var(CCCF)
module VCVCF = Var(CVCF)
module VCVVF = Var(CVVF)
module VCCVF = Var(CCVF)
module VVCCF = Var(VCCF)
module VVVCF = Var(VVCF)
module VVVVF = Var(VVVF)
module VVCVF = Var(VCVF)

module CCCCCF = Const(CCCCF)
module CCCVCF = Const(CCVCF)
module CCCVVF = Const(CCVVF)
module CCCCVF = Const(CCCVF)
module CCVCCF = Const(CVCCF)
module CCVVCF = Const(CVVCF)
module CCVVVF = Const(CVVVF)
module CCVCVF = Const(CVCVF)
module CVCCCF = Const(VCCCF)
module CVCVCF = Const(VCVCF)
module CVCVVF = Const(VCVVF)
module CVCCVF = Const(VCCVF)
module CVVCCF = Const(VVCCF)
module CVVVCF = Const(VVVCF)
module CVVVVF = Const(VVVVF)
module CVVCVF = Const(VVCVF)
module VCCCCF = Var(CCCCF)
module VCCVCF = Var(CCVCF)
module VCCVVF = Var(CCVVF)
module VCCCVF = Var(CCCVF)
module VCVCCF = Var(CVCCF)
module VCVVCF = Var(CVVCF)
module VCVVVF = Var(CVVVF)
module VCVCVF = Var(CVCVF)
module VVCCCF = Var(VCCCF)
module VVCVCF = Var(VCVCF)
module VVCVVF = Var(VCVVF)
module VVCCVF = Var(VCCVF)
module VVVCCF = Var(VVCCF)
module VVVVCF = Var(VVVCF)
module VVVVVF = Var(VVVVF)
module VVVCVF = Var(VVCVF)
