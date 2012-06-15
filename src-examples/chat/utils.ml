

let debug_prefix prefix =
  fun fmt ->
    let k channel = output_char channel '\n'; flush channel in
    Printf.kfprintf k stderr (prefix^^"> "^^fmt)

let identity x = x
let flip f x y = f y x
let const x = fun _ -> x

let curry f = fun x y -> f (x, y)
let uncurry f = fun (x, y) -> f x y

let on f g = fun x y -> g (f x) (f y)

(** [tap f] corresponds to [identity] with a side-effect given in [f]. *)
let tap f x = f x; x
let undefined _ = failwith "undefined"

let singleton x = [x]
let cons h t = h :: t

let some x = Some x
let get_some = function Some x -> x | None -> failwith "get_some"
let get_option ~default = function Some x -> x | None -> default
let iter_option f = function Some x -> f x | None -> ()

let pair x y = (x, y)
let flip_pair (x, y) = y, x
let first f (x, y) = f x, y
let second f (x, y) = x, f y

let ( // ) = flip
let ( **> ) f x = f x
let ( |> ) x f = x f (* %revapply *)
let ( -| ) f g = fun x -> f (g x)
let ( |- ) f g = fun x -> g (f x)

let push x xs = xs := x :: !xs

let modify ref f = ref := f !ref
let set ref x = ref := x

module Weak_info = struct

  module type KEY = sig
    type t
    include Hashtbl.HashedType with type t := t
  end

  module type INFO = sig
    type t
    val prototype : t
  end

  (** This module provides an easy way to associate information (provided in the
      [Info] module argument) to some given data (specified through the [Key]
      module argument) while the entry might be garbage-collected whenever the
      key-value is.
      It adds the functions [find_by_key], [mem_by_key], and [add_by_key] to the
      functionality of [Weak.S]. Those functions work similar like the functions
      [find], [mem], and [add] from [Hashtbl.S].  *)

  module Make (Key : KEY) (Info : INFO) = struct

    module E = struct

      type t = {
        key : Key.t;
        info : Info.t;
      }

      let key { key } = key
      let info { info } = info
      let key_info { key; info } = key, info

      let equal = on key Key.equal
      let hash = Key.hash -| key
    end

    module W = Weak.Make (E)

    let create = W.create
    let fold = W.fold

    let find_by_key table key =
      E.info (W.find table { E.key; info = Info.prototype })

    let mem_by_key table key =
      W.mem table { E.key; info = Info.prototype }

    let add_by_key table key info =
      W.remove table { E.key; info = Info.prototype };
      let data = { E.key; info } in
      W.add table data;
      data

  end
end
