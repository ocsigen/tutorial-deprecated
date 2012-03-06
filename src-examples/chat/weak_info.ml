open Utils

let debug fmt = debug_prefix "Weak_info"

module type KEY = sig
  type t
  include Hashtbl.HashedType with type t := t
end

module type INFO = sig
  type t
  val prototype : t
end

(** This module provides an easy way to associate information (provided in the [Info] module argument) to some given
    data (specified through the [Key] module argument) while the entry might be garbage-collected whenever the key-value
    is.
    It adds the functions [find_by_key], [mem_by_key], and [add_by_key] to the functionality of [Weak.S]. Those
    functions work similar like the functions [find], [mem], and [add] from [Hashtbl.S].
  *)

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
