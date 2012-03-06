
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
