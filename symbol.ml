

type sym = int;;

let syms_tab = [|'A'; 'B'; 'C'; 'D'; 'E'; 'F'; 'G'; 'H'; 'I'; 'J'; 'K'; 'L'; 'M'; 'N'; 'O'; 'P'; 'Q'; 'R'; 'S'; 'T'; 'U'; 'V'; 'W'; 'X'; 'Y'; 'Z'|];;


let a = 0;;

let nb_syms = Array.length syms_tab;;

let of_char c =
  let res = (ref 0) in
  for i = 0 to nb_syms - 1 do
    if syms_tab.(i) = c then
      res := i;
  done;
  !res
;;

let to_char s = syms_tab.(s) ;;

let of_int i = i ;;

let to_int s = s ;;

let next s = (s + 1) mod nb_syms ;;

let (++) s1 s2 = (s1 + s2) mod nb_syms ;;

let (--) s1 s2 =
  (s1 - s2 + (nb_syms)) mod nb_syms;;

let iter f =
for i = 0 to nb_syms - 1 do
  f i;
done;
;;

let fold f seed =
  let res = ref seed in
  for i = 0 to nb_syms - 1 do
    res := f !res i;
  done;
  !res
;;

let rec pow a b =
  if b = 0 then
    1
  else
    a * pow a (b-1)
;;

module Set =
  struct
    type t = int ;;
    let empty = 0 ;;

    let member symbol set =
      let rec member_aux s set =
        if s = symbol then
          set >= pow 2 s
        else
          if set >= pow 2 s then
            member_aux (s-1) (set - (pow 2 s))
          else
            member_aux (s-1) (set)
      in member_aux (nb_syms - 1) set
    ;;


    let add symbol set =
      (if not (member symbol set) then
        pow 2 symbol
      else
        0)
      + set
    ;;

    let singleton symbol = pow 2 symbol ;;

  end

module Map =
struct

  type 'a t = 'a array ;;

  let get t symbol = t.(symbol) ;;

  let set t symbol new_value = t.(symbol) <- new_value ;;

  let make elem = Array.make (nb_syms) elem ;;

  let init f =
    let res = Array.make (nb_syms) (f 0) in
    for i = 1 to nb_syms - 1 do
      res.(i) <- f i;
    done;
    res ;;

  let copy t =
    let res = Array.make (nb_syms) t.(0) in
    for i = 1 to (nb_syms) - 1 do
      res.(i) <- t.(i);
    done;
    res ;;

  let map f t =
    let res = Array.make (nb_syms) (f t.(0)) in
    for i = 0 to (nb_syms) - 1 do
      res.(i) <- f t.(i);
    done;
    res ;;

  let inverse t =
    let res = Array.make (nb_syms) 0 in
    for i = 0 to (nb_syms) - 1 do
      res.(t.(i)) <- i;
    done;
    res ;;

  let print_tmap out_channel t =
    for i = 0 to Array.length t - 1 do
      (Printf.fprintf out_channel "%c is mapped to %c\n" (to_char i) (to_char t.(i)));
    done
  ;;
end
