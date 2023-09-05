exception Read_error of string

let read_error fmt = Printf.ksprintf (fun s -> raise (Read_error s)) fmt
let magic_string = "\147NUMPY"
let magic_string_len = String.length magic_string

module Reader = struct
  type t =
    { buf : Bytes.t
    ; mutable pos : int
    }

  let length t = Bytes.length t.buf

  let direct (type k l) t i kind : k =
    match (kind : (k, l) Bigarray.kind) with
    | Bigarray.Float32 -> Int32.float_of_bits (Bytes.get_int32_ne t.buf i)
    | Float64 -> Int64.float_of_bits (Bytes.get_int64_ne t.buf i)
    | Int32 -> Bytes.get_int32_ne t.buf i
    | Int64 -> Bytes.get_int64_ne t.buf i
    | _ -> failwith "unsupported kind"

  let load_data str = { buf = Bytes.of_string str; pos = 0 }

  let read t dst offset len =
    Bytes.blit t.buf t.pos dst offset len;
    t.pos <- t.pos + len;
    len
end

let map_file file_descr ~pos kind layout shape =
  let elt_size = Bigarray.kind_size_in_bytes kind in
  let pos = Int64.to_int pos in
  let is_scalar = Array.length shape = 0 in
  let len = (Reader.length file_descr - pos) / elt_size in
  let flat =
    Bigarray.Array1.init kind layout len (fun i ->
      Reader.direct file_descr ((i * elt_size) + pos) kind)
  in
  let array =
    Bigarray.reshape
      (Bigarray.genarray_of_array1 flat)
      (if is_scalar then [| 1 |] else shape)
  in
  if is_scalar then Bigarray.reshape array [||] else array

let really_read fd len =
  let buffer = Bytes.create len in
  let rec loop offset =
    let read = Reader.read fd buffer offset (len - offset) in
    if read + offset < len
    then loop (read + offset)
    else if read = 0
    then read_error "unexpected eof"
  in
  loop 0;
  Bytes.to_string buffer

module Header = struct
  type packed_kind = P : (_, _) Bigarray.kind -> packed_kind

  type t =
    { kind : packed_kind
    ; fortran_order : bool
    ; shape : int array
    }

  let split str ~on =
    let parens = ref 0 in
    let indexes = ref [] in
    for i = 0 to String.length str - 1 do
      match str.[i] with
      | '(' -> incr parens
      | ')' -> decr parens
      | c when !parens = 0 && c = on -> indexes := i :: !indexes
      | _ -> ()
    done;
    List.fold_left
      (fun (prev_p, acc) index ->
        index, String.sub str (index + 1) (prev_p - index - 1) :: acc)
      (String.length str, [])
      !indexes
    |> fun (first_pos, acc) -> String.sub str 0 first_pos :: acc

  let trim str ~on =
    let rec loopr start len =
      if len = 0
      then start, len
      else if List.mem str.[start + len - 1] on
      then loopr start (len - 1)
      else start, len
    in
    let rec loopl start len =
      if len = 0
      then start, len
      else if List.mem str.[start] on
      then loopl (start + 1) (len - 1)
      else loopr start len
    in
    let start, len = loopl 0 (String.length str) in
    String.sub str start len

  let parse header =
    let header_fields =
      trim header ~on:[ '{'; ' '; '}'; '\n' ]
      |> split ~on:','
      |> List.map String.trim
      |> List.filter (fun s -> String.length s > 0)
      |> List.map (fun header_field ->
           match split header_field ~on:':' with
           | [ name; value ] ->
             trim name ~on:[ '\''; ' ' ], trim value ~on:[ '\''; ' '; '('; ')' ]
           | _ -> read_error "unable to parse field %s" header_field)
    in
    let find_field field =
      try List.assoc field header_fields with
      | Not_found -> read_error "cannot find field %s" field
    in
    let kind =
      let kind = find_field "descr" in
      (match kind.[0] with
       | '|' | '=' -> ()
       | '>' ->
         if not Sys.big_endian then read_error "big endian data but arch is little endian"
       | '<' ->
         if Sys.big_endian then read_error "little endian data but arch is big endian"
       | otherwise -> read_error "incorrect endianness %c" otherwise);
      match String.sub kind 1 (String.length kind - 1) with
      | "f4" -> P Float32
      | "f8" -> P Float64
      | "i4" -> P Int32
      | "i8" -> P Int64
      | "u1" -> P Int8_unsigned
      | "i1" -> P Int8_signed
      | "u2" -> P Int16_unsigned
      | "i2" -> P Int16_signed
      | "S1" -> P Char
      | "c8" -> P Complex32
      | "c16" -> P Complex64
      | otherwise -> read_error "incorrect descr %s" otherwise
    in
    let fortran_order =
      match find_field "fortran_order" with
      | "False" -> false
      | "True" -> true
      | otherwise -> read_error "incorrect fortran_order %s" otherwise
    in
    let shape =
      find_field "shape"
      |> split ~on:','
      |> List.map String.trim
      |> List.filter (fun s -> String.length s > 0)
      |> List.map int_of_string
      |> Array.of_list
    in
    { kind; fortran_order; shape }
end

type packed_array = P : (_, _, _) Bigarray.Genarray.t -> packed_array
type packed_array1 = P1 : (_, _, _) Bigarray.Array1.t -> packed_array1
type packed_array2 = P2 : (_, _, _) Bigarray.Array2.t -> packed_array2
type packed_array3 = P3 : (_, _, _) Bigarray.Array3.t -> packed_array3

let read_raw data =
  let file_descr = Reader.load_data data in
  let pos, header =
    try
      let magic_string' = really_read file_descr magic_string_len in
      if magic_string <> magic_string' then read_error "magic string mismatch";
      let version = really_read file_descr 2 |> fun v -> v.[0] |> Char.code in
      let header_len_len =
        match version with
        | 1 -> 2
        | 2 -> 4
        | _ -> read_error "unsupported version %d" version
      in
      let header, header_len =
        really_read file_descr header_len_len
        |> fun str ->
        let header_len = ref 0 in
        for i = String.length str - 1 downto 0 do
          header_len := (256 * !header_len) + Char.code str.[i]
        done;
        really_read file_descr !header_len, !header_len
      in
      let header = Header.parse header in
      Int64.of_int (header_len + header_len_len + magic_string_len + 2), header
    with
    | exn -> raise exn
  in
  let (Header.P kind) = header.kind in
  let build layout =
    let array = map_file file_descr ~pos kind layout header.shape in
    P array
  in
  if header.fortran_order then build Fortran_layout else build C_layout

(** Type equalities module, used in conversion function *)
module Eq = struct
  (** An equality type to extract type equalities *)
  type ('a, 'b) t = W : ('a, 'a) t

  open Bigarray

  (** Type equalities for bigarray kinds *)
  module Kind = struct
    let ( === )
      : type a b c d. (a, b) kind -> (c, d) kind -> ((a, b) kind, (c, d) kind) t option
      =
     fun x y ->
      match x, y with
      | Float32, Float32 -> Some W
      | Float64, Float64 -> Some W
      | Int8_signed, Int8_signed -> Some W
      | Int8_unsigned, Int8_unsigned -> Some W
      | Int16_signed, Int16_signed -> Some W
      | Int16_unsigned, Int16_unsigned -> Some W
      | Int32, Int32 -> Some W
      | Int64, Int64 -> Some W
      | Int, Int -> Some W
      | Nativeint, Nativeint -> Some W
      | Complex32, Complex32 -> Some W
      | Complex64, Complex64 -> Some W
      | Char, Char -> Some W
      | _ -> None
  end

  (** Type equalities for layout *)
  module Layout = struct
    let ( === ) : type a b. a layout -> b layout -> (a layout, b layout) t option =
     fun x y ->
      match x, y with
      | Fortran_layout, Fortran_layout -> Some W
      | C_layout, C_layout -> Some W
      | _, _ -> None
  end
end

(** Conversion functions from packed arrays to bigarrays *)

let to_bigarray
  (type a b c)
  (layout : c Bigarray.layout)
  (kind : (a, b) Bigarray.kind)
  (P x)
  =
  match Eq.Layout.(Bigarray.Genarray.layout x === layout) with
  | None -> None
  | Some Eq.W ->
    (match Eq.Kind.(Bigarray.Genarray.kind x === kind) with
     | None -> None
     | Some Eq.W -> Some (x : (a, b, c) Bigarray.Genarray.t))

let to_bigarray1
  (type a b c)
  (layout : c Bigarray.layout)
  (kind : (a, b) Bigarray.kind)
  (P1 x)
  =
  match Eq.Layout.(Bigarray.Array1.layout x === layout) with
  | None -> None
  | Some Eq.W ->
    (match Eq.Kind.(Bigarray.Array1.kind x === kind) with
     | None -> None
     | Some Eq.W -> Some (x : (a, b, c) Bigarray.Array1.t))

let to_bigarray2
  (type a b c)
  (layout : c Bigarray.layout)
  (kind : (a, b) Bigarray.kind)
  (P2 x)
  =
  match Eq.Layout.(Bigarray.Array2.layout x === layout) with
  | None -> None
  | Some Eq.W ->
    (match Eq.Kind.(Bigarray.Array2.kind x === kind) with
     | None -> None
     | Some Eq.W -> Some (x : (a, b, c) Bigarray.Array2.t))

let to_bigarray3
  (type a b c)
  (layout : c Bigarray.layout)
  (kind : (a, b) Bigarray.kind)
  (P3 x)
  =
  match Eq.Layout.(Bigarray.Array3.layout x === layout) with
  | None -> None
  | Some Eq.W ->
    (match Eq.Kind.(Bigarray.Array3.kind x === kind) with
     | None -> None
     | Some Eq.W -> Some (x : (a, b, c) Bigarray.Array3.t))
