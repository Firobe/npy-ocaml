type packed_array = P : (_, _, _) Bigarray.Genarray.t -> packed_array
type packed_array1 = P1 : (_, _, _) Bigarray.Array1.t -> packed_array1
type packed_array2 = P2 : (_, _, _) Bigarray.Array2.t -> packed_array2
type packed_array3 = P3 : (_, _, _) Bigarray.Array3.t -> packed_array3

(** [read_mmap filename ~shared] returns a packed bigarray mmaped to the content
    of [filename]. If [shared] is [true] modifications made to the array are reflected
    to the file. *)
val read_raw : string -> packed_array


(** Conversion functions from packed arrays to bigarrays *)

(** [to_bigarray layout kind packed_array] returns [Some a] with
    [a] a [Bigarray.Genarray.t] if the layout and the kind of [packed_array]
    were equal to the [layout] and [kind] arguments. Otherwise, [to_bigarray]
    returns [None]
*)
val to_bigarray :
     'c Bigarray.layout
  -> ('a, 'b) Bigarray.kind
  -> packed_array
  -> ('a, 'b, 'c) Bigarray.Genarray.t option

(** Same as {!to_bigarray} for [Bigarray.Array1.t] *)
val to_bigarray1 :
     'c Bigarray.layout
  -> ('a, 'b) Bigarray.kind
  -> packed_array1
  -> ('a, 'b, 'c) Bigarray.Array1.t option

(** Same as {!to_bigarray} for [Bigarray.Array2.t] *)
val to_bigarray2 :
     'c Bigarray.layout
  -> ('a, 'b) Bigarray.kind
  -> packed_array2
  -> ('a, 'b, 'c) Bigarray.Array2.t option

(** Same as {!to_bigarray} for [Bigarray.Array3.t} *)
val to_bigarray3 :
     'c Bigarray.layout
  -> ('a, 'b) Bigarray.kind
  -> packed_array3
  -> ('a, 'b, 'c) Bigarray.Array3.t option
