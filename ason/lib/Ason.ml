module type Encode = sig
  type json
  (** Represents JSON values. *)

  type 'a encoder = 'a -> json
  (** A JSON encoder function. *)

  val encode : 'a encoder -> 'a -> string
  (** Encode a value to a string. *)

  val null : json
  (** Is the encoding of the JSON null value. *)

  val json : json encoder
  (** A pass-through encoder for JSON values. *)

  val bool : bool encoder
  (** Encodes bool JSON values. *)

  val int : int encoder
  (** Encodes int JSON values. *)

  val float : float encoder
  (** Encodes float JSON values. *)

  val string : string encoder
  (** Encodes string JSON values. *)

  val list : 'a encoder -> 'a list encoder
  (** Encodes list JSON values. *)

  val array : 'a encoder -> 'a array encoder
  (** Encodes array JSON values. *)

  val nullable : 'a encoder -> 'a option encoder
  (** Encodes an option as a nullable JSON value. *)

  val singleton : 'a encoder -> 'a encoder
  (** Encodes a value as a singleton JSON array. *)

  val obj : (string * json) list encoder
  (** Encodes a list of JSON fields as a JSON object. *)

  val dict : 'a encoder -> (string * 'a) list encoder
  (** Encodes a list of fields of the same type as a JSON object. *)
end

module type Decode = sig
  type json
  (** Represents JSON values. *)

  type 'a decoder = json -> ('a, exn) result
  (** A JSON decoder function. *)

  val parse : 'a decoder -> string -> ('a, exn) result
  (** Decode a [json] value from a string. *)

  val json : json decoder
  (** A pass-through decoder for JSON values. *)

  val null : unit decoder
  (** Decodes a JSON null value. *)

  val bool : bool decoder
  (** Decodes a JSON boolean value. *)

  val int : int decoder
  (** Decodes a JSON number as an int. *)

  val float : float decoder
  (** Decodes a JSON number as a float. *)

  val string : string decoder
  (** Decodes a JSON string value. *)

  val list : 'a decoder -> 'a list decoder
  (** Decodes a JSON array as a list. *)

  val array : 'a decoder -> 'a array decoder
  (** Decodes a JSON array. *)

  val singleton : 'a decoder -> 'a decoder
  (** Decodes a JSON array and then the single object contained in it. *)

  val pair : 'a decoder -> 'b decoder -> ('a * 'b) decoder
  (** Decodes a JSON array with two elements as a pair. *)

  val field : ?default:'a -> string -> 'a decoder -> 'a decoder
  (** [field ?default name obj] is the value for field [name] in [obj]. If the field [name] is
        missing and [default] is provided, [default] is returned. *)

  val obj : (string * json) list decoder
  (** Decodes a JSON object with arbitrary JSON values. *)

  val dict : 'a decoder -> (string * 'a) list decoder
  (** Decodes a JSON object with values of the same type. *)

  val nullable : 'a decoder -> 'a option decoder
  (** Decodes a JSON value that may be null as an option. *)

  val is_null : json -> bool
  (** Checks if a JSON value is a null. *)

  val is_bool : json -> bool
  (** Checks if a JSON value is a bool. *)

  val is_int : json -> bool
  (** Checks if a JSON value is an int. *)

  val is_float : json -> bool
  (** Checks if a JSON value is a float. *)

  val is_string : json -> bool
  (** Checks if a JSON value is a string. *)

  val is_array : json -> bool
  (** Checks if a JSON value is an array. *)

  val is_obj : json -> bool
  (** Checks if a JSON value is an array. *)
end

module Encode = struct
  module type Extra = sig
    include Encode

    val map : ('a -> 'b) -> 'b encoder -> 'a encoder
  end

  module Extra (E : Encode) : Extra with type json = E.json = struct
    include E

    let map f encoder x = encoder (f x)
  end
end

module Decode = struct
  exception Field_error of string * exn
  exception Array_error of int * exn
  exception Type_error of (string * string option)
  exception Or_error of exn * exn

  let field_error name exn = Field_error (name, exn)
  let array_error i exn = Array_error (i, exn)
  let type_error ?ctx expected = Type_error (expected, ctx)
  let not_found = Not_found
  let exn exn = exn

  let rec error_to_string (err : exn) =
    match err with
    | Field_error (name, err) ->
        String.concat "" [ "field \""; name; "\": "; error_to_string err ]
    | Array_error (i, err) ->
        String.concat ""
          [ "element at index "; string_of_int i; ": "; error_to_string err ]
    | Type_error (expected, Some ctx) ->
        String.concat "" [ "in "; ctx; "expected "; expected ]
    | Type_error (expected, None) -> String.concat "" [ "expected "; expected ]
    | Or_error (e1, e2) ->
        String.concat ""
          [
            "both decoders failed: ";
            error_to_string e1;
            ";";
            error_to_string e2;
          ]
    | Not_found -> "missing"
    | exn -> Printexc.to_string exn

  let () =
    Printexc.register_printer (fun exn ->
        match exn with
        | Field_error _ | Array_error _ | Type_error _ | Or_error _ ->
            Some (error_to_string exn)
        | _ -> None)

  module type Extra = sig
    include Decode

    val ignore : unit decoder

    val decode : 'a decoder -> json -> ('a, exn) result
    (** Decode a [json] value. *)

    val error_to_string : exn -> string
    val decode_or_fail : 'a decoder -> json -> 'a
    val parse_or_fail : 'a decoder -> string -> 'a
    val map : ('a -> 'b) -> 'a decoder -> 'b decoder
    val map_option : ('a -> 'b option) -> 'a decoder -> 'b decoder
    val ( or ) : 'a decoder -> 'a decoder -> 'a decoder
  end

  module Extra (D : Decode) : Extra with type json = D.json = struct
    include D

    let ignore _ = Ok ()
    let decode decoder (json : json) = decoder json

    let decode_or_fail decoder json =
      match decoder json with
      | Ok x -> x
      | Error err -> failwith (error_to_string err)

    let parse_or_fail decoder string =
      match parse decoder string with
      | Ok x -> x
      | Error err -> failwith (error_to_string err)

    let error_to_string = error_to_string

    let map f decoder json =
      match decoder json with Ok x -> Ok (f x) | Error err -> Error err

    let map_option f decoder json =
      match decoder json with
      | Ok x -> (
          match f x with
          | Some x' -> Ok x'
          | None -> Error (Failure "map_option"))
      | Error err -> Error err

    let ( or ) dec1 dec2 json =
      match dec1 json with
      | Ok x -> Ok x
      | Error err1 -> (
          match dec2 json with
          | Ok x -> Ok x
          | Error err2 -> Error (Or_error (err1, err2)))
  end
end
