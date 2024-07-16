module Basic = struct
  module Encode = Ason.Encode.Extra (struct
    type json = Yojson.Basic.t
    type 'a encoder = 'a -> json

    let encode encode x = Yojson.Basic.to_string (encode x)
    let null = `Null
    let json = Fun.id
    let int x = `Int x
    let float x = `Float x
    let bool x = `Bool x
    let string x = `String x
    let singleton encode x = `List [ encode x ]
    let nullable encode opt = match opt with Some x -> encode x | None -> null
    let obj xs = `Assoc xs
    let dict encode xs = `Assoc (List.map (fun (k, v) -> (k, encode v)) xs)
    let list encode list = `List (List.map encode list)
    let array encode arr = `List (List.map encode (Array.to_list arr))
  end)

  module Decode = Ason.Decode.Extra (struct
    type json = Yojson.Basic.t
    type 'a decoder = json -> ('a, exn) result

    let parse decoder str =
      try
        let json = Yojson.Basic.from_string str in
        decoder json
      with exn -> Error (Ason.Decode.exn exn)

    let null json =
      match json with
      | `Null -> Ok ()
      | _ -> Error (Ason.Decode.type_error "null")

    let json json = Ok json

    let bool json =
      match json with
      | `Bool x -> Ok x
      | _ -> Error (Ason.Decode.type_error "bool")

    let int json =
      match json with
      | `Int x -> Ok x
      | _ -> Error (Ason.Decode.type_error "int")

    let float json =
      match json with
      | `Float x -> Ok x
      | _ -> Error (Ason.Decode.type_error "float")

    let string json =
      match json with
      | `String x -> Ok x
      | _ -> Error (Ason.Decode.type_error "string")

    let list decode json =
      match json with
      | `List xs0 ->
          let rec loop i xs acc =
            match xs with
            | [] -> Ok (List.rev acc)
            | x :: xs' -> (
                match decode x with
                | Ok y -> loop (i + 1) xs' (y :: acc)
                | Error err -> Error (Ason.Decode.array_error i err))
          in
          loop 0 xs0 []
      | _ -> Error (Ason.Decode.type_error "array")

    let array decode json = Result.map Array.of_list (list decode json)

    let singleton decode json =
      match json with
      | `List [ json_item ] -> decode json_item
      | `List _ -> Error (Ason.Decode.type_error "singleton array")
      | _ -> Error (Ason.Decode.type_error "array")

    let pair decode_a decode_b json =
      match json with
      | `List [ a; b ] -> (
          match (decode_a a, decode_b b) with
          | Ok a', Ok b' -> Ok (a', b')
          | Error err, _ -> Error (Ason.Decode.array_error 0 err)
          | _, Error err -> Error (Ason.Decode.array_error 1 err))
      | _ -> Error (Ason.Decode.type_error "array of two elements")

    let rec lookup_field name0 fields =
      match fields with
      | [] -> `Null
      | (name, value) :: _ when String.equal name name0 -> value
      | _ :: obj' -> lookup_field name0 obj'

    let field ?default name decode json =
      match json with
      | `Assoc fields -> (
          match lookup_field name fields with
          | `Null -> (
              match default with
              | None ->
                  Error (Ason.Decode.field_error name Ason.Decode.not_found)
              | Some x -> Ok x)
          | non_null -> decode non_null)
      | _ ->
          Error (Ason.Decode.field_error name (Ason.Decode.type_error "object"))

    let map_fields fk decode_v json =
      match json with
      | `Assoc xs0 ->
          let rec loop xs acc =
            match xs with
            | [] -> Ok (List.rev acc)
            | (k, x) :: xs' -> (
                match decode_v x with
                | Ok y -> loop xs' ((fk k, y) :: acc)
                | Error err -> Error (Ason.Decode.field_error k err))
          in
          loop xs0 []
      | _ -> Error (Ason.Decode.type_error "object")

    let obj json = map_fields Fun.id Result.ok json
    let dict decode json = map_fields Fun.id decode json

    let nullable decode json =
      match json with
      | `Null -> Ok None
      | _ -> (
          match decode json with Ok x -> Ok (Some x) | Error err -> Error err)

    let is_null json = match json with `Null -> true | _ -> false
    let is_bool json = match json with `Bool _ -> true | _ -> false
    let is_int json = match json with `Int _ -> true | _ -> false
    let is_float json = match json with `Float _ -> true | _ -> false
    let is_string json = match json with `Float _ -> true | _ -> false
    let is_array json = match json with `List _ -> true | _ -> false
    let is_obj json = match json with `Assoc _ -> true | _ -> false
  end)
end
