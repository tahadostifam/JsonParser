type json =
  | JNull
  | JBool of bool
  | JNumber of float
  | JString of string
  | JArray of json list
  | JObject of (string * json) list
;;

let parse_prefix prefix str =
  let str_len = String.length str in
  let prefix_len = String.length prefix in
  if str_len >= prefix_len && String.sub str 0 prefix_len = prefix then
    Some (String.sub str prefix_len (str_len - prefix_len))
  else
    None
;;

let rec skip_white_space str =
  if str = "" then ""
  else
    match str.[0] with
    | ' ' | '\n' | '\r' | '\t' ->
        skip_white_space (String.sub str 1 (String.length str - 1))
    | _ -> str
;;

let parse_number str =
  let str_len = String.length str in
  let rec find_number_end i =
    if i >= str_len then i
    else
      match str.[i] with
      | '0' .. '9' | '-' | '+' | '.' | 'e' | 'E' -> find_number_end (i + 1)
      | _ -> i
  in
  let end_idx = find_number_end 0 in
  if end_idx = 0 then failwith "Invalid number"
  else
    let num_str = String.sub str 0 end_idx in
    let rest = String.sub str end_idx (str_len - end_idx) in
    (JNumber (float_of_string num_str), rest)
;;

let parse_string str =
  if str = "" || str.[0] <> '"' then failwith "Expected '\"' at start of string"
  else
    let rec aux i acc =
      if i >= String.length str then failwith "Unterminated string"
      else
        match str.[i] with
        | '"' ->
            let s = String.concat "" (List.rev acc) in
            let rest = String.sub str (i + 1) (String.length str - i - 1) in
            (JString s, rest)
        | '\\' ->
            if i + 1 >= String.length str then failwith "Invalid escape"
            else
              let next = str.[i + 1] in
              let ch =
                match next with
                | '"' -> "\""
                | '\\' -> "\\"
                | '/' -> "/"
                | 'b' -> "\b"
                | 'f' -> "\012"
                | 'n' -> "\n"
                | 'r' -> "\r"
                | 't' -> "\t"
                | _ -> failwith ("Unknown escape: " ^ String.make 1 next)
              in
              aux (i + 2) (ch :: acc)
        | c -> aux (i + 1) ((String.make 1 c) :: acc)
    in
    aux 1 []
;;

let rec parse_value str =
  let str = skip_white_space str in
  if str = "" then failwith "Unexpected end of input."
  else
    match str.[0] with
    | 'n' ->
        (match parse_prefix "null" str with
        | Some rest -> (JNull, rest)
        | None -> failwith "Invalid null.")
    | 't' ->
        (match parse_prefix "true" str with
        | Some rest -> (JBool true, rest)
        | None -> failwith "Invalid true.")
    | 'f' ->
        (match parse_prefix "false" str with
        | Some rest -> (JBool false, rest)
        | None -> failwith "Invalid false.")
    | '"' -> parse_string str
    | '[' -> parse_array str
    | '{' -> parse_object str
    | ch when ('0' <= ch && ch <= '9') || ch = '-' -> parse_number str
    | _ -> failwith ("Unexpected character: " ^ String.make 1 str.[0])

and parse_array str =
  let rest = String.sub str 1 (String.length str - 1) |> skip_white_space in
  let rec aux acc s =
    let s = skip_white_space s in
    if s = "" then failwith "Unterminated array"
    else
      match s.[0] with
      | ']' ->
          let rest_after = String.sub s 1 (String.length s - 1) in
          (JArray (List.rev acc), rest_after)
      | _ ->
          let (value, rest1) = parse_value s in
          let rest1 = skip_white_space rest1 in
          if rest1 <> "" && rest1.[0] = ',' then
            aux (value :: acc)
              (String.sub rest1 1 (String.length rest1 - 1))
          else
            aux (value :: acc) rest1
  in
  aux [] rest

and parse_object str =
  let rest = String.sub str 1 (String.length str - 1) |> skip_white_space in
  let rec aux acc s =
    let s = skip_white_space s in
    if s = "" then failwith "Unterminated object"
    else
      match s.[0] with
      | '}' ->
          let rest_after = String.sub s 1 (String.length s - 1) in
          (JObject (List.rev acc), rest_after)
      | '"' ->
          let (key_json, rest1) = parse_string s in
          (match key_json with
          | JString key ->
              let rest1 = skip_white_space rest1 in
              if rest1 = "" || rest1.[0] <> ':' then
                failwith "Expected ':' after key";
              let rest2 =
                String.sub rest1 1 (String.length rest1 - 1) |> skip_white_space
              in
              let (value, rest3) = parse_value rest2 in
              let rest3 = skip_white_space rest3 in
              if rest3 <> "" && rest3.[0] = ',' then
                aux
                  ((key, value) :: acc)
                  (String.sub rest3 1 (String.length rest3 - 1))
              else
                aux ((key, value) :: acc) rest3
          | _ -> failwith "Expected string key in object")
      | _ -> failwith "Expected string key in object"
  in
  aux [] rest
;;

let rec string_of_json j =
  match j with
  | JNull -> "null"
  | JBool b -> if b then "true" else "false"
  | JNumber n -> string_of_float n
  | JString s -> "\"" ^ String.escaped s ^ "\""
  | JArray lst ->
      "[" ^ String.concat ", " (List.map string_of_json lst) ^ "]"
  | JObject obj ->
      let pairs =
        List.map (fun (k, v) -> "\"" ^ k ^ "\": " ^ string_of_json v) obj
      in
      "{" ^ String.concat ", " pairs ^ "}"
;;

let parse_json str =
  let (value, rest) = parse_value str in
  let rest = skip_white_space rest in
  if rest <> "" then failwith ("Unexpected trailing input: " ^ rest)
  else value
;;

let rec repl () =
  print_string "> ";
  flush stdout;
  match read_line () with
  | exception End_of_file -> print_endline "Program exit!"
  | line ->
      if line = "quit" || line = "exit" then
        print_endline "Program exit!"
      else (
        try
          let value = parse_json line in
          print_endline (string_of_json value)
        with Failure msg ->
          Printf.printf "Parse error: %s\n" msg;
        repl ())
;;

let _ = repl ()
