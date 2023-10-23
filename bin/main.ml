exception InvalidToken of string;;

type token_type =
  | ILLEGAL of string
  | EOF
  | LET
  | IDENT of string
  | ASSIGN
  | INT of int
  | STRING of string
  | FUNCTION
  | PLUS
  | COMMA
  | SEMICOLON
  | LBRACE (* { *)
  | RBRACE (* } *)
  | LPAREN (* ( *)
  | RPAREN (* ) *)
  | RBRACK (* ] *)
  | LBRACK (* [ *)
  | GT
  | LT
  | MINUS
  | SLASH
  | BANG
  | ASTERISK
  | TRUE
  | FALSE
  | RETURN
  | IF
  | ELSE
  | EQUAL
  | NOTEQUAL
;;

type lexer = {
  input : string;
  pos : int;
  ch : char option;
};;

let init_lexer input =
  if String.empty == input
  then { input ; pos = 0 ; ch = None}
  else { input ; pos = 0 ; ch = Some (String.get input 0)}
;;

let empty_lexer lexer = { lexer with ch = None};;
let advanced_lexer lexer = let pos = lexer.pos + 1 in { lexer with pos; ch = Some(String.get lexer.input pos)};;
let at_end lexer = lexer.pos == String.length lexer.input -1;;

let advance lexer =
  if at_end lexer
  then empty_lexer lexer
  else advanced_lexer lexer
;;

(* let rec eat_comment lexer = 
  if at_end lexer
  then empty_lexer lexer
  else match lexer.ch with 
  | None -> lexer
  | Some x -> match x with
    |'\n' -> lexer 
    | _ -> eat_comment (advance lexer) *)

(* let peek lexer  = let al = advance lexer in al, al.ch *)

let inspector_equals lexer = 
  match lexer.ch with 
  | None -> lexer, ASSIGN 
  | Some x -> match x with 
  | '=' -> advance lexer, EQUAL 
  | _ -> lexer, ASSIGN

let insepctor_exclaimation lexer = 
  match lexer.ch with 
  | None -> lexer, BANG 
  | Some x -> match x with 
  | '=' -> advance lexer, NOTEQUAL 
  | _ -> lexer, BANG

let rec eat lexer = match lexer.ch with
  | None -> lexer
  | Some x -> match x with
    | ' ' | '\t' | '\n' | '\r' -> eat (advance lexer)
    | _ -> lexer
;;

let is_digit = function '0' .. '9' -> true | _ -> false 
let is_alpha = function | '_' | 'a' .. 'z' | 'A' .. 'Z' -> true | _ -> false

let rec read_string lexer =
  let lexer = advance lexer in
  let rec aux lexer acc =
    match lexer.ch with
    | None -> raise (InvalidToken "Expected end of string")
    | Some x -> (
      match x with
      | '"' -> advance lexer, STRING (String.of_seq (List.to_seq (List.rev acc)))
      | x -> aux (advance lexer) (x::acc)
    )
  in aux lexer []

let rec read_identifier lexer acc = match lexer.ch with 
  | None -> lexer, acc
  | Some x -> if is_alpha x
     then read_identifier (advance lexer) (acc ^ (Char.escaped x))
     else lexer, acc
;;

let rec read_number lexer acc = match lexer.ch with
  | None -> lexer, int_of_string acc
  | Some x -> match x with 
    | x when is_digit x -> read_number (advance lexer) (acc ^ Char.escaped x)
    | _ -> lexer, int_of_string acc
;;

let lookup key = match key with
  | "let" -> LET
  | "fn" -> FUNCTION
  | "true" -> TRUE
  | "false" -> FALSE
  | "return" -> RETURN
  | "if" -> IF
  | "else" -> ELSE
  | x -> IDENT(x)
;;

let next_token lexer = 
  let lexer = eat lexer in
   match lexer.ch with
  | None -> lexer, EOF
  | Some x -> match x with
    | ';' -> advance lexer, SEMICOLON
    | '(' -> advance lexer, LPAREN
    | ')' -> advance lexer, RPAREN
    | '{' -> advance lexer, LBRACE
    | '}' -> advance lexer, RBRACE
    | '+' -> advance lexer, PLUS
    | ',' -> advance lexer, COMMA
    | '>' -> advance lexer, GT
    | '<' -> advance lexer, LT
    | '-' -> advance lexer, MINUS
    | '/' -> advance lexer, SLASH
    | '*' -> advance lexer, ASTERISK 
    | '[' -> advance lexer, LBRACK
    | ']' -> advance lexer, RBRACK
    | '"' -> read_string lexer
    | '!' -> insepctor_exclaimation (advance lexer)
    | '=' -> inspector_equals (advance lexer)
    | x when is_alpha x -> let (l, tok) = read_identifier lexer "" in l, lookup tok
    | x when is_digit x -> let (l, tok) = read_number (advance lexer) (Char.escaped x) in l, INT(tok)
    | x -> (empty_lexer lexer), ILLEGAL(Char.escaped x)
;;

let _print_token token = match token with
| ILLEGAL x -> ("ILLEGAL(" ^ x ^ ")")
| PLUS -> "PLUS"
| COMMA -> "COMMA"
| SEMICOLON -> "SEMICOLON"
| LPAREN -> "LPAREN"
| RPAREN -> "RPAREN"
| LBRACE -> "LBRACE"
| RBRACE -> "RBRACE"
| ASSIGN -> "ASSIGN"
| IDENT x -> ("IDENT(" ^ x ^ ")")
| EOF -> "EOF"
| LET -> "LET"
| FUNCTION -> "FUNCTION"
| INT x -> ("INT(" ^ string_of_int x ^ ")")
| BANG -> "BANG"
| ASTERISK -> "ASTERISK"
| SLASH -> "SLASH"
| LT -> "LESS THAN"
| GT -> "GREATER THAN"
| MINUS -> "MINUS"
| IF -> "IF"
| ELSE -> "ELSE"
| RETURN -> "RETURN"
| TRUE -> "TRUE"
| FALSE -> "FALSE"
| EQUAL -> "EQUAL"
| NOTEQUAL -> "NOTEQUAL"
| STRING x -> "STRING(\"" ^ x ^ "\")"
| LBRACK -> "LBRACK"
| RBRACK -> "RBRACK"
;;

let string_of_token token_type = match token_type with 
  | ILLEGAL _ -> "ILLEGAL"
  | IDENT _ -> "IDENT"
  | INT _ -> "INT"
  | PLUS -> "PLUS"
  | COMMA -> "COMMA"
  | SEMICOLON -> "SEMICOLON"
  | LPAREN -> "LPAREN"
  | RPAREN -> "RPAREN"
  | LBRACE -> "LBRACE"
  | RBRACE -> "RBRACE"
  | ASSIGN -> "ASSIGN"
  | EOF -> "EOF"
  | LET -> "LET"
  | FUNCTION -> "FUNCTION"
  | BANG -> "BANG"
  | ASTERISK -> "ASTERISK"
  | SLASH -> "SLASH"
  | LT -> "LT"
  | GT -> "GT"
  | MINUS -> "MINUS"
  | IF -> "IF"
  | ELSE -> "ELSE"
  | RETURN -> "RETURN"
  | TRUE -> "TRUE"
  | FALSE -> "FALSE"
  | EQUAL -> "EQUAL"
  | NOTEQUAL -> "NOTEQUAL"
  | STRING _ -> "STRING"
  | LBRACK -> "LBRACK"
  | RBRACK -> "RBRACK"
;;

let rec _print_tokens lexer =
  let lexer, token = next_token lexer in
  match token with
  | EOF -> ()
  | _ -> print_endline (match token with
      | ILLEGAL x -> ("ILLEGAL(" ^ x ^ ")")
      | PLUS -> "PLUS"
      | COMMA -> "COMMA"
      | SEMICOLON -> "SEMICOLON"
      | LPAREN -> "LPAREN"
      | RPAREN -> "RPAREN"
      | LBRACE -> "LBRACE"
      | RBRACE -> "RBRACE"
      | ASSIGN -> "ASSIGN"
      | IDENT x -> ("IDENT(" ^ x ^ ")")
      | EOF -> "EOF"
      | LET -> "LET"
      | FUNCTION -> "FUNCTION"
      | INT x -> ("INT(" ^ string_of_int x ^ ")")
      | BANG -> "BANG"
      | ASTERISK -> "ASTERISK"
      | SLASH -> "SLASH"
      | LT -> "LT"
      | GT -> "GT"
      | MINUS -> "MINUS"
      | IF -> "IF"
      | ELSE -> "ELSE"
      | RETURN -> "RETURN"
      | TRUE -> "TRUE"
      | FALSE -> "FALSE"
      | EQUAL -> "EQUAL"
      | NOTEQUAL -> "NOTEQUAL"
      | STRING x -> x
      | LBRACK -> "LBRACK"
      | RBRACK -> "RBRACK"
    );
    _print_tokens lexer
;;

type integer = {token: token_type; value : int;}
type identifier = {token: token_type ; value: string;}
type boolean = {token: token_type ; value : bool}

type parser = {
  current: token_type;
  next: token_type;
  l: lexer;
  prefixParseFns: (string, prefixParseFn) Hashtbl.t;
  infixParseFns: (string, infixParseFn) Hashtbl.t;
} 
and 
prefixParseFn = parser -> parser * expression
and
infixParseFn = parser -> expression -> parser * expression
and  
node =
 | Program of program
 | Statement of statement
 | Expression of expression
 | EOT
and statement =
 | Let of {token: token_type; name: identifier; value: expression;}
 | Return of {token: token_type; expression: expression;}
 | ExpressionStatement of {expression: expression;}
and expression =
 | String of {token: token_type ; value : string}
 | Identifier of identifier
 | Integer of integer
 | Boolean of boolean
 | If of if_expression
 | Function of function_literal
 | PrefixExpression of {token: token_type ; operator : string ; right : expression;}
 | InfixExpression of {token: token_type ; operator : string ; left : expression ; right : expression;}
 | CallExpression of {token: token_type ; fn: expression ; arguments : expression list}
 | Array of {token: token_type ; elements : expression list}
 | IndexExpression of {token: token_type ; left : expression ; index : expression}
 and program = statement list
and block_statement = {token: token_type; statements: statement list}
and if_expression = {token: token_type; condition: expression ; consequence: block_statement; alternative : block_statement option}
and function_literal = {token: token_type ; parameters: identifier list ; body: block_statement;}
;;

let findPrefixFn parser = 
  if Hashtbl.mem parser.prefixParseFns (string_of_token parser.current)
  then Some(Hashtbl.find parser.prefixParseFns (string_of_token parser.current))
  else None
;;

let findInfixFn parser = 
  if Hashtbl.mem parser.infixParseFns (string_of_token parser.current)
  then Some(Hashtbl.find parser.infixParseFns (string_of_token parser.current))
  else None
;;

type order =
  | LOWEST
  | EQUALS 
  | LESSGREATER 
  | SUM 
  | PRODUCT 
  | PREFIX 
  | CALL
  | INDEX [@@deriving enum] 
;;

let order_to_string order = 
  match order with
| LOWEST -> "LOWEST"
| EQUALS -> "EQUALS"
| LESSGREATER -> "LESSGREATER"
| SUM -> "SUM"
| PRODUCT -> "PRODUCT"
| PREFIX -> "PREFIX"
| CALL-> "CALL"
| INDEX -> "INDEX"

let token_to_order token =
  match token with
  | EQUAL -> EQUALS
  | NOTEQUAL -> EQUALS
  | LT -> LESSGREATER
  | GT -> LESSGREATER
  | PLUS -> SUM
  | MINUS -> SUM
  | SLASH -> PRODUCT
  | ASTERISK -> PRODUCT
  | LPAREN -> CALL
  | LBRACK -> INDEX
  | _ -> LOWEST

let order_to_enum order = match order with
  | LOWEST -> 0
  | EQUALS -> 1
  | LESSGREATER -> 2
  | SUM -> 3
  | PRODUCT -> 4
  | PREFIX -> 5
  | CALL -> 6
  | INDEX -> 7
  
let add_prefix_fn parser token fn = 
  Hashtbl.add parser.prefixParseFns token fn

let add_infix_fn parser token fn =
  Hashtbl.add parser.infixParseFns token fn

let init_parser lexer =
  let (al, current) = next_token lexer in
  let (al, next) = next_token al in
  {l = al ; current = current ; next = next; prefixParseFns = Hashtbl.create 9; infixParseFns = Hashtbl.create 9}

let rec build_parse_fns parser =
  add_prefix_fn parser "IDENT" parse_identifier;
  add_prefix_fn parser "INT" parse_integer;
  add_prefix_fn parser  "MINUS" parse_prefix_expression;
  add_prefix_fn parser "BANG" parse_prefix_expression;
  add_prefix_fn parser "TRUE" parse_bool;
  add_prefix_fn parser "FALSE" parse_bool;
  add_prefix_fn parser "LPAREN" parse_grouped_expression;
  add_prefix_fn parser "IF" parse_if_expression;
  add_prefix_fn parser "FUNCTION" parse_function_literal;
  add_prefix_fn parser "STRING" parse_string_literal;
  add_prefix_fn parser "LBRACK" parse_array_literal;
  add_infix_fn parser "PLUS" parse_infix_expression;
  add_infix_fn parser "MINUS" parse_infix_expression;
  add_infix_fn parser "SLASH" parse_infix_expression;
  add_infix_fn parser "ASTERISK" parse_infix_expression;
  add_infix_fn parser "EQUAL" parse_infix_expression;
  add_infix_fn parser "NOTEQUAL" parse_infix_expression;
  add_infix_fn parser "LT" parse_infix_expression;
  add_infix_fn parser "GT" parse_infix_expression;
  add_infix_fn parser "LPAREN" parse_call_expression;
  add_infix_fn parser "LBRACK" parse_index_expression;
  parser

and
parse_identifier parser =
  let parser, identifier = _build_ident parser in
  parser, Identifier identifier

and _build_ident parser =
 parser, { token = parser.current ; value = parse_ident parser } 

and
parse_ident parser = 
  match parser.current with
  | IDENT x -> x
  | _ -> raise (InvalidToken ("expected string, got: "^ _print_token parser.current))

and
parse_int parser = 
  match parser.current with
  | INT x -> x
  | _ -> raise (InvalidToken "expected int")

and
parse_string_literal parser =
  match parser.current with
  | STRING x -> parser, String {token = parser.current ; value = x}
  | _ -> raise (InvalidToken "expected string")

and
parse_integer parser =
  parser, Integer { token = parser.current ; value = parse_int parser }

and
_dump_tokens parser = 
  print_endline(_print_token parser.current);
  if parser.current == EOF 
  then ()
  else _dump_tokens (parse_next parser)

and parse_next parser =
  let (al, next_token) = next_token parser.l in
  {l = al ; current = parser.next ; next = next_token ; prefixParseFns = parser.prefixParseFns; infixParseFns = parser.infixParseFns}

and parse_prefix_expression parser =
  let ap = parse_next parser in 
  let p, right = p_e ap PREFIX in
  p, PrefixExpression 
      {token = parser.current ; operator = string_of_token parser.current ; right }

and parse_infix_expression parser left = 
  let order = token_to_order parser.current in
  let ap = parse_next parser in
  let ap, right = p_e ap order in
  ap, InfixExpression {token = parser.current ; operator = string_of_token parser.current ; left ; right }

and parse_index_expression parser left =
  let next = parse_next parser in
  let next, idx = p_e next LOWEST in
  let next, assertion = expect_peek next "RBRACK" in
  match assertion with
  | true -> next, IndexExpression {token = parser.current ; left = left ; index = idx}
  | false -> raise (InvalidToken "Expected RBRACK")

and p_e parser order =
  let prefixFnOption = findPrefixFn parser in 
  match prefixFnOption with
  | Some fn -> let parser, left = fn parser in
  parse_infix parser order left
  | None -> raise (InvalidToken ("expected prefixFn for: " ^ string_of_token parser.current ^" next: "^_print_token parser.next))
        
and parse_infix parser precedence left =
  let rec aux parser order left =
    match string_of_token parser.next with
    | "SEMICOLON" -> parser, left
    | "RPAREN" ->  parser, left
    | _ ->
    if order_to_enum order < order_to_enum (token_to_order parser.next)
    then
      let maybeInfixFnParser = parse_next parser in
      let infixFnOption = findInfixFn maybeInfixFnParser in
      match infixFnOption with
      | None ->  parser, left
      | Some fn ->
        let parser, right = fn maybeInfixFnParser left in
        aux parser order right
    else
      parser, left
  in aux parser precedence left

and parse_bool parser = match parser.current with
  | TRUE -> parser, Boolean {token = parser.current ; value = true}
  | FALSE -> parser, Boolean {token = parser.current ; value = false}
  | _ -> raise (InvalidToken "expected true or false")

and parse_grouped_expression parser =
  let parser = parse_next parser in (* eats open bracket *)
  let parser, e = p_e parser LOWEST in
  parse_next parser, e

and expect_peek parser str_token =
  if string_of_token parser.next == str_token
  then parse_next parser, true
  else parser, false

and current_token_is parser str_token =
  if string_of_token parser.current == str_token
  then true
  else false

and peek_token_is parser str_token =
  if string_of_token parser.next == str_token
  then true
  else false 

and parse_one parser = let parser, statement = match parser.current with
  | LET -> p_l_s parser
  | RETURN -> p_r_s parser
  | _ -> p_e_s parser
in parser, statement


and parse_block_statement parser = (* GIVE THIS THE { token (LBRACE) *)
  let parser = parse_next parser in 
  let parser, list = bhop parser [] in
  parser, list
and bhop parser acc =
  let tok = string_of_token parser.current in
  match tok with
  | "RBRACE" | "EOF" -> parser, List.rev acc
  | "SEMICOLON" ->
    let parser = parse_next parser in
    bhop parser acc
  | _ -> let parser, stmt = parse_one parser in
    bhop (parse_next parser) (stmt::acc)

and parse_program parser = 
  let rec aux parser acc =
    match parser.current with
    | EOF -> List.rev acc
    | _ -> 
        let parser, statement = parse_one parser in
        aux (parse_next parser) (statement::acc)
  in aux parser []

and p_e_s parser = 
  let parser, expression = p_e parser LOWEST in
  let e = ExpressionStatement {expression} in
  if peek_token_is parser "SEMICOLON"
  then parse_next parser, e
  else parser, e

and p_l_s let_parser = 
 let ident_parser, identAssertion =
  expect_peek let_parser "IDENT" in
  match identAssertion with
  | true -> let assign_parser, assignAssertion =
    expect_peek ident_parser "ASSIGN" in 
    (match assignAssertion with 
    | true -> let expression_parser = parse_next assign_parser in
      let parser, expression = p_e expression_parser LOWEST in
      (match string_of_token parser.next with
      | "SEMICOLON" -> 
      (parse_next parser),
      Let {token = let_parser.current 
      ; name = 
        { token = ident_parser.current 
        ; value = parse_ident ident_parser
        } 
      ; value = expression
      }

      | _ -> 
        parser,
        Let {token = let_parser.current 
        ; name = 
          { token = ident_parser.current 
          ; value = parse_ident ident_parser
          } 
        ; value = expression
        }) 
    | false -> raise (InvalidToken "expected assign")
    )
  | false -> raise (InvalidToken "expected ident")

and p_r_s return_parser =
  let expression_parser = parse_next return_parser in 
  let parser, expression = p_e expression_parser LOWEST in
  match string_of_token parser.next with
  | "SEMICOLON" -> (parse_next parser), Return {token = return_parser.current ; expression}
  | _ -> parser, Return {token = return_parser.current ; expression}

and parse_if_expression og_parser =
  let parser, assertion = expect_peek og_parser ("LPAREN") in
  match assertion with 
  | false -> raise (InvalidToken "expected lparen")
  | true ->
  let parser = parse_next parser in
  let parser, condition = p_e parser LOWEST in
  let parser = parse_next parser in (* GOTO RPAREN *)
  let parser = parse_next parser in (* GOTO LBRACE *)
  let consequence_parser, consequence = (* pass in as LBRACE *)
  parse_block_statement parser in
  let has_else = peek_token_is consequence_parser "ELSE" in
  match has_else with
  | true ->
    let parser =  parse_next consequence_parser in (* ELSE EATER *)
    let parser = parse_next parser in  (* { EATER  *)
    let parser, alternative = parse_block_statement parser in
    parser,
    If {
      token = og_parser.current;
      condition;
      consequence = { token = consequence_parser.current; statements = consequence };
      alternative = Some { token = parser.current; statements = alternative };
    }
  | false -> 
    consequence_parser,
    If {
      token = og_parser.current;
      condition;
      consequence = { token = consequence_parser.current; statements = consequence };
      alternative = None;
    }

and parse_function_params parser =
 let rec p_f_p parser acc = 
  let token = string_of_token parser.current in
  match token with
  | "RPAREN" -> parser, List.rev acc
  | "COMMA" -> p_f_p (parse_next parser) acc
  | _ -> let parser, id = _build_ident parser in p_f_p (parse_next parser) (id::acc)
 in p_f_p parser []

and parse_function_literal og_parser = 
  let parser, lparen = expect_peek og_parser "LPAREN" in
    match lparen with
    | false -> raise (InvalidToken "expected lparen")
    | true ->
    let parser = parse_next parser in
    let parser, params = parse_function_params parser in
    let parser, lbrace = expect_peek parser "LBRACE" in
    match lbrace with
    | false -> raise (InvalidToken "expected lbrace")
    | true ->
    let parser, block = parse_block_statement parser in
    parser, Function
      {token = og_parser.current ; parameters =  params ;
       body = 
        {token = parser.current ; statements = block}
     ;}

and walk parser acc =
  let assertComma = peek_token_is parser "COMMA" in
  match assertComma with
  | false -> parser, List.rev acc
  | true ->
    let parser = parse_next parser in
    let parser = parse_next parser in
    let parser, expression = p_e parser LOWEST in
    walk parser (expression::acc)

and parse_array_literal parser =
  if parser.next == RBRACK
  then parse_next parser, Array { token = parser.current ; elements = []}
  else let parser = parse_next parser in
  let parser, left = p_e parser LOWEST in
  let parser, stmts = walk parser (left::[]) in
  if current_token_is parser "SEMICOLON"
  then parse_next parser, Array { token = parser.current ; elements = stmts}
  else let rBrack =
    current_token_is parser "RBRACK" 
   || peek_token_is parser "RBRACK"
   || peek_token_is parser "EOF" 
   || peek_token_is parser "SEMICOLON" in
  match rBrack with
  | false ->
    raise (InvalidToken ("expected RBRACK, got: "^_print_token parser.next))
  | true -> let parser = parse_next parser in
    parser, Array { token = parser.current ; elements = stmts}

and parse_call_expression og_parser fn =
  if og_parser.next == RPAREN
  then parse_next og_parser, CallExpression {token = og_parser.current ; fn ; arguments = []}
  else
    let parser = parse_next og_parser in
    let parser, left = p_e parser LOWEST in
    let parser, stmts = walk parser (left::[]) in
    if current_token_is parser "SEMICOLON"
    then parser, CallExpression {token = og_parser.current ; fn ; arguments = stmts}
    else
    let rBrace =
      current_token_is parser "RPAREN" 
     || peek_token_is parser "RPAREN"
     || peek_token_is parser "EOF" 
     || peek_token_is parser "SEMICOLON" in
    match rBrace with
    | false ->
      let _ = print_endline("dying on: "^_print_token parser.current) in 
      raise (InvalidToken ("expected RPAREN, got: "^_print_token parser.next))
    | true -> let parser = parse_next parser in
      parser, CallExpression {token = og_parser.current ; fn ; arguments = stmts}

let rec _dump_tokens parser = 
  print_endline(_print_token parser.current);
  if parser.current == EOF 
  then ()
  else _dump_tokens (parse_next parser)
and
 parse_next parser =
  let (al, next_token) = next_token parser.l in
   {l = al ; current = parser.next ; next = next_token ; prefixParseFns = parser.prefixParseFns; infixParseFns = parser.infixParseFns}
;;

let rec skip_till_semicolon_next parser = 
  let _, assertion = 
  expect_peek parser (string_of_token SEMICOLON) in
  if assertion == false 
  then skip_till_semicolon_next (parse_next parser)
  else parser 

let rec pretty_print_node node =
  match node with
  | Program program -> pretty_print_program program
  | Statement statement -> pretty_print_statement statement
  | Expression expression -> pretty_print_expression expression
  | EOT -> ""

and pretty_print_statement statement =
  match statement with
  | Let { name = identifier; value = expression; _ } ->
    Printf.sprintf "let %s = %s" identifier.value (pretty_print_expression expression)
  | Return { expression; _ } ->
    Printf.sprintf "return %s" (pretty_print_expression expression)
  | ExpressionStatement { expression } ->
    pretty_print_expression expression

and pretty_print_expression expression =
  match expression with
  | String {token ; value } -> value
  | Identifier { value; _ } -> value
  | Integer { value; _ } -> string_of_int value
  | Boolean {value ; _} -> string_of_bool value
  | PrefixExpression { operator; right; _ } ->
    Printf.sprintf "(%s%s)" operator (pretty_print_expression right)
  | InfixExpression { left; operator; right; _ } ->
    let left_str = pretty_print_expression left in
    let right_str = pretty_print_expression right in
    let operator_str = match operator with
      | "PLUS" -> "+"
      | "MINUS" -> "-"
      | "ASTERISK" -> "*"
      | "SLASH" -> "/"
      | "EQUAL" -> "=="
      | "NOTEQUAL" -> "!="
      | "LT" -> "<"
      | "GT" -> ">"
      | _ -> operator
    in
    Printf.sprintf "(%s %s %s)" left_str operator_str right_str
  | If { condition; consequence; alternative ; _ } ->
    let condition_str = pretty_print_expression condition in
    let consequence_str = pretty_print_block_statement consequence in
    let alternative_str =
      match alternative with
      | Some alt -> "else " ^ pretty_print_block_statement alt
      | None -> ""
    in
    Printf.sprintf "if (%s) {%s}%s" condition_str consequence_str alternative_str
  | Function { parameters; body; _ } ->
    let parameters_str = List.map (fun (param : identifier) -> param.value) parameters in
    let parameters = String.concat ", " parameters_str in
    let body_str = pretty_print_block_statement body in
    Printf.sprintf "fn (%s) {%s}" parameters body_str
  | CallExpression { fn; arguments; _ } ->
    let fn_str = pretty_print_expression fn in
    let arguments_str = List.map pretty_print_expression arguments in
    let arguments = String.concat ", " arguments_str in
    Printf.sprintf "%s(%s)" fn_str arguments
  | Array {token ; elements} -> "Array temp print"
   |IndexExpression {token: token_type ; left : expression ; index : expression} -> "IndexExpression temp print"

and pretty_print_block_statement block_statement =
  let statements_str = List.map pretty_print_statement block_statement.statements in
  String.concat "\n" statements_str

and pretty_print_program program =
  let statements_str = List.map pretty_print_statement program in
  String.concat "\n" statements_str

type builtInFn = obj list -> obj

and obj =
  | Integer of int
  | Boolean of bool
  | String of string
  | Return of obj
  | BuiltIn of  builtInFn
  | Array of obj list
  | Function of {parameters : identifier list ; body : block_statement ; env : environment}
  | Error of string
  | Assign
  | Null
  | Empty
and environment = {env : (string, obj) Hashtbl.t ; outer : environment option}

let rec printObj obj =
  match obj with
  | Integer x -> print_endline(string_of_int x)
  | Boolean x -> print_endline(string_of_bool x)
  | String x -> print_endline("\"" ^ x ^ "\"")
  | Return x -> printObj x
  | Null -> print_endline "Null"
  | Empty -> print_endline "Empty"
  | Function _ -> print_endline ""
  | Assign -> print_endline ""
  | Error x -> print_endline x
  | BuiltIn x -> print_endline ""
  | Array x -> print_endline (printArrayObj x "[")

and
printArrayObj arr acc =
  match arr with
  | [] -> acc ^ "]"
  | h :: [] -> acc ^ stringOfObj h ^ "]"
  | h :: t -> printArrayObj t (acc ^ (stringOfObj h ^ ","))

and stringOfObjVariant obj =
  match obj with
  | Integer _ -> "Integer"
  | Boolean _ -> "Boolean"
  | String _ -> "String"
  | Return _ -> "Return"
  | Null -> "Null"
  | Empty ->  "Empty"
  | Error _ -> "Error" 
  | Assign -> "Assign"
  | Function _ -> "Function"
  | BuiltIn _ -> "BuiltIn"
  | Array _ -> "Array"

and stringOfObj obj =
    match obj with
    | Integer x -> string_of_int x
    | Boolean x -> string_of_bool x
    | String x -> x
    | Return x -> stringOfObj x
    | Null -> "Null"
    | Empty ->  "Empty"
    | Assign -> "Assign"
    | Error x -> x  
    | Function _ -> "Function"
    | BuiltIn _ -> "BuiltIn"
    | Array _ -> "Array"

let built_in_len t =
  match t with
  | [x] -> 
    (match x with 
    | String x -> Integer (String.length x)
    | Array x -> Integer (List.length x)
    | _ -> Error ("cannot perform len on: " ^ stringOfObjVariant x)
    )
  | _ -> Error ("invalid arg count: " ^ string_of_int (List.length t))

let build_built_ints  =
  let (builtIns : (string, obj) Hashtbl.t) = Hashtbl.create 1 in
  Hashtbl.add builtIns "len" (BuiltIn built_in_len);
  builtIns 

let rec envGet environment k builtIns =
   match Hashtbl.find_opt environment.env k with
  | Some x -> x
  | None -> match environment.outer with
    | Some x -> envGet x k builtIns
    | None -> (* Error ("unknown identifier") *)
    (match Hashtbl.find_opt builtIns k with
    | Some x -> x
    | None -> Error ("unknown identifier"))

let newClosedEnv outer = {env = Hashtbl.create 0 ; outer }
let evalError input = Error input

let isError obj = match obj with
  | Error _ -> true
  | _ -> false

let rec evalStatement stmt env builtIns = 
  match stmt with
  | Let {token ; name ; value} -> 
    let obj, env = evalExpression value env builtIns in
    if isError obj
    then obj, env
    else let _ = Hashtbl.add env.env name.value obj in Assign, env
  | Return {token ; expression} -> 
    let evaluatedExpression, env = evalExpression expression env builtIns in
    if isError evaluatedExpression then evaluatedExpression, env else
    Return evaluatedExpression, env
  | ExpressionStatement {expression} -> evalExpression expression env builtIns

and evalStatements stmts env builtIns =
  let rec aux stmts obj env builtIns =
    match stmts with
    | [] -> obj, env
    | h :: [] -> evalStatement h env builtIns
    | h :: t ->
      let o, env = evalStatement h env builtIns in
      match o with
      | Error _ | Return _ -> o, env
      | _ -> aux t o env builtIns
  in aux stmts Null env builtIns

and evalExpressions exps env builtIns = 
  let rec aux exps acc env =
    match exps with
    | [] -> List.rev acc, env
    | h :: [] -> let exp, env = evalExpression h env builtIns in List.rev (exp::acc), env
    | h :: t -> let exp, env = evalExpression h env builtIns in aux t (exp::acc) env
  in aux exps [] env

and checkEvalExpressionsErrors exps =
  let rec aux expr =
    match expr with
    | [] -> None, exps
    | h :: [] -> if isError h then Some(h), exps else None, exps
    | h :: t -> if isError h then Some(h), exps else aux t
  in aux exps

and evalExpression exp env builtIns =
  match exp with
  | Integer {token; value} -> Integer value, env
  | Boolean {token; value} -> Boolean value, env
  | String {token ; value} -> String value, env
  | PrefixExpression {token; operator; right} ->
      let r, env = evalExpression right env builtIns in
      if isError r then r, env else evalPrefixExp operator r, env
  | InfixExpression {token; operator; left; right} ->
      let r, env = evalExpression right env builtIns in
      if isError r then r, env else
      let l, env = evalExpression left env builtIns in
      if isError l then l, env else
      evalInfixExp operator l r, env
  | If {token; condition; consequence; alternative} ->
     evalIfExpression condition consequence alternative env builtIns
  | Identifier {token ; value} -> 
    envGet env value builtIns, env
  | Function {token ; parameters ; body} ->
     Function {parameters ; body ; env}, env
  | CallExpression {token ; fn ; arguments} ->
    let fn, env = evalExpression fn env builtIns in
    if isError fn
    then fn, env else
    let args, env = evalExpressions arguments env builtIns in
    let err, args = checkEvalExpressionsErrors args in
    (
      match err with
      | Some err -> err, env
      | None -> applyFunction fn args env builtIns
    )
  | Array {token ; elements} ->
    let elems, env = evalExpressions elements env builtIns in
    let err, args = checkEvalExpressionsErrors elems in
    (
      match err with
      | Some err -> err, env
      | None -> Array elems, env
    )
  | IndexExpression {token ; left ; index} ->
    let left, env = evalExpression left env builtIns in
    if isError left then left, env else
    let idx, env = evalExpression index env builtIns in
    if isError idx then idx, env else
    evalIndexExpression left idx env

and evalIndexExpression left index env =
  match left, index with
  | Array x, Integer y -> evalArrayIndexExpression x y, env
  | _ -> Error "Expected array, int", env

and evalArrayIndexExpression arr index =
  let length = List.length arr in
  if length - 1 < index then Error "index greater than array boundary"
  else List.nth arr index

and applyFunction fn args env builtIns =
  match fn with
  | Function { env ; body ; parameters} -> extendFunctionEnv fn args builtIns, env
  | BuiltIn fn -> fn args, env
  | _ -> Error "Expected function", env

and extendFunctionEnv fn args builtIns =
  match fn with
  | Function {parameters ; body ; env} -> 
    let extendedEnv = newClosedEnv (Some env) in
    let extendedEnv = setExtendedEnv extendedEnv parameters args in
    runFn fn extendedEnv builtIns
  | _ -> raise (InvalidToken "Expected function")

and setExtendedEnv env params args =
  let rec aux env (params : identifier list) args =
    match params with
    | [] -> env
    | h :: [] ->
      let arg = List.hd args in
      Hashtbl.add env.env h.value arg;
      env 
    | h :: t -> 
      let arg = List.hd args in
      Hashtbl.add env.env h.value arg;
      aux env t (List.tl args)
  in aux env params args

and runFn fn extendedEnv builtIns =
  match fn with
  | Function {parameters ; body ; env} ->
    let evaluated, _ = evalStatements body.statements extendedEnv builtIns in
    unwrapReturn evaluated
  | _ -> raise (InvalidToken "Expected function")

and unwrapReturn stmt = match stmt with
  | Return x -> x
  | _ -> stmt

and evalInfixExp operator left right =
  match (left, right) with
  | Integer x, Integer y -> evalIntegerInfixExp operator x y 
  | String x, String y -> evalStringInfixExp operator x y
  | Boolean _, Boolean _ -> (
    match operator with
    | "EQUAL" -> Boolean (left = right)
    | "NOTEQUAL" -> Boolean (left <> right)
    | _ -> evalError ("unknown operator: "^operator^ " for "^stringOfObjVariant left ^ " and " ^stringOfObjVariant right) (* unknown operator ig *)
  )
  | _ -> evalError ("type missmatch for "^stringOfObjVariant left^" "^operator ^" "^stringOfObjVariant right)

and evalStringInfixExp operator x y =
  match operator with
  | "PLUS" -> String (x ^ y)
  | "EQUAL" -> Boolean (x = y)
  | "NOTEQUAL" -> Boolean (x <> y)
  | _ -> evalError ("unknown operator: "^operator^ " for "^ x ^ " and " ^ y)

and evalIntegerInfixExp op x y =
  match op with
  | "MINUS" -> Integer (x - y)
  | "PLUS" -> Integer (x + y)
  | "ASTERISK" -> Integer (x * y)
  | "SLASH" -> Integer (x / y)
  | "LT" -> Boolean (x < y)
  | "GT" -> Boolean (x > y)
  | "EQUAL" -> Boolean (x = y)
  | "NOTEQUAL" -> Boolean (x <> y)
  | _ -> evalError ("unknown operator: " ^ op ^ "for Integer")

and evalPrefixExp operator right =
  match operator with
  | "BANG" -> evalBangOperatorExpression right
  | "MINUS" -> evalMinusOperatorExpression right
  | _ -> Null

and evalBangOperatorExpression right =
  match right with
  | Integer x -> (match x with | x when x <= 0 -> Boolean true | _ -> Boolean false)
  | Boolean x -> (match x with | true -> Boolean false | false -> Boolean true)
  | Null -> Boolean true
  | _ -> Boolean false

and evalMinusOperatorExpression right =
  match right with
  | Integer x -> Integer (-x)
  | _ -> evalError ("unknown operator - for: " ^ stringOfObj right)

and evalIfExpression condition consequence alternative env builtIns =
  let cond, env = evalExpression condition env builtIns in
  if isError cond then cond, env else
  if isTruthy cond
  then evalStatements consequence.statements env builtIns
  else match alternative with
  | Some x -> evalStatements x.statements env builtIns
  | None -> Null, env
  
and isTruthy obj =
  match obj with
  | Boolean x -> x
  | Integer x -> 
    if x <= 0 
    then false
    else true
  | Null -> false
  | _ -> raise (InvalidToken "truthy checked empty??")

(* let test =
    let lexer = init_lexer input in
    let parser = init_parser lexer in
    let parser = build_parse_fns parser in
    let statements = parse_program parser in
    let program = Program statements in
    eval program *)


let start = 
  print_endline "  ▄▄▄▄    ██░ ██  ▒█████   ██▓███  
  ▓█████▄ ▓██░ ██▒▒██▒  ██▒▓██░  ██▒
  ▒██▒ ▄██▒██▀▀██░▒██░  ██▒▓██░ ██▓▒
  ▒██░█▀  ░▓█ ░██ ▒██   ██░▒██▄█▓▒ ▒
  ░▓█  ▀█▓░▓█▒░██▓░ ████▓▒░▒██▒ ░  ░
  ░▒▓███▀▒ ▒ ░░▒░▒░ ▒░▒░▒░ ▒▓▒░ ░  ░
  ▒░▒   ░  ▒ ░▒░ ░  ░ ▒ ▒░ ░▒ ░     
   ░    ░  ░  ░░ ░░ ░ ░ ▒  ░░       
   ░       ░  ░  ░    ░ ░           
        ░                                 
      ";
  flush stdout;
  let env = { env = Hashtbl.create 0 ; outer = None } in
  let rec process_input env =
  print_string "🐒 >> ";
  flush stdout;
  let input = read_line () in
  if input = "exit" then
      ()
  else
    let lexer = init_lexer input in
    let parser = init_parser lexer in
    (* _dump_tokens parser; *)
    let parser = build_parse_fns parser in
    let statements = parse_program parser in
    let builtIns = build_built_ints in
    let obj, env = evalStatements statements env builtIns in
    printObj obj;
(*     let str = pretty_print_statement statement in
    print_endline str; *)
(*     let program_str = pretty_print_program (List.rev statements) in
    print_endline program_str; *)
    process_input env
  in process_input env