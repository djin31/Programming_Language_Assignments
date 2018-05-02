type token =
  | TRUE
  | FALSE
  | VAR of (string)
  | CONST of (string)
  | NUMBER of (int)
  | FAIL
  | SEP
  | OPEN_PAREN
  | CLOS_PAREN
  | OPEN_SQ
  | CLOS_SQ
  | LIST_DIFF
  | CUT
  | EOF
  | EQUALS
  | GREATER
  | LESS
  | ADD
  | SUB
  | MUL
  | DIV
  | EXP
  | COMMA
  | SEM_COL
  | END

open Parsing;;
let _ = parse_error;;
# 1 "parser.mly"

  open Interpreter
# 34 "parser.ml"
let yytransl_const = [|
  257 (* TRUE *);
  258 (* FALSE *);
  262 (* FAIL *);
  263 (* SEP *);
  264 (* OPEN_PAREN *);
  265 (* CLOS_PAREN *);
  266 (* OPEN_SQ *);
  267 (* CLOS_SQ *);
  268 (* LIST_DIFF *);
  269 (* CUT *);
    0 (* EOF *);
  270 (* EQUALS *);
  271 (* GREATER *);
  272 (* LESS *);
  273 (* ADD *);
  274 (* SUB *);
  275 (* MUL *);
  276 (* DIV *);
  277 (* EXP *);
  278 (* COMMA *);
  279 (* SEM_COL *);
  280 (* END *);
    0|]

let yytransl_block = [|
  259 (* VAR *);
  260 (* CONST *);
  261 (* NUMBER *);
    0|]

let yylhs = "\255\255\
\001\000\001\000\002\000\003\000\003\000\004\000\004\000\004\000\
\005\000\005\000\007\000\007\000\007\000\007\000\007\000\007\000\
\007\000\007\000\007\000\007\000\007\000\007\000\007\000\007\000\
\007\000\006\000\006\000\008\000\008\000\008\000\000\000\000\000"

let yylen = "\002\000\
\001\000\002\000\002\000\002\000\004\000\004\000\001\000\001\000\
\001\000\003\000\001\000\001\000\001\000\001\000\001\000\003\000\
\003\000\003\000\003\000\003\000\003\000\003\000\003\000\003\000\
\003\000\001\000\003\000\001\000\003\000\003\000\002\000\002\000"

let yydefred = "\000\000\
\000\000\000\000\000\000\000\000\008\000\007\000\001\000\031\000\
\000\000\000\000\032\000\000\000\000\000\002\000\000\000\004\000\
\003\000\014\000\015\000\011\000\012\000\013\000\000\000\000\000\
\000\000\000\000\000\000\000\000\000\000\000\000\000\000\006\000\
\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\000\000\000\000\005\000\024\000\000\000\000\000\025\000\000\000\
\000\000\000\000\000\000\000\000\000\000\000\000\023\000\027\000\
\010\000\030\000\029\000"

let yydgoto = "\003\000\
\008\000\011\000\009\000\010\000\028\000\025\000\026\000\031\000"

let yysindex = "\006\000\
\001\000\117\255\000\000\250\254\000\000\000\000\000\000\000\000\
\001\000\255\254\000\000\235\254\114\255\000\000\117\255\000\000\
\000\000\000\000\000\000\000\000\000\000\000\000\114\255\114\255\
\001\255\118\255\002\255\006\255\082\255\092\255\014\255\000\000\
\114\255\114\255\114\255\114\255\114\255\114\255\114\255\114\255\
\114\255\117\255\000\000\000\000\035\255\114\255\000\000\108\255\
\108\255\108\255\066\255\066\255\021\255\021\255\000\000\000\000\
\000\000\000\000\000\000"

let yyrindex = "\000\000\
\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\000\000\036\255\033\255\000\000\000\000\052\255\000\000\000\000\
\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\255\
\068\255\072\255\044\255\056\255\017\255\032\255\000\000\000\000\
\000\000\000\000\000\000"

let yygindex = "\000\000\
\055\000\000\000\000\000\254\255\027\000\034\000\237\255\030\000"

let yytablesize = 270
let yytable = "\012\000\
\007\000\013\000\017\000\029\000\030\000\015\000\001\000\002\000\
\016\000\032\000\016\000\016\000\027\000\048\000\049\000\050\000\
\051\000\052\000\053\000\054\000\055\000\016\000\016\000\042\000\
\047\000\021\000\030\000\021\000\021\000\043\000\021\000\021\000\
\021\000\021\000\021\000\021\000\021\000\058\000\021\000\027\000\
\022\000\040\000\022\000\022\000\026\000\022\000\022\000\022\000\
\022\000\022\000\022\000\022\000\019\000\022\000\019\000\019\000\
\009\000\019\000\019\000\019\000\019\000\019\000\028\000\014\000\
\020\000\019\000\020\000\020\000\057\000\020\000\020\000\020\000\
\020\000\020\000\056\000\059\000\017\000\020\000\017\000\017\000\
\018\000\000\000\018\000\018\000\038\000\039\000\040\000\000\000\
\000\000\017\000\044\000\000\000\000\000\018\000\000\000\033\000\
\034\000\035\000\036\000\037\000\038\000\039\000\040\000\045\000\
\000\000\033\000\034\000\035\000\036\000\037\000\038\000\039\000\
\040\000\046\000\018\000\019\000\020\000\021\000\022\000\000\000\
\004\000\023\000\005\000\024\000\036\000\037\000\038\000\039\000\
\040\000\006\000\000\000\033\000\034\000\035\000\036\000\037\000\
\038\000\039\000\040\000\041\000\000\000\000\000\000\000\000\000\
\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\000\000\000\000\000\000\000\000\004\000\000\000\005\000\000\000\
\000\000\000\000\000\000\000\000\000\000\006\000"

let yycheck = "\002\000\
\000\000\008\001\024\001\023\000\024\000\007\001\001\000\002\000\
\009\001\009\001\011\001\012\001\015\000\033\000\034\000\035\000\
\036\000\037\000\038\000\039\000\040\000\022\001\024\001\022\001\
\011\001\009\001\046\000\011\001\012\001\024\001\014\001\015\001\
\016\001\017\001\018\001\019\001\020\001\003\001\022\001\042\000\
\009\001\021\001\011\001\012\001\009\001\014\001\015\001\016\001\
\017\001\018\001\019\001\020\001\009\001\022\001\011\001\012\001\
\024\001\014\001\015\001\016\001\017\001\018\001\011\001\009\000\
\009\001\022\001\011\001\012\001\042\000\014\001\015\001\016\001\
\017\001\018\001\041\000\046\000\009\001\022\001\011\001\012\001\
\009\001\255\255\011\001\012\001\019\001\020\001\021\001\255\255\
\255\255\022\001\009\001\255\255\255\255\022\001\255\255\014\001\
\015\001\016\001\017\001\018\001\019\001\020\001\021\001\012\001\
\255\255\014\001\015\001\016\001\017\001\018\001\019\001\020\001\
\021\001\022\001\001\001\002\001\003\001\004\001\005\001\255\255\
\004\001\008\001\006\001\010\001\017\001\018\001\019\001\020\001\
\021\001\013\001\255\255\014\001\015\001\016\001\017\001\018\001\
\019\001\020\001\021\001\022\001\255\255\255\255\255\255\255\255\
\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\
\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\
\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\
\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\
\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\
\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\
\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\
\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\
\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\
\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\
\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\
\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\
\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\
\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\
\255\255\255\255\255\255\255\255\004\001\255\255\006\001\255\255\
\255\255\255\255\255\255\255\255\255\255\013\001"

let yynames_const = "\
  TRUE\000\
  FALSE\000\
  FAIL\000\
  SEP\000\
  OPEN_PAREN\000\
  CLOS_PAREN\000\
  OPEN_SQ\000\
  CLOS_SQ\000\
  LIST_DIFF\000\
  CUT\000\
  EOF\000\
  EQUALS\000\
  GREATER\000\
  LESS\000\
  ADD\000\
  SUB\000\
  MUL\000\
  DIV\000\
  EXP\000\
  COMMA\000\
  SEM_COL\000\
  END\000\
  "

let yynames_block = "\
  VAR\000\
  CONST\000\
  NUMBER\000\
  "

let yyact = [|
  (fun _ -> failwith "parser")
; (fun __caml_parser_env ->
    Obj.repr(
# 37 "parser.mly"
            ( [] )
# 224 "parser.ml"
               : Interpreter.program))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 1 : 'p_clause) in
    let _2 = (Parsing.peek_val __caml_parser_env 0 : Interpreter.program) in
    Obj.repr(
# 38 "parser.mly"
                       ( (_1)::(_2) )
# 232 "parser.ml"
               : Interpreter.program))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 1 : 'p_atom) in
    Obj.repr(
# 42 "parser.mly"
                 ( _1 )
# 239 "parser.ml"
               : Interpreter.goal))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 1 : 'p_atom) in
    Obj.repr(
# 46 "parser.mly"
                 ( Fact(_1) )
# 246 "parser.ml"
               : 'p_clause))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 3 : 'p_atom) in
    let _3 = (Parsing.peek_val __caml_parser_env 1 : 'p_atomlist) in
    Obj.repr(
# 47 "parser.mly"
                            ( Rule(_1, _3) )
# 254 "parser.ml"
               : 'p_clause))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 3 : string) in
    let _3 = (Parsing.peek_val __caml_parser_env 1 : 'p_termlist) in
    Obj.repr(
# 51 "parser.mly"
                                         ( Pred (_1,_3) )
# 262 "parser.ml"
               : 'p_atom))
; (fun __caml_parser_env ->
    Obj.repr(
# 52 "parser.mly"
              ( Cut )
# 268 "parser.ml"
               : 'p_atom))
; (fun __caml_parser_env ->
    Obj.repr(
# 53 "parser.mly"
               ( Fail )
# 274 "parser.ml"
               : 'p_atom))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 0 : 'p_atom) in
    Obj.repr(
# 57 "parser.mly"
                 ( [_1] )
# 281 "parser.ml"
               : 'p_atomlist))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 2 : 'p_atom) in
    let _3 = (Parsing.peek_val __caml_parser_env 0 : 'p_atomlist) in
    Obj.repr(
# 58 "parser.mly"
                              ( (_1)::(_3) )
# 289 "parser.ml"
               : 'p_atomlist))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 0 : string) in
    Obj.repr(
# 62 "parser.mly"
          ( Var(_1) )
# 296 "parser.ml"
               : 'p_term))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 0 : string) in
    Obj.repr(
# 63 "parser.mly"
            ( Const(_1) )
# 303 "parser.ml"
               : 'p_term))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 0 : int) in
    Obj.repr(
# 64 "parser.mly"
             ( Nat(_1) )
# 310 "parser.ml"
               : 'p_term))
; (fun __caml_parser_env ->
    Obj.repr(
# 65 "parser.mly"
           ( T )
# 316 "parser.ml"
               : 'p_term))
; (fun __caml_parser_env ->
    Obj.repr(
# 66 "parser.mly"
            ( F )
# 322 "parser.ml"
               : 'p_term))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 2 : 'p_term) in
    let _3 = (Parsing.peek_val __caml_parser_env 0 : 'p_term) in
    Obj.repr(
# 67 "parser.mly"
                        ( Node ("Equals",[_1;_3]) )
# 330 "parser.ml"
               : 'p_term))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 2 : 'p_term) in
    let _3 = (Parsing.peek_val __caml_parser_env 0 : 'p_term) in
    Obj.repr(
# 68 "parser.mly"
                         ( Node ("Greater",[_1;_3]) )
# 338 "parser.ml"
               : 'p_term))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 2 : 'p_term) in
    let _3 = (Parsing.peek_val __caml_parser_env 0 : 'p_term) in
    Obj.repr(
# 69 "parser.mly"
                      ( Node ("Less",[_1;_3]) )
# 346 "parser.ml"
               : 'p_term))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 2 : 'p_term) in
    let _3 = (Parsing.peek_val __caml_parser_env 0 : 'p_term) in
    Obj.repr(
# 70 "parser.mly"
                     ( Node ("Add",[_1;_3]) )
# 354 "parser.ml"
               : 'p_term))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 2 : 'p_term) in
    let _3 = (Parsing.peek_val __caml_parser_env 0 : 'p_term) in
    Obj.repr(
# 71 "parser.mly"
                     ( Node ("Sub",[_1;_3]) )
# 362 "parser.ml"
               : 'p_term))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 2 : 'p_term) in
    let _3 = (Parsing.peek_val __caml_parser_env 0 : 'p_term) in
    Obj.repr(
# 72 "parser.mly"
                     ( Node ("Mul",[_1;_3]) )
# 370 "parser.ml"
               : 'p_term))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 2 : 'p_term) in
    let _3 = (Parsing.peek_val __caml_parser_env 0 : 'p_term) in
    Obj.repr(
# 73 "parser.mly"
                     ( Node ("Div",[_1;_3]) )
# 378 "parser.ml"
               : 'p_term))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 2 : 'p_term) in
    let _3 = (Parsing.peek_val __caml_parser_env 0 : 'p_term) in
    Obj.repr(
# 74 "parser.mly"
                     ( Node ("Exp",[_1;_3]) )
# 386 "parser.ml"
               : 'p_term))
; (fun __caml_parser_env ->
    let _2 = (Parsing.peek_val __caml_parser_env 1 : 'p_term) in
    Obj.repr(
# 75 "parser.mly"
                               ( _2 )
# 393 "parser.ml"
               : 'p_term))
; (fun __caml_parser_env ->
    let _2 = (Parsing.peek_val __caml_parser_env 1 : 'p_list) in
    Obj.repr(
# 76 "parser.mly"
                         ( _2 )
# 400 "parser.ml"
               : 'p_term))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 0 : 'p_term) in
    Obj.repr(
# 80 "parser.mly"
          ( [_1] )
# 407 "parser.ml"
               : 'p_termlist))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 2 : 'p_term) in
    let _3 = (Parsing.peek_val __caml_parser_env 0 : 'p_termlist) in
    Obj.repr(
# 81 "parser.mly"
                          ( (_1)::(_3) )
# 415 "parser.ml"
               : 'p_termlist))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 0 : 'p_term) in
    Obj.repr(
# 85 "parser.mly"
             ( Node ("Cons",[_1]) )
# 422 "parser.ml"
               : 'p_list))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 2 : 'p_term) in
    let _3 = (Parsing.peek_val __caml_parser_env 0 : 'p_list) in
    Obj.repr(
# 86 "parser.mly"
                      ( Node ("Cons",[(_1);(_3)]) )
# 430 "parser.ml"
               : 'p_list))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 2 : 'p_term) in
    let _3 = (Parsing.peek_val __caml_parser_env 0 : string) in
    Obj.repr(
# 87 "parser.mly"
                       ( Node ("Cons",[(_1);Var(_3)]) )
# 438 "parser.ml"
               : 'p_list))
(* Entry database *)
; (fun __caml_parser_env -> raise (Parsing.YYexit (Parsing.peek_val __caml_parser_env 0)))
(* Entry query *)
; (fun __caml_parser_env -> raise (Parsing.YYexit (Parsing.peek_val __caml_parser_env 0)))
|]
let yytables =
  { Parsing.actions=yyact;
    Parsing.transl_const=yytransl_const;
    Parsing.transl_block=yytransl_block;
    Parsing.lhs=yylhs;
    Parsing.len=yylen;
    Parsing.defred=yydefred;
    Parsing.dgoto=yydgoto;
    Parsing.sindex=yysindex;
    Parsing.rindex=yyrindex;
    Parsing.gindex=yygindex;
    Parsing.tablesize=yytablesize;
    Parsing.table=yytable;
    Parsing.check=yycheck;
    Parsing.error_function=parse_error;
    Parsing.names_const=yynames_const;
    Parsing.names_block=yynames_block }
let database (lexfun : Lexing.lexbuf -> token) (lexbuf : Lexing.lexbuf) =
   (Parsing.yyparse yytables 1 lexfun lexbuf : Interpreter.program)
let query (lexfun : Lexing.lexbuf -> token) (lexbuf : Lexing.lexbuf) =
   (Parsing.yyparse yytables 2 lexfun lexbuf : Interpreter.goal)
