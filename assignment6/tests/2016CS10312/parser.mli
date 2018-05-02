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

val database :
  (Lexing.lexbuf  -> token) -> Lexing.lexbuf -> Interpreter.program
val query :
  (Lexing.lexbuf  -> token) -> Lexing.lexbuf -> Interpreter.goal
