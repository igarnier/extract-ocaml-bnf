open MenhirSdk
open Format

module type C = Cmly_api.GRAMMAR

let grammar =
  In_channel.with_open_bin Sys.argv.(1) Cmly_read.read_channel

module G = Cmly_read.Lift (struct let grammar = grammar end)

let tokens =
[
 "AMPERAMPER",             "&&";
 "AMPERSAND",              "&";
 "AND",                    "and";
 "AS",                     "as";
 "ASSERT",                 "assert";
 "BACKQUOTE",              "`";
 "BANG",                   "!";
 "BAR",                    "|";
 "BARBAR",                 "||";
 "BARRBRACKET",            "|]";
 "BEGIN",                  "begin";
 (* "<char> CHAR",            "'a'" (\* just an example *\); *)
 "CLASS",                  "class";
 "COLON",                  ":";
 "COLONCOLON",             "::";
 "COLONEQUAL",             ":=";
 "COLONGREATER",           ":>";
 "COMMA",                  ",";
 "CONSTRAINT",             "constraint";
 "DO",                     "do";
 "DONE",                   "done";
 "DOT",                    ".";
 "DOTDOT",                 "..";
 "DOWNTO",                 "downto";
 "ELSE",                   "else";
 "END",                    "end";
 "EOF",                    "";
 "EQUAL",                  "=";
 "EXCEPTION",              "exception";
 "EXTERNAL",               "external";
 "FALSE",                  "false";
 (* <string * char option> FLOAT", "42.0" (\* just an example *\); *)
 "FOR",                    "for";
 "FUN",                    "fun";
 "FUNCTION",               "function";
 "FUNCTOR",                "functor";
 "GREATER",                ">";
 "GREATERRBRACE",          ">}";
 "GREATERRBRACKET",        ">]";
 "IF",                     "if";
 "IN",                     "in";
 "INCLUDE",                "include";
 (* <string> INFIXOP0      "!="   (\* just an example *\); *)
 (* <string> INFIXOP1      "@"    (\* just an example *\); *)
 (* <string> INFIXOP2      "+!"   (\* chosen with care; see above *\); *)
 (* <string> INFIXOP3      "land" (\* just an example *\); *)
 (* <string> INFIXOP4      "**"   (\* just an example *\); *)
 (* <string> DOTOP",         ".+"; *)
 (* <string> LETOP",         "let*" (\* just an example *\); *)
 (* <string> ANDOP",         "and*" (\* just an example *\); *)
 "INHERIT",                "inherit";
 "INITIALIZER",            "initializer";
 (* "<string * char option> INT", "42"  (\* just an example *\); *)
 (* "<string> LABEL",         "~label:" (\* just an example *\); *)
 "LAZY",                   "lazy";
 "LBRACE",                 "{";
 "LBRACELESS",             "{<";
 "LBRACKET",               "[";
 "LBRACKETBAR",            "[|";
 "LBRACKETLESS",           "[<";
 "LBRACKETGREATER",        "[>";
 "LBRACKETPERCENT",        "[%";
 "LBRACKETPERCENTPERCENT", "[%%";
 "LESS",                   "<";
 "LESSMINUS",              "<-";
 "LET",                    "let";
 (* "<string> LIDENT",        "lident" (\* just an example *\); *)
 "LPAREN",                 "(";
 "LBRACKETAT",             "[@";
 "LBRACKETATAT",           "[@@";
 "LBRACKETATATAT",         "[@@@";
 "MATCH",                  "match";
 "METHOD",                 "method";
 "MINUS",                  "-";
 "MINUSDOT",               "-.";
 "MINUSGREATER",           "->";
 "MODULE",                 "module";
 "MUTABLE",                "mutable";
 "NEW",                    "new";
 "NONREC",                 "nonrec";
 "OBJECT",                 "object";
 "OF",                     "of";
 "OPEN",                   "open";
 (* "<string> OPTLABEL",      "?label:" (\* just an example *\); *)
 "OR",                     "or";
 "PERCENT",                "%";
 "PLUS",                   "+";
 "PLUSDOT",                "+.";
 "PLUSEQ",                 "+=";
 (* "<string> PREFIXOP",      "!+" (\* chosen with care; see above *\); *)
 "PRIVATE",                "private";
 "QUESTION",               "?";
 "QUOTE",                  "'";
 "RBRACE",                 "}";
 "RBRACKET",               "]";
 "REC",                    "rec";
 "RPAREN",                 ")";
 "SEMI",                   ";";
 "SEMISEMI",               ";;";
 "HASH",                   "#";
 (* "<string> HASHOP",        "##" (\* just an example *\); *)
 "SIG",                    "sig";
 "STAR",                   "*";
 (* "<string * Location.t * string option>; STRING",                 "\"hello\"" (\* just an example *\); *)
 (* "<string * Location.t * string * Location.t * string option>; QUOTED_STRING_EXPR",     "{%hello|world|}"  (\* just an example *\); *)
 (* <string * Location.t * string * Location.t * string option>; QUOTED_STRING_ITEM",     "{%%hello|world|}" (\* just an example *\); *)
 "STRUCT",                 "struct";
 "THEN",                   "then";
 "TILDE",                  "~";
 "TO",                     "to";
 "TRUE",                   "true";
 "TRY",                    "try";
 "TYPE",                   "type";
 (* "<string> UIDENT",        "UIdent" (\* just an example *\); *)
 "UNDERSCORE",             "_";
 "VAL",                    "val";
 "VIRTUAL",                "virtual";
 "WHEN",                   "when";
 "WHILE",                  "while";
 "WITH",                   "with";
]

open G

let prods = Hashtbl.create 101

let () =
  Production.iter (fun prod ->
      let nonterminal = Production.lhs prod in
      let rhs = Production.rhs prod in
      let p = Format.asprintf "%a" Print.nonterminal nonterminal in
      (match Hashtbl.find_opt prods p with
      | None ->
        (Hashtbl.add prods p () ;
         printf "%s ::= " p)
      | Some _ ->
        let spaces = String.make (String.length p + 2) ' ' in
        printf "%s | " spaces) ;
      Array.iter (fun (sym, _id, _attrs) ->
          printf "%a " Print.symbol sym
        ) rhs ;
      printf "@."
    )

(*
   TODO:
   - invert lexer
   - normalize name of nonterminals
   - set root to be toplevel phrase
   - remove redundant rules
   - get rid of weird sublanguage?
*)

(* let rec pp_lr1 fmtr lr1 = *)
(*   let transitions = Lr1.transitions lr1 in *)
(*   List.iter (fun (symbol, lr1) -> *)
(*       fprintf fmtr "sym=%a, l1=%a@." Print.symbol symbol pp_lr1 lr1 *)
(*     ) transitions *)

(* let () = *)
(*   List.iter (fun (nonterminal, production, _lr1) -> *)
(*       printf "%a, %a@." Print.nonterminal nonterminal Print.production production *)
(*     ) Grammar.entry_points *)
