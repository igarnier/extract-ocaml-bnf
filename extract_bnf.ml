open MenhirSdk
open Format

module type C = Cmly_api.GRAMMAR

let grammar =
  match Array.to_list Sys.argv |> List.tl with
  | [cmly] -> In_channel.with_open_bin cmly Cmly_read.read_channel
  | _ ->
      Printf.eprintf "usage: extract_bnf.exe grammar.cmly\n" ;
      exit 1

module G = Cmly_read.Lift (struct
  let grammar = grammar
end)

let tokens =
  [ ("AMPERAMPER", "&&");
    ("AMPERSAND", "&");
    ("AND", "and");
    ("AS", "as");
    ("ASSERT", "assert");
    ("BACKQUOTE", "`");
    ("BANG", "!");
    ("BAR", "|");
    ("BARBAR", "||");
    ("BARRBRACKET", "|]");
    ("BEGIN", "begin");
    (* "<char> CHAR",            "'a'" (\* just an example *\); *)
    ("CLASS", "class");
    ("COLON", ":");
    ("COLONCOLON", "::");
    ("COLONEQUAL", ":=");
    ("COLONGREATER", ":>");
    ("COMMA", ",");
    ("CONSTRAINT", "constraint");
    ("DO", "do");
    ("DONE", "done");
    ("DOT", ".");
    ("DOTDOT", "..");
    ("DOWNTO", "downto");
    ("ELSE", "else");
    ("END", "end");
    ("EOF", "");
    ("EQUAL", "=");
    ("EXCEPTION", "exception");
    ("EXTERNAL", "external");
    ("FALSE", "false");
    (* "<string * char option> FLOAT", "42.0" (\* just an example *\); *)
    ("FOR", "for");
    ("FUN", "fun");
    ("FUNCTION", "function");
    ("FUNCTOR", "functor");
    ("GREATER", ">");
    ("GREATERRBRACE", ">}");
    ("GREATERRBRACKET", ">]");
    ("IF", "if");
    ("IN", "in");
    ("INCLUDE", "include");
    (* <string> INFIXOP0      "!="   (\* just an example *\); *)
    (* <string> INFIXOP1      "@"    (\* just an example *\); *)
    (* <string> INFIXOP2      "+!"   (\* chosen with care; see above *\); *)
    (* <string> INFIXOP3      "land" (\* just an example *\); *)
    (* <string> INFIXOP4      "**"   (\* just an example *\); *)
    (* <string> DOTOP,         ".+"; *)
    (* <string> LETOP,         "let*" (\* just an example *\); *)
    (* <string> ANDOP,         "and*" (\* just an example *\); *)
    ("INHERIT", "inherit");
    ("INITIALIZER", "initializer");
    (* "<string * char option> INT", "42"  (\* just an example *\); *)
    (* "<string> LABEL",         "~label:" (\* just an example *\); *)
    ("LAZY", "lazy");
    ("LBRACE", "{");
    ("LBRACELESS", "{<");
    ("LBRACKET", "[");
    ("LBRACKETBAR", "[|");
    ("LBRACKETLESS", "[<");
    ("LBRACKETGREATER", "[>");
    ("LBRACKETPERCENT", "[%");
    ("LBRACKETPERCENTPERCENT", "[%%");
    ("LESS", "<");
    ("LESSMINUS", "<-");
    ("LET", "let");
    (* "<string> LIDENT",        "lident" (\* just an example *\); *)
    ("LPAREN", "(");
    ("LBRACKETAT", "[@");
    ("LBRACKETATAT", "[@@");
    ("LBRACKETATATAT", "[@@@");
    ("MATCH", "match");
    ("METHOD", "method");
    ("MINUS", "-");
    ("MINUSDOT", "-.");
    ("MINUSGREATER", "->");
    ("MODULE", "module");
    ("MUTABLE", "mutable");
    ("NEW", "new");
    ("NONREC", "nonrec");
    ("OBJECT", "object");
    ("OF", "of");
    ("OPEN", "open");
    (* "<string> OPTLABEL",      "?label:" (\* just an example *\); *)
    ("OR", "or");
    ("PERCENT", "%");
    ("PLUS", "+");
    ("PLUSDOT", "+.");
    ("PLUSEQ", "+=");
    (* "<string> PREFIXOP",      "!+" (\* chosen with care; see above *\); *)
    ("PRIVATE", "private");
    ("QUESTION", "?");
    ("QUOTE", "'");
    ("RBRACE", "}");
    ("RBRACKET", "]");
    ("REC", "rec");
    ("RPAREN", ")");
    ("SEMI", ";");
    ("SEMISEMI", ";;");
    ("HASH", "#");
    (* "<string> HASHOP",        "##" (\* just an example *\); *)
    ("SIG", "sig");
    ("STAR", "*");
    (* "<string * Location.t * string option>; STRING",                 "\"hello\"" (\* just an example *\); *)
    (* "<string * Location.t * string * Location.t * string option>; QUOTED_STRING_EXPR",     "{%hello|world|}"  (\* just an example *\); *)
    (* "<string * Location.t * string * Location.t * string option>; QUOTED_STRING_ITEM",     "{%%hello|world|}" (\* just an example *\); *)
    ("STRUCT", "struct");
    ("THEN", "then");
    ("TILDE", "~");
    ("TO", "to");
    ("TRUE", "true");
    ("TRY", "try");
    ("TYPE", "type");
    (* "<string> UIDENT",        "UIdent" (\* just an example *\); *)
    ("UNDERSCORE", "_");
    ("VAL", "val");
    ("VIRTUAL", "virtual");
    ("WHEN", "when");
    ("WHILE", "while");
    ("WITH", "with") ]
  |> List.to_seq |> Hashtbl.of_seq

let quote s = "\"" ^ s ^ "\""

let quote_terminal term =
  match Hashtbl.find_opt tokens term with None -> term | Some res -> quote res

let normalize = false

(* [normalize_nonterminal] tries to rename nonterminals to remove parenthesis, etc - useful
   for further conversion to LLama's GBNF format *)
let normalize_nonterminal term =
  let bytes = Bytes.of_string term in
  let len = Bytes.length bytes in
  Bytes.to_seq bytes
  |> Seq.mapi (fun i c ->
         match c with
         | '(' -> Some '-'
         | '_' -> Some '-'
         | ',' -> Some '-'
         | ')' ->
             if i = len - 1 then None
             else if Bytes.get bytes (i + 1) = ')' then None
             else Some '-'
         | c -> Some c)
  |> Seq.filter_map Fun.id |> String.of_seq

open G

let prods = Hashtbl.create 101

let () =
  Production.iter (fun prod ->
      let nonterminal = Production.lhs prod in
      let rhs = Production.rhs prod in
      let p = Format.asprintf "%a" Print.nonterminal nonterminal in
      let p = normalize_nonterminal p in
      (match Hashtbl.find_opt prods p with
      | None ->
          Hashtbl.add prods p () ;
          printf "%s ::= " p
      | Some _ ->
          let spaces = String.make (String.length p + 2) ' ' in
          printf "%s | " spaces) ;
      if Array.length rhs = 0 then printf "%s" (quote "")
      else
        Array.iter
          (fun (sym, _id, _attrs) ->
            match sym with
            | T terminal ->
                let t = asprintf "%a" Print.terminal terminal in
                let t = quote_terminal t in
                printf "%s " t
            | N nonterminal ->
                let nt = asprintf "%a" Print.nonterminal nonterminal in
                let nt = normalize_nonterminal nt in
                printf "%s " nt)
          rhs ;
      printf "@.")

(*
   TODO:
   - invert lexer
   - normalize name of nonterminals
   - set root to be toplevel phrase
   - remove redundant rules
   - get rid of weird sublanguage?
*)
