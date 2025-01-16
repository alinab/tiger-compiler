Tiger Language Parser Construction Write-up
-------------------------------------------

To generate a parser for the Tiger compiler, [ML-Yacc][1][2], a parser generator has been used.

### ML-Yacc Declarations

The ML-Yacc declarations for the Tiger parser is divided into two parts:
- those required by ML-Yacc
- optional ones e.g. for error-correction and for intermediate parser output.

Tiger.grm contains values for the following required declarations:
* `%name`: specifies the name of the parser. The name provided here should agree with the name provided in the `%header` specification in the corresponding ML-Lex file. In the .lex file, the `%header%` should be given the value -

where:
  * ParserName is the name of the parser
  * TokenStructure is the name of the structure specifying functions for tokens used by the parser
  * TokenSignature is the name of the signature specifying types for parser tokens

* `%term`: specifies the set of all terminals. For terminals that carry a value with them (ID, INT), the type of that value is specified as an ML datatype.
* `%nonterm`: specifies the set of all non-terminals that are used in the grammar of the Tiger language.
* `%pos`: specifies the type of the position of payload values as an ML non- polymorphic datatype

Other declarations are as follows:

* `%eop`, `%nonshift%`: specifies the end-of-parse symbol which is EOF. ML-Yacc will not know which terminal to end parsing on unless the `%eop` value is included in the `%term` list. `%nonshift` terminals cannot be shifted and will produce an error if found on the right-side of a production rule. `%nonshift` must be declared as shifting any one of them may introduce newer errors while the parser is trying to correct a previously caught error.

* `%left`, `%right`, `%nonassoc`: specify the associativities of terminals. The Tiger Language Reference Manual from (pages 512-521) provides the details for this.

* `%nodefault`: (to include in the future) suppresses default reductions i.e. in the internal LR table, if only production rule can be reduced in a state, then that rule may be made the default action for that state. This saves space when representing these tables but for later parser versions, this specification may be useful in debugging.

* `%start`: specifies the start symbol or the non-terminal where parsing begins. In its absence, the non-terminal for the first rule in the final section is used.

### Error-recovery declarations

The error-recovery algorithm used  is a modification of the [Burke-Fisher][3] method. ML-Yacc attempts error recovery by making a single token correction from the point where the syntax error is detected to 15 tokens before that point. This is because an error may not be obvious or be able to be caught at the very token where the error originates. The token corrections can involve deleting a token, substituting one token for another or inserting a missing token. As specified in [2], ML-Yacc:

> uses a simple heuristic priority scheme to order the corrections, and then arbitrarily chooses one of the corrections with the highest priority. You have some control over the priority scheme by being able to name a set of preferred insertions and a set of preferred substitutions. The priorities for corrections, ordered from highest to lowest priority, are preferred insertions, preferred substitutions, insertions, deletions, and substitutions.

Declarations that help in debugging errors are:

* `%verbose`: produces a file with a `.desc` extension which contains:
  - details of shift/reduce errors
  - details of states of the parser with errors specific to each state

* `%keyword`: specifies a list of all keywords in the Tiger language grammar so that the parser can always check which identifiers do not overlap with any of these words.

* `%prefer`: lists the terminals that are preferred during error correction via insertion of terminals. All other beings equal, insertions from this list are ** done before all other corrections.**

* `%value`: lists the values that should be associated with terminals during insertions or substitutions made during error-correction.

* `%change`: is a generalization of the `%prefer%` and the `%subst (terminals substituted for others while trying to parse errors, ** made after preferred insertions listed in `%prefer` when all other things are equal.**) commands.

### Grammar rules

* Start of parsing: It begins at the rule:

  `program : exp ()`

  with the rule for exp defined later

* Declarations: All declarations are handled using the rules:

  ```
  decs: dec decs ()
      |          ()

  dec: tydec  ()
   | vardec ()
   | fundec ()
  ```

  There can be more than one declaration in a block of declarations and they can be either type, variable or function declarations

* Type declarations: They are parsed using the rules:

  ```tydec: TYPE ID EQ ty ()

  ty: ID                     ()
    | LBRACE tyfields RBRACE ()
    | ARRAY OF ID            ()

  tyfields: (*epsilon*)           ()
          | ID COLON ID typeidseq ()

  typeidseq: COMMA ID COLON ID typeidseq ()
           |                             ()
  ```

  Types can be defined or existing types redefined using declarations. A type field can consist of an empty, one or more identifiers signifying types. A type can also be an array of a type.

* Variable declarations: are parsed using the rules:

  ```vardec: VAR ID ASSIGN exp          ()
        | VAR ID COLON ID ASSIGN exp ()
  ```

  A variable assignment consists of assigning an expression to a variable, with an optional type declaration.

* Function declarations: consist of:

  ``` fundec: FUNCTION ID LPAREN tyfields RPAREN EQ exp          ()
         | FUNCTION ID LPAREN tyfields RPAREN COLON ID EQ exp ()
   ```

   The first line specifies a procedure declaration (procedures do not return values), the second line is a function declaration; functions do return values, whose type is specified after the colon. The tyfields nonterminal specifies the names and types of the parameters passed to the function (always by values).


* Expressions: One or more expressions are parsed using:

  ```
  expseq: exp SEMICOLON expseq ()
        | exp                  ()

  exp: <rules>
  ```

  The rules for various expressions are:

  * let-in:
   ```
   letexp: LET decs IN expseq END ()
   ```
  * nil values, used in records or variables

  * locations: which contain values that can be read or assigned
   ```
   lvalue: ID                 ()
      | lvalue DOT ID            ()
      | lvalue LBRACK exp RBRACK ()
   ```

  * unit:
    ```
    LPAREN RPAREN                   ()
    ```
  * sequence of expressions:
  ```
  LPAREN expseq RPAREN            ()
  ```

  * literals: such as integers and strings

  * integer operations:
  ```
  intop: PLUS   ()
     | MINUS  ()
     | TIMES  ()
     | DIVIDE ()
  ```

  * integer comparisions:
   ```
   eqop:  EQ  ()
     | NEQ ()
     | LT  ()
     | LE  ()
     | GT  ()
     | GE  ()

    intcomp: INT eqop INT  ()
    ```

  * string comparisions:
   ```
   stringcomp: STRING eqop STRING ()
   ```

  * record expressions:
   ```
   recexp: ID LBRACE ID EQ exp recexplist RBRACE ()
       | ID LBRACE RBRACE                      ()

    recexplist: COMMA ID EQ exp recexplist ()
          | (*epsilon*)                ()

    recassign: ID DOT ID ASSIGN exp ()
    ```

    Records are created using the rule with recexp; recexplist specifies one or more ```identifier = expression```s for the record. The rule with recassign assigns an expression to a record variable.

  * array expressions:
   ```
   arrexp: ID LBRACK exp RBRACK OF exp ()
   ```

  * function calls:
   ```
   funccall: ID LPAREN RPAREN                 ()
           | ID LPAREN exp funcarglist RPAREN ()

   funcarglist: COMMA exp funcarglist ()
              |                       ()
   ```
  * if-then-else, if-then expressions:
     ```
     IF exp THEN exp ELSE exp        ()
     IF exp THEN exp                 ()
     ```
  * while expressions:
    ```
    WHILE exp DO exp                ()
    ```
  * for-to-do expression:
     ```
     FOR ID ASSIGN exp TO exp DO exp ()
     ```

  * break
    ```
    BREAK                           ()
    ```
  * arithmetic expressions with variables:

    ```
    ID EQ exp                       ()
    ID intop exp                    ()
    ```

### Documentation
[1]: <https://www.cs.princeton.edu/~appel/modern/ml/ml-yacc/manual.html#section2> Introduction to ML-Yacc
[2]: <https://citeseerx.ist.psu.edu/document?repid=rep1&type=pdf&doi=43247bea84122c52aa2d86aa86a8f4997825d419> User's Guide to ML-Lex and Ml-Yacc
[3]: <https://dl.acm.org/doi/10.1145/22719.22720> A practical method for LR and LL syntactic error diagnosis and recovery
