type pos = int
type svalue = Tokens.svalue
type ('a, 'b) token = ('a, 'b) Tokens.token
type lexresult = (svalue, pos) token

val lineNum = ErrorMsg.lineNum
val linePos = ErrorMsg.linePos
fun err(p1,p2) = ErrorMsg.error p1

fun eof() = let val pos = hd(!linePos) in Tokens.EOF(pos,pos) end
fun getInt x = case Int.fromString x of
                    SOME i => i
                  | NONE => (ErrorMsg.impossible "Parse error for Integer.")

%%
%header (functor TigerLexFun(structure Tokens: Tiger_TOKENS));
whitespace = [\ \t\n];
digits=[0-9]+;
identifiers=[a-z]([a-zA-Z_0-9]*);
strings = (\"([a-zA-Z0-9_ ])*\");
unclosedStrings= (([a-zA-Z0-9][\t\n\\])+\") | (\"([a-zA-Z0-9]*)([\ \t\n\\])*);
reservedChars = [\?\:\;\(\)\[\]\{\}\.\+\-\=\<\>\&\|\!\@\#\$\%\^\&\*\(\),];
commentString = ([a-zA-Z0-9\ ]*{reservedChars}*[a-zA-Z0-9\ ]*);
validComments= (\/\*{whitespace}+{commentString}*{whitespace}+\*\/);
unclosedComment = (\/\*{whitespace}+{commentString}*{whitespace}*) |
({whitespace}*{commentString}*{whitespace}+\*\/);
nestedComments= (\/\*{whitespace}+{commentString}*{whitespace}*{validComments}*{commentString}*{whitespace}*{whitespace}+\*\/);
%%
type   => (Tokens.TYPE(yypos, yypos + 4));
var	   => (Tokens.VAR(yypos, yypos + 3));
function => (Tokens.FUNCTION(yypos, yypos + 8));
break  => (Tokens.BREAK(yypos, yypos + 5));
of     => (Tokens.OF(yypos, yypos + 2));
end    => (Tokens.END(yypos, yypos + 3));
in     => (Tokens.IN(yypos, yypos + 2));
nil    => (Tokens.NIL(yypos, yypos + 3));
let    => (Tokens.LET(yypos, yypos + 3));
do     => (Tokens.DO(yypos, yypos + 2));
to     => (Tokens.TO(yypos, yypos + 2));
for    => (Tokens.FOR(yypos, yypos + 3));
while  => (Tokens.WHILE(yypos, yypos + 5));
else   => (Tokens.ELSE(yypos, yypos + 4));
then   => (Tokens.THEN(yypos, yypos + 4));
if     => (Tokens.IF(yypos, yypos + 2));
array  => (Tokens.ARRAY(yypos, yypos + 5));
"|"     => (Tokens.OR(yypos, yypos + 1));
"&"    => (Tokens.AND(yypos, yypos + 1));
">="     => (Tokens.GE(yypos, yypos + 2));
">"     => (Tokens.GT(yypos, yypos + 1));
"<="     => (Tokens.LE(yypos, yypos + 2));
"<"     => (Tokens.LT(yypos, yypos + 1));
"<>"    => (Tokens.NEQ(yypos, yypos + 2));
"="     => (Tokens.EQ(yypos, yypos + 1));
"/"  => (Tokens.DIVIDE(yypos, yypos + 1));
"*"   => (Tokens.TIMES(yypos, yypos + 1));
"-"   => (Tokens.MINUS(yypos, yypos + 1));
"+"    => (Tokens.PLUS(yypos, yypos + 1));
"."     => (Tokens.DOT(yypos, yypos + 1));
"}"   => (Tokens.RBRACE(yypos, yypos + 1));
"{"  => (Tokens.LBRACE(yypos, yypos + 1));
"]"  => (Tokens.RBRACK(yypos, yypos + 1));
"["  => (Tokens.LBRACK(yypos, yypos + 1));
")"  => (Tokens.RPAREN(yypos, yypos + 1));
"("  => (Tokens.LPAREN(yypos, yypos + 1));
";"  => (Tokens.SEMICOLON(yypos, yypos + 1));
":"  => (Tokens.COLON(yypos, yypos + 1));
","	=> (Tokens.COMMA(yypos, yypos + 1));
":=" => (Tokens.ASSIGN(yypos, yypos + 2));
{digits} => (Tokens.INT(getInt yytext, yypos, yypos + size (yytext)));
{identifiers} => (Tokens.ID(yytext, yypos, yypos + size (yytext)));
{strings}   => (Tokens.STRING(yytext, yypos, yypos + size (yytext)));
{validComments}+ => (lineNum := !lineNum + 1; linePos := yypos :: !linePos; continue());
{unclosedComment} => (ErrorMsg.error yypos ("comment not closed " ^ yytext); continue());
{whitespace}+	=> (lineNum := !lineNum + 1; linePos := yypos :: !linePos; continue());
{nestedComments}+ => (lineNum := !lineNum + 1; linePos := yypos :: !linePos; continue());
.   => (ErrorMsg.error yypos ("illegal character " ^ yytext); continue());
