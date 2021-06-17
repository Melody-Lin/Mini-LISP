%{
#include "y.tab.h"
%}
digit [0-9]
letter [a-z]
number 0|[1-9]{digit}*|-[1-9]{digit}*
id {letter}({letter}|{digit}|-)*
%%
"+"   { return(ADD); }
"-"   { return(SUB); }
"*"   { return(MUL); }
"/"   { return(DIV); }
"mod" { return(MOD); }
">"   { return(GRT); }
"<"   { return(SML); }
"="   { return(EQL); }
"and" { return(AND); }
"or"  { return(OR); }
"not" { return(NOT); }
"#t"  { yylval.bval=1; return(BOOL); }
"#f"  { yylval.bval=0; return(BOOL); }
"print-num" { return(PRTNUM); }
"print-bool" { return(PRTBOOL); }
"define" { return(DEFINE); }
"fun" { return(FUNC); }
"if" { return(IF); }
{number} { yylval.ival=atoi(yytext); return(INUMBER); }
{id} { yylval.sval=strdup(yytext); return(ID); }
"("|")" { return(yytext[0]); }
[ \t]+ 
\n|.
%%