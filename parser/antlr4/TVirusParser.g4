parser grammar TVirusParser;

options {
    tokenVocab = TVirusLexer;
}

expr
    :   LPAR expr RPAR                  #parenExpression
    |   VAR                             #variable
    |   INT                             #integer
    |   expr op=(PLUS|MINUS|MULT) expr  #operate
    |   IF LPAR expr op=(EQ|NEQ|GT|LT|GTEQ|LTEQ) expr RPAR THEN LCURL expr RCURL ELSE LCURL expr RCURL     #ifRule
    |   expr WS expr       #application
    |   LAMBDA VAR DOT expr        #abstraction
    |   REC WS VAR WS VAR DOT expr   #recRule
    ;