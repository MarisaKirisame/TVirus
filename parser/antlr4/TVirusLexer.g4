lexer grammar TVirusLexer;

WHITESPACE : [ \r\n\t]+ -> skip;

fragment DIGIT : [0-9];
fragment LETTER : [a-zA-Z];

LIT_INT : '-'?DIGIT+;

SYM_LPAR : '(';
SYM_RPAR : ')';

SYM_ADD : '+';
SYM_MINUS : '-';
SYM_MUL : '*';
SYM_DIV : '/';

SYM_EQ : '=';
SYM_DE : '==';
SYM_NE : '!=';
SYM_GT : '>';
SYM_LT : '<';
SYM_GE : '>=';
SYM_LE : '<=';

SYM_DOT : '.';
SYM_LAM : 'λ' | '\\';
SYM_ARROW : '->';
SYM_PIPE : '|';
SYM_COLON : ':';
SYM_SEMICOLON : ';';
SYM_COMMA : ',';
SYM_UNDERSCORE : '_';

KW_LET : 'let';
KW_DATA : 'data';
KW_IN : 'in';
KW_FORALL : 'forall';
KW_MATCH : 'match';
KW_WITH : 'with';

KW_INT : 'Int';
KW_BOOL : 'Bool';

IDENT : LETTER (LETTER | DIGIT)*;
