parser grammar TVirusParser;

options {
    tokenVocab = TVirusLexer;
}

primOp : SYM_ADD | SYM_MINUS | SYM_MUL | SYM_DIV | SYM_DE | SYM_NE | SYM_GT | SYM_LT | SYM_GE | SYM_LE;

primType : KW_INT;

type
    : primType                  #primitiveType
    | SYM_LPAR type SYM_RPAR    #parenType
    | type SYM_MUL type         #product
    | type SYM_ADD type         #sum
    | type SYM_ARROW type       #function
    | IDENT                     #defined
    ;

scheme
    : KW_FORALL IDENT+ SYM_DOT type   #poly
    | type                            #mono
    ;

tBind : IDENT (SYM_COLON type)?;
sBind : IDENT (SYM_COLON scheme)?;
cBind : IDENT type?;

typeDecl : KW_DATA IDENT SYM_EQ cBind (SYM_PIPE cBind)*;

expr
    : primOp                                                                         #primitiveOp
    | IDENT                                                                          #variable
    | SYM_LPAR expr SYM_RPAR                                                         #parenExpr
    | LIT_INT                                                                        #literalInteger
    | expr expr                                                                      #application
    | SYM_LAM tBind (SYM_COMMA tBind)* SYM_DOT expr                                  #abstraction
    | KW_LET sBind SYM_EQ expr (SYM_COMMA sBind SYM_EQ expr)* KW_IN expr             #let
    ;

valueDecl : KW_LET sBind SYM_EQ expr;

program : (typeDecl | valueDecl)*;
