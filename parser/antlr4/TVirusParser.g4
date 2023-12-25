parser grammar TVirusParser;

options {
	tokenVocab = TVirusLexer;
}

primOp:
	SYM_ADD
	| SYM_MINUS
	| SYM_MUL
	| SYM_DIV
	| SYM_DE
	| SYM_NE
	| SYM_GT
	| SYM_LT
	| SYM_GE
	| SYM_LE;

primType: KW_INT | KW_BOOL;

type:
	primType					# typePrim
	| SYM_LPAR type SYM_RPAR	# typeParen
	| type SYM_ARROW type		# typeFunc
	| IDENT						# typeVar
	| type type					# typeApp;

scheme:
	KW_FORALL IDENT+ SYM_DOT type	# schemePoly
	| type							# schemeMono;

tBind: IDENT (SYM_COLON type)?;
sBind: IDENT (SYM_COLON scheme)?;
cBind:
	IDENT SYM_LPAR SYM_RPAR								# cBindEmpty
	| IDENT SYM_LPAR type (SYM_COMMA type)* SYM_RPAR	# cBindFull;

typeDecl: KW_DATA IDENT IDENT* SYM_EQ cBind (SYM_PIPE cBind)*;

pat:
	SYM_UNDERSCORE									# patWildcard
	| IDENT											# patVar
	| IDENT SYM_LPAR SYM_RPAR						# patConsEmpty
	| IDENT SYM_LPAR pat (SYM_COMMA pat)* SYM_RPAR	# patConsFull;

expr:
	expr primOp expr														# exprPrimOp
	| IDENT																	# exprVar
	| SYM_LPAR expr SYM_RPAR												# exprParen
	| LIT_INT																# exprLitInt
	| expr SYM_LPAR SYM_RPAR												# exprAppEmpty
	| expr SYM_LPAR expr (SYM_COMMA expr)* SYM_RPAR							# exprAppFull
	| SYM_LAM tBind (SYM_COMMA tBind)* SYM_DOT expr							# exprAbs
	| KW_LET sBind SYM_EQ expr (SYM_COMMA sBind SYM_EQ expr)* KW_IN expr	# exprLet
	| KW_MATCH expr KW_WITH (SYM_PIPE pat SYM_ARROW expr)+					# exprMatch;

valueDecl: KW_LET sBind SYM_EQ expr;

program: (typeDecl | valueDecl)*;