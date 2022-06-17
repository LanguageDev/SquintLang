lexer grammar SquintLexer;

// Uninteresting
COMMENT : '//' ~('\n' | '\r')* -> channel(HIDDEN) ;
WHITESPACE : [ \r\n\t] -> channel(HIDDEN) ;

// Operators
OP_ADD : '+';
OP_SUB : '-';
OP_MUL : '*';
OP_DIV : '/';
OP_MOD : 'mod';
OP_REM : 'rem';
OP_AND : 'and';
OP_OR : 'or';
OP_NOT : 'not';
OP_ASGN : '=';
OP_EQ : '==';
OP_NE : '!=';
OP_GREATER : '>';
OP_LESS : '<';
OP_GR_EQ : '>=';
OP_LE_EQ : '<=';
OP_ADD_ASGN : '+=';
OP_SUB_ASGN : '-=';
OP_MUL_ASGN : '*=';
OP_DIV_ASGN : '/=';

// Punctuation
DOT : '.' ;
COMMA : ',' ;
COLON : ':' ;
SEMICOLON : ';' ;
HASH : '#' ;
PIPE : '|' ;
ARROW : '->' ;

// Pairwise
OPEN_PAREN : '(' ;
CLOSE_PAREN : ')' ;
OPEN_BRACE : '{' -> pushMode(DEFAULT_MODE) ;
CLOSE_BRACE : '}' -> popMode ;
OPEN_BRACKET : '[' ;
CLOSE_BRACKET : ']' ;

// Keywords
KW_IMPORT : 'import' ;
KW_FUNC : 'func' ;
KW_VAR : 'var' ;
KW_VAL : 'val' ;
KW_TYPE : 'type' ;
KW_TRAIT : 'trait' ;
KW_THIS : 'this' ;
KW_RETURN : 'return' ;
KW_FOR : 'for' ;
KW_IMPL : 'impl' ;
KW_IF : 'if' ;
KW_THEN : 'then' ;
KW_ELSE : 'else' ;
KW_WHILE : 'while' ;
KW_DO : 'do' ;
KW_MATCH : 'match' ;
KW_WITH : 'with' ;
KW_IN : 'in' ;
KW_TRUE : 'true' ;
KW_FALSE : 'false' ;

// Literals
DISCARD : '_' ;
IDENTIFIER : [A-Za-z_][A-Za-z0-9_]* ;
INT_LITERAL : [0-9]+ ;
CHAR_LITERAL : '\'' (~('\n' | '\r' | '\'') | ('\'' [abfnrtv'\\])) '\'' ;

OPEN_QUOTE : '"' -> pushMode(LineString) ;

mode LineString ;

CLOSE_QUOTE : '"' -> popMode ;

STR_LIT_ELEMENT : ~('\\' | '"' | '$')+
                | '$'
                | ('\\' [abfnrtv"\\$])
                ;
STR_LIT_FIELD_REF : '$' IDENTIFIER ;

STR_LIT_INTERPOLATE_START : '${' -> pushMode(DEFAULT_MODE) ;
