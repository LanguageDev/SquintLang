grammar Squint;

// Parser

file : declaration* EOF ;

attribute_list : attribute_sequence* ;
attribute_sequence : '#' '[' attribute (',' attribute)* ','? ']' ;
attribute : name ('(' expression_list ')')?
          | name ('(' type_list ')')?;

declaration : function_declaration
            | variable_declaration
            | import_declaration
            | type_declaration
            | impl_declaration
            ;

import_declaration : 'import' path+=name ('.' path+=name)* generic_param_list? ';' ;

function_declaration : attribute_list function_signature block_statement
                     | attribute_list function_signature '=' expression ';'
                     ;
function_signature : 'func' name generic_param_list? '(' parameter_list  ')' (':' return_type=type)? ;

type_declaration : record_type_declaration
                 | du_type_declaration
                 | trait_declaration
                 | type_alias_declaration
                 ;

type_declaration_member_list : (type_declaration_member (',' type_declaration_member)* ','?)? ;
type_declaration_member : ('var' | 'val') name (':' type) ;

record_type_declaration : attribute_list 'type' name generic_param_list? ('(' type_declaration_member_list ')')?  ';' ;

du_type_declaration : 'type' name generic_param_list? '=' ('|'? du_type_ctor ('|' du_type_ctor)*)? ';' ;
du_type_ctor : name '(' type_declaration_member_list ')' ;

trait_declaration : 'trait' name generic_param_list? '{' trait_member* '}' ;

trait_member : function_signature ';' # trait_function
             ;

type_alias_declaration : 'type' name generic_param_list? '=' type ';' ;

impl_declaration : 'impl' type '{' impl_member_declaration* '}'
                 | 'impl' type 'for' type '{' impl_member_declaration* '}'
                 ;

impl_member_declaration : function_declaration ;

variable_declaration : ('var' | 'val') name (':' type)? ('=' value=expression)? ';' ;

generic_param_list : '[' (name (',' name)* ','?)? ']' ;
generic_arg_list : '[' (type (',' type)* ','?)? ']' ;

parameter_list : (parameter (',' parameter)* ','?)? ;
parameter : name ':' type # typed_parameter
          | 'this'        # this_parameter
          ;

statement : 'return' value=expression? ';' # return_statement
          | declaration                    # declaration_statement
          | if_statement                   # keep_statement
          | while_statement                # keep_statement
          | for_statement                  # keep_statement
          | block_statement                # keep_statement
          | match_statement                # keep_statement
          | expression ';'                 # expression_statement
          ;

expression : atom_expression                                                 # wrapped_expression
           | '(' expression ')'                                              # grouping_expression
           | obj=expression '.' member=name '(' args=expression_list ')'     # member_call_expression
           | obj=expression '.' member=name                                  # member_access_expression
           | array=expression '[' indices=expression_list ']'                # index_expression
           | func=expression '(' args=expression_list ')'                    # call_expression
           | op=('+' | '-') subexpr=expression                               # ury_expression
           | left=expression op=('*' | '/' | 'mod' | 'rem') right=expression # bin_expression
           | left=expression op=('+' | '-') right=expression                 # bin_expression
           | left=expression op=rel_op right=expression                      # relational_expression
           | op='not' subexpr=expression                                     # ury_expression
           | left=expression op='and' right=expression                       # bin_expression
           | left=expression op='or' right=expression                        # bin_expression
           |<assoc=right> left=expression op=assign_op right=expression      # assign_expression
           ;
rel_op : '>' | '<' | '>=' | '<=' | '==' | '!=' ;
assign_op : '=' | '+=' | '-=' | '*=' | '/=' ;
atom_expression : int_literal
                | bool_literal
                | str_literal
                | name
                | block_expression
                | if_expression
                | while_expression
                | for_expression
                | match_expression
                | array_expression
                | 'this'
                ;

expression_list : (expression (',' expression)* ','?)? ;

type : type generic_arg_list                               # generic_type
     | '[' ']' element_type=type                           # array_type
     | '(' param_types=type_list ')' '->' return_type=type # func_type
     | '(' type ')'                                        # grouping_type
     | name                                                # name_type
     ;

type_list : (type (',' type)* ','?)? ;

array_expression : '[' expression_list ']' ;

block_expression : '{' statement* expression? '}' ;
block_statement  : '{' statement* '}' ;

if_expression : 'if' condition=expression 'then' then=expression 'else' els=expression
              | 'if' condition=expression 'then' then=expression
              ;
if_statement  : 'if' condition=expression 'then' then=statement 'else' els=statement
              | 'if' condition=expression 'then' then=statement
              ;

while_expression : 'while' condition=expression 'do' body=expression ;
while_statement  : 'while' condition=expression 'do' body=statement ;

for_expression : 'for' iterator=name 'in' iterable=expression 'do' body=expression ;
for_statement  : 'for' iterator=name 'in' iterable=expression 'do' body=statement ;

match_expression : 'match' expression 'with' '{' (match_expression_arm (',' match_expression_arm)* ','?)? '}' ;
match_expression_arm : pattern '->' expression ;

match_statement : 'match' expression 'with' '{' (match_statement_arm (',' match_statement_arm)* ','?)? '}' ;
match_statement_arm : pattern '->' statement ;

pattern : '_'                       # discard_pattern
        | name                      # name_pattern
        | name '(' pattern_list ')' # destructure_pattern
        ;
pattern_list : (pattern (',' pattern)* ','?)? ;

name : IDENTIFIER;
int_literal : INT_LITERAL;
bool_literal : 'true' | 'false';
str_literal : STR_LITERAL ;

// Lexer

COMMENT : '//' ~('\n' | '\r')* -> channel(HIDDEN) ;
WHITESPACE : [ \r\n\t] -> channel(HIDDEN) ;
IDENTIFIER : [A-Za-z_][A-Za-z0-9_]* ;
INT_LITERAL : [0-9]+ ;
STR_LITERAL : '"' ( EscapeSequence | ~('\\'|'"') )* '"' ;

fragment EscapeSequence : '\\' [abfnrtv"\\] ;
