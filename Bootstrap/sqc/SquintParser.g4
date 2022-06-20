parser grammar SquintParser;

options { tokenVocab = SquintLexer; }

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

du_type_declaration : attribute_list 'type' name generic_param_list? '=' ('|'? du_type_ctor ('|' du_type_ctor)*)? ';' ;
du_type_ctor : attribute_list name ('(' type_declaration_member_list ')')? ;

trait_declaration : 'trait' name generic_param_list? OPEN_BRACE trait_member* CLOSE_BRACE ;

trait_member : function_signature ';' # trait_function
             ;

type_alias_declaration : 'type' name generic_param_list? '=' type ';' ;

impl_declaration : 'impl' type OPEN_BRACE impl_member_declaration* CLOSE_BRACE
                 | 'impl' type 'for' type OPEN_BRACE impl_member_declaration* CLOSE_BRACE
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
          | name ':'                       # label_statement
          | 'goto' name ';'                # goto_statement
          | expression ';'                 # expression_statement
          ;

expression : atom_expression                                                 # wrapped_expression
           | '(' expression ')'                                              # grouping_expression
           | expression '.' 'cast' '[' type ']'                              # cast_expression
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
atom_expression : literal
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
     | type '.' name                                       # nested_type
     | name                                                # name_type
     ;

type_list : (type (',' type)* ','?)? ;

array_expression : '[' expression_list ']' ;

block_expression : OPEN_BRACE statement* expression? CLOSE_BRACE ;
block_statement  : OPEN_BRACE statement* CLOSE_BRACE ;

if_expression : 'if' '(' condition=expression ')' then=expression 'else' els=expression
              | 'if' '(' condition=expression ')' then=expression
              ;
if_statement  : 'if' '(' condition=expression ')' then=statement 'else' els=statement
              | 'if' '(' condition=expression ')' then=statement
              ;

while_expression : 'while' '(' condition=expression ')' body=expression ;
while_statement  : 'while' '(' condition=expression ')' body=statement ;

for_expression : 'for' '(' iterator=name 'in' iterable=expression ')' body=expression ;
for_statement  : 'for' '(' iterator=name 'in' iterable=expression ')' body=statement ;

match_expression : 'match' '(' expression ')' '{' ('|'? match_expression_arm ('|' match_expression_arm)*)? '}' ;
match_expression_arm : pattern '->' expression ;

match_statement : 'match' '(' expression ')' '{' ('|'? match_statement_arm ('|' match_statement_arm)*)? '}' ;
match_statement_arm : pattern '->' statement ;

pattern : '_'                                # discard_pattern
        | name                               # name_pattern
        | type ('(' pattern_list ')')? name? # destructure_pattern
        | literal                            # literal_pattern
        ;
pattern_list : (pattern (',' pattern)* ','?)? ;

name : IDENTIFIER;

literal : int_literal
        | char_literal
        | bool_literal
        | char_literal
        | str_literal
        ;

int_literal : INT_LITERAL;
char_literal : CHAR_LITERAL;
bool_literal : 'true' | 'false';

str_literal : OPEN_QUOTE str_literal_content* CLOSE_QUOTE ;
str_literal_content : STR_LIT_FIELD_REF        # str_element_field_ref
                    | str_literal_interpolated # str_element_expr
                    | STR_LIT_ELEMENT          # str_element_lit
                    ;
str_literal_interpolated : STR_LIT_INTERPOLATE_START expression '}' ;
