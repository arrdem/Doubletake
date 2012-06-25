(ns doubletake.lang.c.parser
  (:use
     ; stdlib
     [clojure.set]
     ; this codebase
     doubletake.lang.c.primitives
     ; third party
     [name.choi.joshua.fnparse]))

; token IDENTIFIER CONSTANT STRING_LITERAL SIZEOF
; token PTR_OP INC_OP DEC_OP LEFT_OP RIGHT_OP LE_OP GE_OP EQ_OP NE_OP
; token AND_OP OR_OP MUL_ASSIGN DIV_ASSIGN MOD_ASSIGN ADD_ASSIGN
; token SUB_ASSIGN LEFT_ASSIGN RIGHT_ASSIGN AND_ASSIGN
; token XOR_ASSIGN OR_ASSIGN TYPE_NAME
; token TYPEDEF EXTERN STATIC AUTO REGISTER INLINE RESTRICT
; token CHAR SHORT INT LONG SIGNED UNSIGNED FLOAT DOUBLE CONST VOLATILE VOID
; token BOOL COMPLEX IMAGINARY
; token STRUCT UNION ENUM ELLIPSIS
; token CASE DEFAULT IF ELSE SWITCH WHILE DO FOR GOTO CONTINUE BREAK RETURN
;
; start translation_unit

;-------------------------------------------------------------------------------
; Def all the symbols which aren't imported from primitives
;-------------------------------------------------------------------------------
(def primary_expression)
(def postfix_expression)
(def argument_expression_list)
(def unary_expression)
(def unary_operator)
(def cast_expression)
(def multiplicative_expression)
(def additive_expression)
(def shift_expression)
(def relational_expression)
(def equality_expression)
(def and_expression)
(def exclusive_or_expression)
(def inclusive_or_expression)
(def logical_and_expression)
(def logical_or_expression)
(def conditional_expression)
(def assignment_expression)
(def assignment_operator)
(def expression)
(def constant_expression)
(def declaration)
(def declaration_specifiers)
(def init_declarator_list)
(def init_declarator)
(def storage_class_specifier)
(def type_specifier)
(def struct_or_union_specifier)
(def struct_or_union)
(def struct_declaration_list)
(def struct_declaration)
(def specifier_qualifier_list)
(def struct_declarator_list)
(def struct_declarator)
(def enum_specifier)
(def enumerator_list)
(def enumerator)
(def type_qualifier)
(def function_specifier)
(def declarator)
(def direct_declarator)
(def pointer)
(def type_qualifier_list)
(def parameter_type_list)
(def parameter_list)
(def parameter_declaration)
(def identifier_list)
(def type_name)
(def abstract_declarator)
(def direct_abstract_declarator)
(def initializer)
(def initializer_list)
(def designation)
(def designator_list)
(def designator)
(def statement)
(def labeled_statement)
(def compound_statement)
(def block_item_list)
(def block_item)
(def expression_statement)
(def selection_statement)
(def iteration_statement)
(def jump_statement)
(def translation_unit)
(def external_declaration)
(def function_definition)
(def declaration_list)

;-------------------------------------------------------------------------------
; Now actually bind em...
;-------------------------------------------------------------------------------
(def primary_expression
  (alt IDENTIFIER
       (conc (lit (lit "(")) expression (lit (lit ")")))
       CONSTANT
       STRING_LITERAL))

(def postfix_expression
  (alt primary_expression
      (conc postfix_expression (lit "[") expression (lit "]"))
      (conc postfix_expression (lit "(") (lit ")"))
      (conc postfix_expression (lit "(") argument_expression_list (lit ")"))
      (conc postfix_expression (lit ".") IDENTIFIER)
      (conc postfix_expression PTR_OP IDENTIFIER)
      (conc postfix_expression INC_OP)
      (conc postfix_expression DEC_OP)
      (conc (lit "(") type_name (lit ")") (lit "{") initializer_list (lit "}"))
      (conc (lit "(") type_name (lit ")") (lit "{") initializer_list (lit ",")
            (lit "}"))))

(def argument_expression_list
  (alt (conc argument_expression_list (lit ",") assignment_expression)
       assignment_expression))

(def unary_expression
  (alt (conc INC_OP unary_expression)
       (conc DEC_OP unary_expression)
       (conc unary_operator cast_expression)
       (conc SIZEOF unary_expression)
       (conc SIZEOF (lit "(") type_name (lit ")"))
       postfix_expression))

(def unary_operator
  (alt (lit "&")
       (lit "*")
       (lit "+")
       (lit "-")
       (lit "~")
       (lit "!")))

(def cast_expression
  (alt (conc (lit "(") type_name (lit ")") cast_expression)
       unary_expression))

(def multiplicative_expression
  (alt (conc multiplicative_expression (lit "*") cast_expression)
       (conc multiplicative_expression (lit "/") cast_expression)
       (conc multiplicative_expression (lit "%") cast_expression)
       cast_expression))

(def additive_expression
  (alt (conc additive_expression (lit "+") multiplicative_expression)
       (conc additive_expression (lit "-") multiplicative_expression)
       multiplicative_expression))

(def shift_expression
  (alt (conc shift_expression LEFT_OP additive_expression)
       (conc shift_expression RIGHT_OP additive_expression)
       additive_expression))

(def relational_expression
  (alt (conc relational_expression (lit "<") shift_expression)
       (conc relational_expression (lit ">") shift_expression)
       (conc relational_expression LE_OP shift_expression)
       (conc relational_expression GE_OP shift_expression)
       shift_expression))

(def equality_expression
  (alt (conc equality_expression EQ_OP relational_expression)
       (conc equality_expression NE_OP relational_expression)
       relational_expression))

(def and_expression
  (alt (conc and_expression (lit "&") equality_expression)
       equality_expression))

(def exclusive_or_expression
  (alt (conc exclusive_or_expression (lit "^") and_expression)
       and_expression))

(def inclusive_or_expression
  (alt (conc inclusive_or_expression (lit "|") exclusive_or_expression)
       exclusive_or_expression))

(def logical_and_expression
  (alt (conc logical_and_expression AND_OP inclusive_or_expression)
       inclusive_or_expression))

(def logical_or_expression
  (alt (conc logical_or_expression OR_OP logical_and_expression)
       logical_and_expression))

(def conditional_expression
  (alt (conc logical_or_expression (lit "?") expression (lit ":") conditional_expression)
       logical_or_expression))

(def assignment_expression
  (alt (conc unary_expression assignment_operator assignment_expression)
       conditional_expression))

(def assignment_operator
  (alt (lit "=")
      MUL_ASSIGN
      DIV_ASSIGN
      MOD_ASSIGN
      ADD_ASSIGN
      SUB_ASSIGN
      LEFT_ASSIGN
      RIGHT_ASSIGN
      AND_ASSIGN
      XOR_ASSIGN
      OR_ASSIGN))

(def expression
  (alt (conc expression (lit ",") assignment_expression)
       assignment_expression))

(def constant_expression
  (alt conditional_expression))

(def declaration
  (alt (conc declaration_specifiers init_declarator_list (lit ";"))
       (conc declaration_specifiers (lit ";"))))

(def declaration_specifiers
  (alt (conc storage_class_specifier declaration_specifiers)
       (conc type_specifier)
       (conc type_specifier declaration_specifiers)
       (conc type_qualifier)
       (conc type_qualifier declaration_specifiers)
       (conc function_specifier)
       (conc function_specifier declaration_specifiers)
       storage_class_specifier))

(def init_declarator_list
  (alt (conc init_declarator_list (lit ",") init_declarator)
       init_declarator))

(def init_declarator
  (alt (conc declarator (lit "=") initializer)
       declarator))

(def storage_class_specifier
  (alt TYPEDEF
      EXTERN
      STATIC
      AUTO
      REGISTER))

(def type_specifier
  (alt VOID
       CHAR
       SHORT
       INT
       LONG
       FLOAT
       DOUBLE
       SIGNED
       UNSIGNED
       BOOL
       COMPLEX
       IMAGINARY
       struct_or_union_specifier
       enum_specifier
       TYPE_NAME))

(def struct_or_union_specifier
  (alt (conc struct_or_union IDENTIFIER (lit "{") struct_declaration_list (lit "}"))
      (conc struct_or_union (lit "{") struct_declaration_list (lit "}"))
      (conc struct_or_union IDENTIFIER)))

(def struct_or_union
  (alt STRUCT
       UNION))

(def struct_declaration_list
  (alt (conc struct_declaration_list struct_declaration)
       struct_declaration))

(def struct_declaration
  (conc specifier_qualifier_list struct_declarator_list (lit ";")))

(def specifier_qualifier_list
  (alt (conc type_specifier specifier_qualifier_list)
       (conc type_specifier)
       (conc type_qualifier specifier_qualifier_list)
       (conc type_qualifier)))

(def struct_declarator_list
  (alt (conc struct_declarator_list (lit ",") struct_declarator)
       struct_declarator))

(def struct_declarator
  (alt (conc (lit ":") constant_expression)
       (conc declarator (lit ":") constant_expression)
       declarator))

(def enum_specifier
  (alt (conc ENUM (lit "{") enumerator_list (lit "}"))
       (conc ENUM IDENTIFIER (lit "{") enumerator_list (lit "}"))
       (conc ENUM (lit "{") enumerator_list (lit ",") (lit "}"))
       (conc ENUM IDENTIFIER (lit "{") enumerator_list (lit ",") (lit "}"))
       (conc ENUM IDENTIFIER)))

(def enumerator_list
  (alt (conc enumerator_list (lit ",") enumerator)
       enumerator))

(def enumerator
  (alt (conc IDENTIFIER (lit "=") constant_expression)
       IDENTIFIER))

(def type_qualifier
  (alt CONST
      RESTRICT
      VOLATILE))

(def function_specifier
  (alt INLINE))

(def declarator
  (alt (conc pointer direct_declarator)
       (conc direct_declarator)))

(def direct_declarator
  (alt (conc (lit "(") declarator (lit ")"))
       (conc direct_declarator (lit "[") type_qualifier_list assignment_expression (lit "]"))
       (conc direct_declarator (lit "[") type_qualifier_list (lit "]"))
       (conc direct_declarator (lit "[") assignment_expression (lit "]"))
       (conc direct_declarator (lit "[") STATIC type_qualifier_list assignment_expression (lit "]"))
       (conc direct_declarator (lit "[") type_qualifier_list STATIC assignment_expression (lit "]"))
       (conc direct_declarator (lit "[") type_qualifier_list (lit "*") (lit "]"))
       (conc direct_declarator (lit "[") (lit "*") (lit "]"))
       (conc direct_declarator (lit "[") (lit "]"))
       (conc direct_declarator (lit "(") parameter_type_list (lit ")"))
       (conc direct_declarator (lit "(") identifier_list (lit ")"))
       (conc direct_declarator (lit "(") (lit ")"))))

(def pointer
  (alt (conc (lit "*") type_qualifier_list)
       (conc (lit "*") pointer)
       (conc (lit "*") type_qualifier_list pointer)
       (lit "*")))

(def type_qualifier_list
  (alt (conc type_qualifier_list type_qualifier)
       type_qualifier))

(def parameter_type_list
  (alt (conc parameter_list (lit ",") ELLIPSIS)
       parameter_list))

(def parameter_list
  (alt (conc parameter_list (lit ",") parameter_declaration)
       parameter_declaration))

(def parameter_declaration
  (alt (conc declaration_specifiers declarator)
       (conc declaration_specifiers abstract_declarator)
       (conc declaration_specifiers)))

(def identifier_list
  (alt (conc identifier_list (lit ",") IDENTIFIER)
       IDENTIFIER))

(def type_name
  (alt (conc specifier_qualifier_list abstract_declarator)
       specifier_qualifier_list))

(def abstract_declarator
  (alt (conc direct_abstract_declarator)
       (conc pointer direct_abstract_declarator)
       pointer))

(def direct_abstract_declarator
  (alt (conc (lit "(") abstract_declarator (lit ")"))
       (conc (lit "[") (lit "]"))
       (conc (lit "[") assignment_expression (lit "]"))
       (conc direct_abstract_declarator (lit "[") (lit "]"))
       (conc direct_abstract_declarator (lit "[") assignment_expression (lit "]"))
       (conc (lit "[") (lit "*") (lit "]"))
       (conc direct_abstract_declarator (lit "[") (lit "*") (lit "]"))
       (conc (lit "(") (lit ")"))
       (conc (lit "(") parameter_type_list (lit ")"))
       (conc direct_abstract_declarator (lit "(") (lit ")"))
       (conc direct_abstract_declarator (lit "(") parameter_type_list (lit ")"))))

(def initializer
  (alt (conc (lit "{") initializer_list (lit "}"))
       (conc (lit "{") initializer_list (lit ",") (lit "}"))
       assignment_expression))

(def initializer_list
  (alt (conc designation initializer)
       (conc initializer_list (lit ",") initializer)
       (conc initializer_list (lit ",") designation initializer)
       initializer))

(def designation
  (conc designator_list (lit "=")))

(def designator_list
  (alt (conc designator_list designator)
       designator))

(def designator
  (alt (conc (lit "[") constant_expression (lit "]"))
       (conc (lit ".") IDENTIFIER)))

(def statement
  (alt compound_statement
       expression_statement
       selection_statement
       iteration_statement
       jump_statement
       labeled_statement))

(def labeled_statement
  (alt (conc IDENTIFIER (lit ":") statement)
       (conc CASE constant_expression (lit ":") statement)
       (conc DEFAULT (lit ":") statement)))

(def compound_statement
  (alt (conc (lit "{") (lit "}"))
       (conc (lit "{") block_item_list (lit "}"))))

(def block_item_list
  (alt (conc block_item_list block_item)
       block_item)))

(def block_item
  (alt declaration
       statement))

(def expression_statement
  (alt (conc expression (lit ";"))
       (lit ";")))

(def selection_statement
  (alt (conc IF (lit "(") expression (lit ")") statement)
       (conc IF (lit "(") expression (lit ")") statement ELSE statement)
       (conc SWITCH (lit "(") expression (lit ")") statement)))

(def iteration_statement
  (alt (conc WHILE (lit "(") expression (lit ")") statement)
       (conc DO statement WHILE (lit "(") expression (lit ")") (lit ";"))
       (conc FOR (lit "(") expression_statement expression_statement (lit ")") statement)
       (conc FOR (lit "(") expression_statement expression_statement expression (lit ")") statement)
       (conc FOR (lit "(") declaration expression_statement (lit ")") statement)
       (conc FOR (lit "(") declaration expression_statement expression (lit ")") statement)))

(def jump_statement
  (alt (conc GOTO IDENTIFIER (lit ";"))
       (conc CONTINUE (lit ";"))
       (conc BREAK (lit ";"))
       (conc RETURN (lit ";"))
       (conc RETURN expression (lit ";"))))

(def translation_unit
  (alt (conc translation_unit external_declaration)
       external_declaration))

(def external_declaration
  (alt function_definition declaration))

(def function_definition
  (alt (conc declaration_specifiers declarator declaration_list compound_statement)
       (conc declaration_specifiers declarator compound_statement)))

(def declaration_list
  (alt (conc declaration_list declaration)
       declaration))
