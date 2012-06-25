%token int_const char_const float_const id string enumeration_const
%%

(def translation_unit
  (alt
    external_decl
    (conc translation_unit external_decl)))

(def external_decl
  (alt
    function_definition
    decl))

(def function_definition
  (alt
    (conc decl_specs declarator decl_list compound_stat)
    (conc declarator decl_list compound_stat)
    (conc decl_specs declarator compound_stat)
    (conc declarator compound_stat)))

(def decl
  (alt
    (conc decl_specs init_declarator_list (lit ";"))
    (conc decl_specs (lit ";"))))

(def decl_list
  (alt
    (conc decl_list decl)
    decl))

(def decl_specs
  (alt
    (conc storage_class_spec decl_specs)
    (conc storage_class_spec)
    (conc type_spec decl_specs)
    type_spec
    (conc type_qualifier decl_specs)
    type_qualifier))

(def storage_class_spec
  (alt (lit "auto") (lit "register") (lit "static") (lit "extern") (lit "typedef")))

(def type_spec
  (alt (lit "void")
       (lit "char")
       (lit "short")
       (lit "int")
       (lit "long")
       (lit "float")
       (lit "double")
       (lit "signed")
       (lit "unsigned")
       struct_or_union_spec
       enum_spec
       typedef_name))

(def type_qualifier
  (alt (lit "const") (lit "volatile")))

(def struct_or_union_spec
  (alt (conc struct_or_union id (lit "{") struct_decl_list (lit "}"))
       (conc struct_or_union (lit "{") struct_decl_list (lit "}"))
       (conc struct_or_union id)))

(def struct_or_union
  (alt (lit "struct") (lit "union")))

(def struct_decl_list
  (alt (conc struct_decl_list struct_decl)
       struct_decl))

(def init_declarator_list
  (alt (conc init_declarator_list (lit ",") init_declarator)
       init_declarator))

(def init_declarator
  (alt (conc declarator (lit "=") initializer)
       declarator))

(def struct_decl
  (conc spec_qualifier_list struct_declarator_list (lit ";")))

(def spec_qualifier_list
  (alt (conc type_spec spec_qualifier_list)
       type_spec
       (conc type_qualifier spec_qualifier_list)
       type_qualifier))

(def struct_declarator_list
  (alt (conc struct_declarator_list (lit ",") struct_declarator)
       struct_declarator))

(def struct_declarator
  (alt declarator
       (conc declarator (lit ":") const_exp)
       (conc (lit ":") const_exp)))

(def enum_spec
  (alt (conc (lit "enum") id (lit "{") enumerator_list (lit "}"))
       (conc (lit "enum") (lit "{") enumerator_list (lit "}"))
       (conc (lit "enum") id)))

(alt enumerator_list
  (alt enumerator
       (conc enumerator_list (lit ",") enumerator)))

(def enumerator
  (alt (con id (lit "=") const_exp)
       id))

(def declarator
  (alt (conc pointer direct_declarator)
       direct_declarator))

(def direct_declarator
  (alt id
       (conc (lit "(") declarator (lit ")"))
       (conc direct_declarator (lit "[") const_exp (lit "]"))
       (conc direct_declarator (lit "[") (lit "]"))
       (conc direct_declarator (lit "(") param_type_list (lit ")"))))
       (conc direct_declarator (lit "(") id_list (lit ")"))
       (conc direct_declarator (lit "(") (lit ")"))

(def pointer
  (alt (conc (lit "*") type_qualifier_list)
       (conc (lit "*") type_qualifier_list pointer)
       (conc (lit "*") pointer)
       (lit "*")))

(def type_qualifier_list
  (alt (conc type_qualifier_list type_qualifier)
       type_qualifier))

(def param_type_list
  (alt (conc param_list (lit ",") (lit "..."))
       param_list))

(def param_list
  (alt (conc param_list (lit ",") param_decl)
       param_decl))

(def param_decl
  (alt (conc decl_specs declarator)
       (conc decl_specs abstract_declarator)
       decl_specs))

(def id_list
  (alt (conc id_list (lit ",") id)
       id))

(def initializer
  (alt assignment_exp
       (conc (lit "{") initializer_list (lit "}"))
       (conc (lit "{") initializer_list (lit ",") (lit "}"))))

(def initializer_list
  (alt (conc initializer_list (lit ",") initializer)
       initializer))

(def type_name
  (alt (conc spec_qualifier_list abstract_declarator)
       spec_qualifier_list))

(def abstract_declarator
  (alt (conc pointer direct_abstract_declarator)
       (conc direct_abstract_declarator)
        pointer))

(def direct_abstract_declarator
  (alt (conc (lit "(") abstract_declarator (lit ")"))
       (conc direct_abstract_declarator (lit "[") const_exp (lit "]"))
       (conc (lit "[") const_exp (lit "]"))
       (conc direct_abstract_declarator (lit "[") (lit "]"))
       (conc (lit "[") (lit "]"))
       (conc direct_abstract_declarator (lit "(") param_type_list (lit ")"))
       (conc (lit "(") param_type_list (lit ")"))
       (conc direct_abstract_declarator (lit "(") (lit ")"))
       (conc (lit "(") (lit ")"))))

(def typedef_name
  (conc id))

(def stat
  (alt labeled_stat
       exp_stat
       compound_stat
       selection_stat
       iteration_stat
       jump_stat))

(def labeled_stat
  (alt (conc id (lit ":") stat)
       (conc (lit "case") const_exp (lit ":") stat)
       (conc (lit "default") (lit ":") stat)))

(def exp_stat
  (alt (conc exp (lit ";"))
       (lit ";")))

(def compound_stat
  (alt (conc (lit "{") decl_list stat_list (lit "}"))
       (conc (lit "{") stat_list (lit "}"))
       (conc (lit "{") decl_list (lit "}"))
       (conc (lit "{") (lit "}"))))

(def stat_list
  (rep+ stat))

(def selection_stat
  (alt (conc (lit "if") (lit "(") exp (lit ")") stat)
       (conc (lit "if") (lit "(") exp (lit ")") stat (lit "else") stat)
       (conc (lit "switch") (lit "(") exp (lit ")") stat)))

(def iteration_stat
  (alt (conc (lit "while") (lit "(") exp (lit ")") stat)
       (conc (lit "do") stat (lit "while") (lit "(") exp (lit ")") (lit ";"))
       (conc (lit "for") (lit "(") exp (lit ";") exp (lit ";") exp (lit ")") stat)
       (conc (lit "for") (lit "(") exp (lit ";") exp (lit ";") (lit ")") stat)
       (conc (lit "for") (lit "(") exp (lit ";") (lit ";") exp (lit ")") stat)
       (conc (lit "for") (lit "(") exp (lit ";") (lit ";") (lit ")") stat)
       (conc (lit "for") (lit "(") (lit ";") exp (lit ";") exp (lit ")") stat)
       (conc (lit "for") (lit "(") (lit ";") exp (lit ";") (lit ")") stat)
       (conc (lit "for") (lit "(") (lit ";") (lit ";") exp (lit ")") stat)
       (conc (lit "for") (lit "(") (lit ";") (lit ";") (lit ")") stat)))

(def jump_stat
  (alt (conc (lit "goto") id (lit ";"))
       (conc (lit "continue") (lit ";"))
       (conc (lit "break") (lit ";"))
       (conc (lit "return") exp (lit ";"))
       (conc (lit "return")    (lit ";"))))

(def exp
  (alt (conc exp (lit ",") assignment_exp)
       assignment_exp))

(def assignment_exp
  (alt (conc unary_exp assignment_operator assignment_exp)
       conditional_exp))

(def assignment_operator
  (alt (lit "=")
       (lit "*=")
       (lit "/=")
       (lit "%=")
       (lit "+=")
       (lit "-=")
       (lit "<<=")
       (lit ">>=")
       (lit "&=")
       (lit "^=")
       (lit "|=")))

(def conditional_exp
  (alt (conc logical_or_exp (lit "?") exp (lit ":") conditional_exp)
       logical_or_exp))


(def const_exp
  (conc conditional_exp))

(def logical_or_exp
  (alt (conc logical_or_exp (lit "||") logical_and_exp)
       logical_and_exp))

(def logical_and_exp
  (alt (conc logical_and_exp (lit "&&") inclusive_or_exp)
       inclusive_or_exp))

(def inclusive_or_exp
  (alt (conc inclusive_or_exp (lit "|") exclusive_or_exp)
       exclusive_or_exp))

(def exclusive_or_exp
  (alt (conc exclusive_or_exp (lit "^") and_exp)
       and_exp))

(def and_exp
  (alt (conc and_exp (lit "&") equality_exp)
       equality_exp))

(def equality_exp
  (alt (conc equality_exp (lit "==") relational_exp)
       (conc equality_exp (lit "!=") relational_exp)
       relational_exp))

(def relational_exp
  (alt (conc relational_exp (lit "<") shift_expression)
       (conc relational_exp (lit ">") shift_expression)
       (conc relational_exp (lit "<=") shift_expression)
       (conc relational_exp (lit ">=") shift_expression)
       shift_expression))

(def shift_expression
  (alt (conc shift_expression (lit "<<") additive_exp)
       (conc shift_expression (lit ">>") additive_exp)
       additive_exp))

(def additive_exp
  (alt (conc additive_exp (lit "+") mult_exp)
       (conc additive_exp (lit "-") mult_exp)
       mult_exp))

(def mult_exp
  (alt (conc mult_exp (lit "*") cast_exp)
       (conc mult_exp (lit "/") cast_exp)
       (conc mult_exp (lit "%") cast_exp)
       cast_exp

(def cast_exp
  (alt (conc (lit "(") type_name (lit ")") cast_exp)
       unary_exp))

(def unary_exp
  (alt (conc (lit "++") unary_exp)
       (conc (lit "--") unary_exp)
       (conc unary_operator cast_exp)
       (conc (lit "sizeof") unary_exp)
       (conc (lit "sizeof") (lit "(") type_name (lit ")"))
       postfix_exp))

(def unary_operator
  (alt  (lit "&")
        (lit "*")
        (lit "+")
        (lit "-")
        (lit "~")
        (lit "!")))

(def postfix_exp
  (alt (conc postfix_exp (lit "[") exp (lit "]"))
       (conc postfix_exp (lit "(") argument_exp_list (lit ")"))
       (conc postfix_exp (lit "(") (lit ")"))
       (conc postfix_exp (lit ".") id)
       (conc postfix_exp (lit "->") id)
       (conc postfix_exp (lit "++"))
       (conc postfix_exp (lit "--"))
       primary_exp))

(def primary_exp
  (alt (conc (lit "(") exp (lit ")"))
       id
       const
       string))

(def argument_exp_list
  (alt (conc argument_exp_list (lit ",") assignment_exp)
       assignment_exp))

(def const
  (alt  int_const
    (conc char_const)
    (conc float_const)
    (conc enumeration const)))
