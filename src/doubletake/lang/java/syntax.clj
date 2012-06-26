(ns doubletake.lang.java.syntax
  (:use 
    ; stdlib
    clojure.set
    ; this codebase
    doubletake.lang.java.primitives
    ; third party
    name.choi.joshua.fnparse))

;------------------------------------------------------------------------------
; DEF all the symbols in this file...
;------------------------------------------------------------------------------
(def QualifiedIdentifier)
(def QualifiedIdentifierList)
(def CompilationUnit)
(def ImportDeclaration)
(def TypeDeclaration)
(def ClassOrInterfaceDeclaration)
(def ClassDeclaration)
(def InterfaceDeclaration)  
(def NormalClassDeclaration)
(def EnumDeclaration)
(def NormalInterfaceDeclaration)
(def AnnotationTypeDeclaration)
(def Type)
(def BasicType)
(def ReferenceType)
(def TypeArguments)
(def TypeArgument)
(def NonWildcardTypeArguments)
(def TypeList)
(def TypeArgumentsOrDiamond)
(def NonWildcardTypeArgumentsOrDiamond)
(def TypeParameters)
(def TypeParameter)
(def Bound)
(def Modifier)
(def Annotations)
(def Annotation)
(def AnnotationElement)
(def ElementValuePairs)
(def ElementValuePair)
(def ElementValue)
(def ElementValueArrayInitializer)
(def ElementValues)
(def ClassBody)
(def ClassBodyDeclaration)
(def MemberDecl)
(def MethodOrFieldDecl)
(def MethodOrFieldRest)
(def FieldDeclaratorsRest)
(def MethodDeclaratorRest)
(def VoidMethodDeclaratorRest)
(def ConstructorDeclaratorRest)
(def GenericMethodOrConstructorDecl)
(def GenericMethodOrConstructorRest)
(def InterfaceBody)
(def InterfaceBodyDeclaration)
(def InterfaceMemberDecl)
(def InterfaceMethodOrFieldDecl)
(def InterfaceMethodOrFieldRest)
(def ConstantDeclaratorsRest)
(def ConstantDeclaratorRest)
(def ConstantDeclarator)
(def InterfaceMethodDeclaratorRest)
(def VoidInterfaceMethodDeclaratorRest)
(def InterfaceGenericMethodDecl)
(def FormalParameters)
(def FormalParameterDecls)
(def VariableModifier)
(def FormalParameterDeclsRest)
(def VariableDeclaratorId)
(def VariableDeclarators)
(def VariableDeclarator)
(def VariableDeclaratorRest)
(def VariableInitializer)
(def ArrayInitializer)
(def Block)
(def BlockStatements)
(def BlockStatement)
(def LocalVariableDeclarationStatement)
(def Statement)
(def StatementExpression)
(def Catches)
(def CatchClause)
(def CatchType)
(def Finally)
(def ResourceSpecification)
(def Resources)
(def Resource)
(def SwitchBlockStatementGroups)
(def SwitchBlockStatementGroup)
(def SwitchLabels) 
(def SwitchLabel)
(def EnumConstantName)
(def ForControl)
(def ForVarControl)
(def ForVarControlRest)
(def ForVariableDeclaratorsRest)
(def ForInit)
(def ForUpdate)
(def Expression)
(def AssignmentOperator)
(def Expression1)
(def Expression1Rest)
(def Expression2)
(def Expression2Rest)
(def InfixOp)
(def Expression3)
(def PrefixOp)
(def PostfixOp)
(def Primary)
(def ParExpression)
(def Arguments)
(def SuperSuffix)
(def ExplicitGenericInvocationSuffix)
(def Creator)
(def CreatedName)
(def ClassCreatorRest)
(def ArrayCreatorRest)
(def IdentifierSuffix)
(def ExplicitGenericInvocation)
(def InnerCreator)
(def Selector)
(def EnumBody)
(def EnumConstants)
(def EnumConstant)
(def EnumBodyDeclarations)
(def AnnotationTypeBody)
(def AnnotationTypeElementDeclarations)
(def AnnotationTypeElementDeclaration)
(def AnnotationTypeElementRest)
(def AnnotationMethodOrConstantRest)
(def AnnotationMethodRest)

;------------------------------------------------------------------------------
; Now def the actual matching patterns
;------------------------------------------------------------------------------
(def QualifiedIdentifier
  (conc
    Identifier (rep* (conc (lit ".") Identifier))))

(def QualifiedIdentifierList
  (conc QualifiedIdentifier
        (rep*
          (conc (lit ",")
                QualifiedIdentifier))))

;||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||

(def CompilationUnit
  (conc
    (opt
      (conc (opt Annotations)
            (lit "package")
            QualifiedIdentifier
            (lit ";")
            (opt (lit "\n"))))
    (rep* ImportDeclaration)
    (rep* TypeDeclaration)))

(def ImportDeclaration
  (conc
    (lit "import")
    (opt (conc (lit "static")))
    (conc
      QualifiedIdentifier
      (opt (conc (lit ".") (lit "*"))))

    (lit ";")
    (opt (lit "\n"))))

(def TypeDeclaration
  (alt
    ClassOrInterfaceDeclaration
    (conc (lit ";") (lit "\n"))))

(def ClassOrInterfaceDeclaration
  (conc (rep* Modifier)
        (alt ClassDeclaration
             InterfaceDeclaration)))

(def ClassDeclaration
  (alt
    NormalClassDeclaration
    EnumDeclaration))

(def InterfaceDeclaration
  (alt
    NormalInterfaceDeclaration
    AnnotationTypeDeclaration))

(def NormalClassDeclaration
  (conc
    (lit "class")
    Identifier
    (opt TypeParameters)
    (opt (conc (lit "extends") Type))
    (opt (conc (lit "implements") TypeList))
    ClassBody))

(def EnumDeclaration
  (conc
    (lit "enum")
    Identifier
    (opt (conc (lit "implements") TypeList))
    EnumBody))

(def NormalInterfaceDeclaration
  (conc
    (lit "interface")
    Identifier
    (opt TypeParameters)
    (opt (conc (lit "extends") TypeList))
    InterfaceBody))

(def AnnotationTypeDeclaration
  (conc
    (lit "@interface") Identifier AnnotationTypeBody))

;||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||

(def Type
  (alt
    (conc BasicType (rep* (lit-conc-seq (seq "[]"))))
    (conc ReferenceType (rep* (lit-conc-seq (seq "[]"))))))

(def BasicType
  (lit-alt-seq
    ["byte"
     "short"
     "char"
     "int"
     "long"
     "float"
     "double"
     "boolean"]))

(def ReferenceType
  (conc
    Identifier
    (opt TypeArguments)
    (rep*
      (conc (lit ".") Identifier (opt TypeArguments)))))

(def TypeArguments
  (conc
    (lit "<")
    TypeArgument
    (rep*
      (conc (lit ",") TypeArgument))
    (lit ">")))

(def TypeArgument
  (alt
    ReferenceType
    (lit "?") (opt (alt (lit "extends")
                        (lit "super")) ReferenceType)))

;||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||

(def NonWildcardTypeArguments
  (conc (lit "<") TypeList (lit ">")))

(def TypeList
  (conc
    ReferenceType (rep* (conc (lit ",") ReferenceType))))


(def TypeArgumentsOrDiamond
  (alt
    (conc (lit "<") (lit ">"))
    TypeArguments))

(def NonWildcardTypeArgumentsOrDiamond
  (alt
    (conc (lit "<") (lit ">"))
    NonWildcardTypeArguments))


(def TypeParameters
  (conc
    (lit "<")
    TypeParameter
    (rep* (conc (lit ",")
                TypeParameter
               ))
    (lit ">")))

(def TypeParameter
  (conc
    Identifier (opt (conc (lit "extends") Bound))))

(def Bound
  (conc
    ReferenceType
    (rep* (conc (lit "&")
                ReferenceType))))

;||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||

(def Modifier
  (alt
    Annotation
    (lit "public")
    (lit "protected")
    (lit "private")
    (lit "static")
    (lit "abstract")
    (lit "final")
    (lit "native")
    (lit "synchronized")
    (lit "transient")
    (lit "volatile")
    (lit "strictfp")))

(def Annotations
  (conc
    Annotation (rep* Annotation)))

(def Annotation
  (conc
    (lit "@") QualifiedIdentifier (opt (conc (lit "(")
                                             (opt (conc
                                                    AnnotationElement
                                                   ))
                                            (lit ")")))))

(def AnnotationElement
  (alt
    ElementValuePairs
    ElementValue))

(def ElementValuePairs
  (conc
    ElementValuePair
   (rep* (conc (lit ",") ElementValuePair))))

(def ElementValuePair
  (conc
    Identifier
    (lit "=")
    ElementValue))

(def ElementValue
  (alt
    Annotation
    Expression1
    ElementValueArrayInitializer))

(def ElementValueArrayInitializer
  (conc
    (lit "{")
    (opt ElementValues)
    (opt ",")
    (lit "}")))

(def ElementValues
  (conc
    ElementValue
    (opt (conc (lit ",")
               ElementValue
              ))))


;||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||

(def ClassBody
  (conc
    (lit "{")
    (opt ClassBodyDeclaration)
    (lit "}")))

(def ClassBodyDeclaration
  (alt
    (lit ";")
    (conc (rep* (conc Modifier)
                MemberDecl))
    (conc (opt (lit "static")) Block)))

(def MemberDecl
  (alt
    MethodOrFieldDecl
    (conc (lit "void")
          Identifier
          VoidMethodDeclaratorRest)
    (conc Identifier ConstructorDeclaratorRest)
    GenericMethodOrConstructorDecl
    ClassDeclaration
    InterfaceDeclaration))

(def MethodOrFieldDecl
  (conc
    Type Identifier MethodOrFieldRest))

(def MethodOrFieldRest
  (alt
    (conc FieldDeclaratorsRest (lit ";"))
    MethodDeclaratorRest))

(def FieldDeclaratorsRest
  (conc
    VariableDeclaratorRest
    (rep* (conc (lit ",") VariableDeclarator))))

(def MethodDeclaratorRest
  (conc
    FormalParameters
    (rep* (conc (lit "[")
                (lit "]")))
    (opt (conc (lit "throws")

               QualifiedIdentifierList))
    (alt Block (lit ";"))))

(def VoidMethodDeclaratorRest
  (conc
    FormalParameters
    (opt (conc (lit "throws") QualifiedIdentifierList))
    (alt Block (conc (lit ";")))))

(def ConstructorDeclaratorRest
  (conc
    FormalParameters
    (opt (conc (lit "throws")
              QualifiedIdentifierList))

    Block))

(def GenericMethodOrConstructorDecl
  (conc
    TypeParameters GenericMethodOrConstructorRest))

(def GenericMethodOrConstructorRest
  (alt
    (conc (alt Type (lit "void"))
          Identifier
          MethodDeclaratorRest)
    (conc Identifier
          ConstructorDeclaratorRest)))

;||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||

(def InterfaceBody
  (conc
    (lit "{")
    (rep* InterfaceBodyDeclaration)
    (lit "}")))

(def InterfaceBodyDeclaration
  (alt
    (lit ";")
    (conc (rep* Modifier) InterfaceMemberDecl)))

(def InterfaceMemberDecl
  (alt
    InterfaceMethodOrFieldDecl
    (conc (lit "void") Identifier VoidInterfaceMethodDeclaratorRest)
    InterfaceGenericMethodDecl
    ClassDeclaration
    InterfaceDeclaration))

(def InterfaceMethodOrFieldDecl
  (conc
    Type Identifier InterfaceMethodOrFieldRest))

(def InterfaceMethodOrFieldRest
  (alt
    (conc ConstantDeclaratorsRest (lit ";"))
    InterfaceMethodDeclaratorRest))

(def ConstantDeclaratorsRest
  (conc
    ConstantDeclaratorRest
    (rep* (conc (lit ",") ConstantDeclarator))))

(def ConstantDeclaratorRest
  (conc
    (rep* (conc (lit "[") (lit "]")))
    (lit "=")
    VariableInitializer))

(def ConstantDeclarator
  (conc
    Identifier ConstantDeclaratorRest))

(def InterfaceMethodDeclaratorRest
  (conc
    FormalParameters
    (rep* (conc (lit "[") (lit "]")))
    (opt (conc (lit "throws")
               QualifiedIdentifierList))
    (lit ";")))

(def VoidInterfaceMethodDeclaratorRest
  (conc
    FormalParameters
    (opt (conc (lit "throws")
               QualifiedIdentifierList))
    (lit ";")))

(def InterfaceGenericMethodDecl
  (conc
    TypeParameters
    (alt Type (lit "void"))
    Identifier
    InterfaceMethodDeclaratorRest))

;|||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||

(def FormalParameters
  (conc
    (lit "(")
    (opt FormalParameterDecls)
    (lit ")")))

(def FormalParameterDecls
  (conc
    (rep* VariableModifier) Type FormalParameterDeclsRest))

(def VariableModifier
  (alt
    (lit "final")
    Annotation))

(def FormalParameterDeclsRest
  (alt
    (conc VariableDeclaratorId (opt (conc (lit ",") FormalParameterDecls)))
    (conc (lit "...") VariableDeclaratorId)))

(def VariableDeclaratorId
  (conc Identifier (rep* (conc (lit "[") (lit "]")))))

(def VariableDeclarators
  (conc VariableDeclarator (rep* (conc (lit ",") VariableDeclarator))))

(def VariableDeclarator
  (conc Identifier VariableDeclaratorRest))

(def VariableDeclaratorRest
  (conc (rep* (conc (lit "[") (lit "]")))
        (opt (conc (lit "=") VariableInitializer))))

(def VariableInitializer
  (alt
    ArrayInitializer
    Expression))

(def ArrayInitializer
  (conc
    (lit "{")
    (rep* (conc VariableInitializer
                (rep* (conc (lit ",") VariableInitializer))
                (opt (lit ","))))
    (lit "}")))

;|||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||

(def Block
  (conc
    (lit "{")
    BlockStatements
    (lit "}")))

(def BlockStatement
  (alt
    LocalVariableDeclarationStatement
    ClassOrInterfaceDeclaration
    (conc (rep* (conc Identifier (lit ":"))) Statement)))

(def BlockStatements
  (rep* BlockStatement))

(def LocalVariableDeclarationStatement
  (conc
    (rep* VariableModifier) Type VariableDeclarators (lit ";")))

(def Statement
  (alt
    Block
    (lit ";")
    (conc Identifier (lit ":") Statement)
    (conc StatementExpression (lit ";"))
    (conc (lit "if") ParExpression Statement (opt (conc (lit "else") Statement)))
    (conc (lit "assert") Expression (opt (conc (lit ":") Expression)) (lit ";"))
    (conc (lit "switch") ParExpression (lit "{") SwitchBlockStatementGroups (lit "}"))
    (conc (lit "while") ParExpression Statement)
    (conc (lit "do") Statement (lit "while") ParExpression (lit ";"))
    (conc (lit "for") (lit "(") ForControl (lit ")") Statement)
    (conc (lit "break") (opt Identifier) (lit ";"))
    (conc (lit "continue") (opt Identifier) (lit ";"))
    (conc (lit "return") (opt Expression) (lit ";"))
    (conc (lit "throw") Expression (lit ";"))
    (conc (lit "synchronized") ParExpression Block)
    (conc (lit "try") Block (alt Catches (conc (opt Catches) Finally)))
    (conc (lit "try") ResourceSpecification Block (opt Catches) (opt Finally))))

(def StatementExpression
  (conc Expression))

;|||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||

(def Catches
  (conc CatchClause (lit "{") CatchClause (lit "}")))

(def CatchClause
  (conc
    (lit "catch")
    (lit "(")
    (rep* VariableModifier)
    CatchType
    Identifier
    (lit ")")
    Block))

(def CatchType
  (alt
    Identifier
    (conc (lit "|") Identifier)))

(def Finally
  (conc (lit "finally") Block))

(def ResourceSpecification
  (conc
    (lit "(")
    Resources
    (opt (lit ";"))
    (lit ")")))

(def Resources
  (conc
    Resource
    (rep*
      (conc (lit ";") Resource))))

(def Resource
  (conc
    (rep* VariableModifier)
    ReferenceType
    VariableDeclaratorId
    (lit "=")
    Expression))

;|||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||

(def SwitchBlockStatementGroups
  (conc
    (lit "{")
    SwitchBlockStatementGroup
    (lit "}")))

(def SwitchBlockStatementGroup
  (conc
    SwitchLabels BlockStatements))

(def SwitchLabel
  (alt
    (conc (lit "case") Expression (lit ":"))
    (conc (lit "case") EnumConstantName (lit ":"))
    (conc (lit "default") (lit ":"))))

(def SwitchLabels
  (rep+ SwitchLabel))

(def EnumConstantName
    Identifier)

(def ForControl
  (alt
    ForVarControl
    (conc ForInit (lit ";") (opt Expression) (lit ";") (opt ForUpdate))))

(def ForVarControl
  (conc
    (rep* VariableModifier) Type VariableDeclaratorId  ForVarControlRest))

(def ForVarControlRest
  (alt
    (conc ForVariableDeclaratorsRest (lit ";") (opt Expression) (lit ";") (opt ForUpdate))
    (conc (lit ":") Expression)))

(def ForVariableDeclaratorsRest
  (conc
    (opt (conc (lit "=") VariableInitializer))
    (rep* (conc (lit ",") VariableDeclarator))))

(def ForInit
  (conc
    StatementExpression
    (rep* (conc (lit ",") StatementExpression))))

(def ForUpdate
  (conc
    StatementExpression
    (rep* (conc (lit ",") StatementExpression))))

;|||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||
(def PrefixOp
  (alt
    (lit-conc-seq (seq "++"))
    (lit-conc-seq (seq "--"))
    (lit-conc-seq (seq "!"))
    (lit-conc-seq (seq "~"))
    (lit-conc-seq (seq "+"))
    (lit-conc-seq (seq "-"))))

(def PostfixOp
  (alt
    (lit-conc-seq (seq "++"))
    (lit-conc-seq (seq "--"))))

(def Expression
  (conc
    Expression1 (opt (conc AssignmentOperator Expression1))))

(def AssignmentOperator
  (alt
    (lit-conc-seq (seq "="))
    (lit-conc-seq (seq "+="))
    (lit-conc-seq (seq "-="))
    (lit-conc-seq (seq "*="))
    (lit-conc-seq (seq "/="))
    (lit-conc-seq (seq "&=")) 
    (lit-conc-seq (seq "|="))
    (lit-conc-seq (seq "^="))
    (lit-conc-seq (seq "%="))
    (lit-conc-seq (seq "<<="))
    (lit-conc-seq (seq ">>="))
    (lit-conc-seq (seq ">>>="))))

(def Expression1
  (conc
    Expression2 (opt Expression1Rest)))

(def Expression1Rest
  (conc
    (lit "?") Expression (lit ":") Expression1))

(def Expression2
  (conc
    Expression3 (opt Expression2Rest)))

(def Expression2Rest
  (alt
    (rep* InfixOp Expression3)
    (lit "instanceof") Type))

(def InfixOp
  (alt 
    (lit-conc-seq (seq "||"))
    (lit-conc-seq (seq "&&"))

    (lit-conc-seq (seq "&"))
    (lit-conc-seq (seq "=="))
    (lit-conc-seq (seq "!="))
    (lit-conc-seq (seq "<"))
    (lit-conc-seq (seq ">"))
    (lit-conc-seq (seq "<="))
    (lit-conc-seq (seq ">="))
    (lit-conc-seq (seq "<<"))
    (lit-conc-seq (seq ">>"))
    (lit-conc-seq (seq ">>>"))
    (lit-conc-seq (seq "+"))
    (lit-conc-seq (seq "-"))
    (lit-conc-seq (seq "*"))
    (lit-conc-seq (seq "/"))
    (lit-conc-seq (seq "%"))))

(def Expression3
  (alt
    (conc PrefixOp Expression3)
    (conc (alt Expression Type) Expression3)
    (conc Primary (rep* Selector) (rep* PostfixOp))))

(def Primary
  (alt
    Literal
    ParExpression
    (conc (lit "this") (opt Arguments))
    (conc (lit "super") SuperSuffix)
    (conc (lit "new") Creator)
    (conc NonWildcardTypeArguments (alt ExplicitGenericInvocationSuffix (conc (lit "this") Arguments )))
    (conc Identifier (rep* (conc (lit ".") Identifier)) (opt IdentifierSuffix))
    (conc BasicType (rep* (conc (lit "[") (lit "]"))) (lit ".") (lit "class"))
    (conc (lit "void") (lit ".") (lit "class"))))

(def ParExpression
  (conc
    (lit "(")
    Expression
    (lit ")")))

(def Arguments
  (conc
    (lit "(")
    (opt (conc  Expression (rep* (conc (lit ",") Expression))))))

(def SuperSuffix
  (alt
    Arguments
    (conc (lit ".") Identifier (opt Arguments))))

(def ExplicitGenericInvocationSuffix
  (alt
    (lit "super") SuperSuffix
    Identifier Arguments))

(def Creator
  (alt
    (conc NonWildcardTypeArguments CreatedName ClassCreatorRest)
    (conc CreatedName (alt ClassCreatorRest ArrayCreatorRest))))

(def CreatedName
  (conc
    Identifier (opt TypeArgumentsOrDiamond) (rep* (conc (lit ".") Identifier (opt TypeArgumentsOrDiamond)))))

(def ClassCreatorRest
  (conc Arguments (opt ClassBody)))

(def ArrayCreatorRest
  (conc (lit "[")
      (alt (conc (lit "]") (rep* (conc (lit "[") (lit "]"))) ArrayInitializer)
           (conc Expression (lit "]") (rep* (conc (lit "[")
                                                  Expression
                                                  (lit "]")))
                                      (rep* (conc (lit "[") (lit "]")))))))

(def IdentifierSuffix
  (alt
    (conc (lit "[") (alt (conc (rep* (conc (lit "[") (lit "]"))) (lit ".") (lit "class")) Expression) (lit "]"))
    Arguments
    (conc (lit ".")
          (alt
            (lit "class")
            ExplicitGenericInvocation
            (lit "this")
            (conc (lit "super") Arguments)
            (conc (lit "new") (opt NonWildcardTypeArguments) InnerCreator )))))

(def ExplicitGenericInvocation
  (conc
    NonWildcardTypeArguments ExplicitGenericInvocationSuffix))

(def InnerCreator
  (conc
    Identifier (opt NonWildcardTypeArgumentsOrDiamond) ClassCreatorRest))

(def Selector
  (alt
    (conc (lit ".") Identifier (opt Arguments))
    (conc (lit ".") ExplicitGenericInvocation)
    (conc (lit ".") (lit "this"))
    (conc (lit ".") (lit "super") SuperSuffix)
    (conc (lit ".") (lit "new") (opt NonWildcardTypeArguments) InnerCreator)
    (conc (lit "[") Expression (lit "]"))))

(def EnumBody
  (conc
    (lit "{")
    (opt EnumConstants)
    (opt (lit ","))
    (opt EnumBodyDeclarations)
    (lit "}")))

(def EnumConstants
  (alt
    EnumConstant
    (conc EnumConstants (lit ",") EnumConstant)))

(def EnumConstant
  (conc
    (opt Annotations) Identifier (opt Arguments) (opt ClassBody)))

(def EnumBodyDeclarations
  (conc
    (lit ";") (rep* ClassBodyDeclaration)))

(def AnnotationTypeBody
  (conc
    (lit "{")
    (opt AnnotationTypeElementDeclarations)
    (lit "}")))

(def AnnotationTypeElementDeclarations
  (alt
    AnnotationTypeElementDeclaration
    (conc AnnotationTypeElementDeclarations AnnotationTypeElementDeclaration)))

(def AnnotationTypeElementDeclaration
  (conc
    (rep* Modifier) AnnotationTypeElementRest))

(def AnnotationTypeElementRest
  (alt
    (conc Type Identifier AnnotationMethodOrConstantRest (lit ";"))
    ClassDeclaration
    InterfaceDeclaration
    EnumDeclaration
    AnnotationTypeDeclaration))

(def AnnotationMethodOrConstantRest
  (alt
    AnnotationMethodRest
    ConstantDeclaratorsRest))

(def AnnotationMethodRest
  (conc
    (lit "(") (lit ")") (opt (conc (lit "[") (lit "]")))
    (opt (conc (lit "default") ElementValue))))
