(ns doubletake.slicer-subtrees
  (:require [clojure.pprint])
  (:import [org.eclipse.jdt.core ICompilationUnit]
           [org.eclipse.jdt.core.dom 
              AST 
              ASTParser 
              CompilationUnit]
           
           [org.eclipse.jdt.core.dom 
            ; things supported with type dispatch
              AnonymousClassDeclaration
              CatchClause
              Modifier
              Comment
              Type

            ; things which need type dispatch support
              TypeParameter VariableDeclaration
              
              ; WARNING - these are SUBCLASSES of BodyDeclaration and must take prio
              EnumConstantDeclaration FieldDeclaration Initializer 
              MethodDeclaration 
              
              ; WARNING - these are SUBCLASSES of Statement and must take prio
              AssertStatement Block BreakStatement ConstructorInvocation 
              ContinueStatement DoStatement EmptyStatement EnhancedForStatement
              ExpressionStatement ForStatement IfStatement LabeledStatement
              ReturnStatement SuperConstructorInvocation SwitchCase
              SwitchStatement SynchronizedStatement ThrowStatement TryStatement
              TypeDeclarationStatement VariableDeclarationStatement
              WhileStatement
            ]))



; The high-level sketch for these routines is to, given an AST node which could
; could have been the victim of a prior diff calculation, calculate first a
; list of all possible execution paths through the code represented by the
; given AST
;
; The second step, and a seperate function, is to filter for only those slice 
; sequences which contain a node from a list of nodes. This will be used in
; conjunction with the diff tool to select only slices where a node in the 
; slice has changed.

(defn has-label? [break-statement]
  (nil? (.getBodyDeclarationsPrtoperty break-statement
                                       (.LABEL_PROPERTY break-statement))))

(defmulti subtrees 
 "About:
    A class-type dispatched method for taking the subtrees of an ASTNode 
    subclass.
  Arguments:
    An object which sublcasses the Eclipse ASTNode object
  Returns:
    A formatted list such that each element of the list is a full subtree of
    the ASTNode argument. No gurantees are made as to the order of the trees
    only that all subtrees present in the original node will appear in the
    resulting list.

    In the case of atomic subtrees such as Block types, they shall be contained
    in a list of length 1 within the returned list so that the recursive case
    first down the subtrees and then across is not disturbed.

    If the returned list contains K sublists, each sublist shall be the list
    of child nodes which can be reached by a distinct control case. Such lists
    may be empty."    
  class)

  ; dispatch by type...
(defmethod subtrees AnnotationTypeDeclaration [v] 
  ; BODY_DECLARATIONS_PROPERTY  -> BodyDeclaration list
  ; JAVADOC_PROPERTY            -> JavaDoc singleton
  ; MODIFIERS2_PROPERTY         -> Modifiers list
  ; NAME_PROPERTY               -> SimpleName
  (list (.getBodyDeclarationsPrtoperty v (.BODY_DECLARATIONS_PROPERTY  v))))

(defmethod subtrees AnonymousClassDeclaration [v]
  ; BODY_DECLARATIONS_PROPERTY -> BodyDeclaration list
  ;
  ; While all that is sane demands that I not support nested classes I kinda
  ; have to so this is in fact a recursive case, and requires that 
  ; BodyDeclaration also be a recursive case.
  (list))

(defmethod subtrees ArrayAccess [v] 
  ; ARRAY_PROPERTY  -> Expression
  ; INDEX_PROPERTY  -> Expression
  ; #terminal
  (list))

(defmethod subtrees ArrayCreation [v]
  ; DIMENSIONS_PROPERTY  -> Expression
  ; INITIALIZER_PROPERTY -> ArrayInitializer
  ; TYPE_PROPERTY        -> ArrayType
  ; #terminal
  ; #assignment 
  (list))

(defmethod subtrees ArrayInitializer [v] 
  ; EXPRESSIONS_PROPERTY -> Expression list
  ; #terminal
  ; #assignment
  (list))

(defmethod subtrees ArrayType [v] 
  ; COMPONENT_TYPE_PROPERTY  -> Type
  ; #terminal
  (list))

(defmethod subtrees AssertStatement [v] 
  ; EXPRESSION_PROPERTY -> Expression
  ; MESSAGE_PROPERTY    -> Expression 
  ; #terminal
  (list))

(defmethod subtrees Assignment [v] 
  ; LEFT_HAND_SIDE_PROPERTY  -> Expression
  ; OPERATOR_PROPERTY        -> Assignment.Operator
  ; RIGHT_HAND_SIDE_PROPERTY -> Expression
  ; #assignment
  ; #terminal?
  (list))

(defmethod subtrees Block [v] 
  ; STATEMENTS_PROPERTY  -> Statement list
  ; non-terminal node, definitely a recursion case
  (list (.getBodyDeclarationsPrtoperty v (.STATEMENTS_PROPERTY v))))

(defmethod subtrees BooleanLiteral [v] (list))
(defmethod subtrees BreakStatement [v] (list))
(defmethod subtrees CastExpression [v] (list))

(defmethod subtrees CatchClause [v] 
  ; BODY_PROPERTY      -> Block
  ; EXCEPTION_PROPERTY -> SingleVariableDeclaration
  (list))

(defmethod subtrees CharacterLiteral [v] (list))

(defmethod subtrees ClassInstanceCreation [v]
  ; ANONYMOUS_CLASS_DECLARATION_PROPERTY -> AnonymousClassDeclaration
  ; ARGUMENTS_PROPERTY                   -> Expression list
  ; EXPRESSION_PROPERTY                  -> Expression
  ; NAME_PROPERTY                        -> Name
  ; TYPE_ARGUMENTS_PROPERTY              -> Type list
  ; TYPE_PROPERTY                        -> Type
  ;
  ; For all that I would love to make this a terminal case the unfortunate
  ; truth is that it has to be a recursive case if this slicer is to support
  ; nested objects.
  (list (list (.getBodyDeclarationsPrtoperty v (. v ANONYMOUS_CLASS_DECLARATION_PROPERTY)))))

(defmethod subtrees CompilationUnit [v]
  ; IMPORTS_PROPERTY -> ImportDeclaration list
  ; PACKAGE_PROPERTY -> PackageDeclaration
  ; TYPES_PROPERTY   -> AbstractTypeDeclaration list
  ;
  ; This shouldn't be a node which this subtree system ever encounters, but
  ; just in case this is a recursion case down into TYPES_PROPERTY
  (list (.getBodyDeclarationsPrtoperty v (. v TYPES_PROPERTY))))

(defmethod subtrees ConditionalExpression [v] 
  ; EXPRESSION_PROPERTY      -> Expression 
  ; THEN_EXPRESSION_PROPERTY -> Expression
  ; ELSE_EXPRESSION_PROPERTY -> Expression
  ;
  ; Not sure what to do with this node, as it represents both a control
  ; structure and usually an inline argument  manipulaion. As a result I can't
  ; directly slice on the paths "through" this node like I can elsewhere.
  ;
  ; Also not sure if I want to go down the arguments of this statement or not
  ; because once again Expression is unclear.
  ;
  ; Emailed DeWayne about this and we'll see what he says to make of this
  ; 
  ; Update: DeWayne came back and said consider it to be a terminal for slicing
  ; and don't worry about expanding it into the equivalent if structure.
  (list))

(defmethod subtrees ConstructorInvocation [v] (list))
(defmethod subtrees ContinueStatement [v] (list))

(defmethod subtrees DoStatement [v]
  ; BODY_PROPERTY        -> Statement
  ; EXPRESSION_PROPERTY  -> Expression
  ;
  ; As this is a looping conditional there are two execution cases:
  ;   Actually there is only one since the Statement is executed before the
  ;   contidional and the conditional will evaluate at least once rendering
  ;   the backedge case equivalent to the forward edge case, so only one case
  (list (list (.getBodyDeclarationsPrtoperty v (. g BODY_PROPERTY)))))

(defmethod subtrees EmptyStatement [v] (list))

(defmethod subtrees EnhancedForStatement [v]
  ; BODY_PROPERTY       -> Statement (Block with p > 90%)
  ; EXPRESSION_PROPERTY -> Expression 
  ; PARAMETER_PROPERTY  -> SingleVariableDeclaration
  (list (list (.getBodyDeclarationsPrtoperty v (. v PARAMETER_PROPERTY))
              (.getBodyDeclarationsPrtoperty v (. v BODY_PROPERTY)))))

(defmethod subtrees EnumConstantDeclaration [v] (list))
(defmethod subtrees EnumDeclaration [v] (list))
(defmethod subtrees ExpressionStatement [v] (list))
(defmethod subtrees FieldAccess [v] (list))

(defmethod subtrees ForStatement [v]
  ; BODY_PROPERTY         -> Statement
  ; EXPRESSION_PROPERTY   -> Expression
  ; INITIALIZERS_PROPERTY -> Expression list
  ; UPDATERS_PROPERTY     -> Expression list
  ;
  ; This node has three cases
  ; - the "null" edge
  ; - the "do once" edge
  ; - the "do [n]" edge
  (list
    (list) ; null case
    (list  ; do-once case
      (.getBodyDeclarationsPrtoperty v (. v INITIALIZERS_PROPERTY))
      (.getBodyDeclarationsPrtoperty v (. v BODY_PROPERTY)))
    (list  ; the do more than once edge
      (.getBodyDeclarationsPrtoperty v (. v INITIALIZERS_PROPERTY))
      (.getBodyDeclarationsPrtoperty v (. v UPDATERS_PROPERTY))
      (.getBodyDeclarationsPrtoperty v (. v BODY_PROPERTY)))))

(defmethod subtrees IfStatement [v]
  ; EXPRESSION_PROPERTY     -> Expression
  ; THEN_STATEMENT_PROPERTY -> Statement
  ; ELSE_STATEMENT_PROPERTY -> Statement
  (list
    (list ; true case...
      (.getBodyDeclarationsPrtoperty v (. v EXPRESSION_PROPERTY))
      (.getBodyDeclarationsPrtoperty v (. v THEN_EXPRESSION_PROPERTY)))

    (list ; false case... may be null
      (.getBodyDeclarationsPrtoperty v (. v EXPRESSION_PROPERTY))
      (.getBodyDeclarationsPrtoperty v (. v ELSE_EXPRESSION_PROPERTY)))))

(defmethod subtrees ImportDeclaration [v] (list))
(defmethod subtrees InfixExpression [v] (list))
(defmethod subtrees Initializer [v] (list))
(defmethod subtrees InstanceofExpression [v] (list))
(defmethod subtrees Javadoc [v] (list))

(defmethod subtrees LabeledStatement [v] 
  ; This is going to be a nontrivial case because I have to support breaks up
  (list))

(defmethod subtrees LineComment [v] (list))
(defmethod subtrees MarkerAnnotation [v] (list))
(defmethod subtrees MemberRef [v] (list))
(defmethod subtrees MemberValuePair [v] (list))
(defmethod subtrees Message [v] (list))
(defmethod subtrees MethodDeclaration [v] (list))
(defmethod subtrees MethodInvocation [v] (list))
(defmethod subtrees MethodRef [v] (list))
(defmethod subtrees MethodRefParameter [v] (list))
(defmethod subtrees Modifier [v] (list))
(defmethod subtrees Modifier.ModifierKeyword [v] (list))
(defmethod subtrees Name [v] (list))
(defmethod subtrees NodeFinder [v] (list))
(defmethod subtrees NormalAnnotation [v] (list))
(defmethod subtrees NullLiteral [v] (list))
(defmethod subtrees NumberLiteral [v] (list))
(defmethod subtrees PackageDeclaration [v] (list))
(defmethod subtrees ParameterizedType [v] (list))
(defmethod subtrees ParenthesizedExpression [v] (list))
(defmethod subtrees PostfixExpression [v] (list))
(defmethod subtrees PostfixExpression.Operator [v] (list))
(defmethod subtrees PrefixExpression [v] (list))
(defmethod subtrees PrimitiveType [v] (list))
(defmethod subtrees PrimitiveType.Code [v] (list))
(defmethod subtrees QualifiedName [v] (list))
(defmethod subtrees QualifiedType [v] (list))
(defmethod subtrees ReturnStatement [v] (list))
(defmethod subtrees SimpleName [v] (list))
(defmethod subtrees SimplePropertyDescriptor [v] (list))
(defmethod subtrees SimpleType [v] (list))
(defmethod subtrees SingleMemberAnnotation [v] (list))
(defmethod subtrees SingleVariableDeclaration [v] (list))

(defmethod subtrees Statement [v] (list))

(defmethod subtrees StringLiteral [v] (list))
(defmethod subtrees StructuralPropertyDescriptor [v] (list))
(defmethod subtrees SuperConstructorInvocation [v] (list))
(defmethod subtrees SuperFieldAccess [v] (list))
(defmethod subtrees SuperMethodInvocation [v] (list))

(defmethod subtrees SwitchCase [v] (list))
(defmethod subtrees SwitchStatement [v] (list))

(defmethod subtrees SynchronizedStatement [v] (list))
(defmethod subtrees TagElement [v] (list))
(defmethod subtrees TextElement [v] (list))
(defmethod subtrees ThisExpression [v] (list))
(defmethod subtrees ThrowStatement [v] (list))
(defmethod subtrees TryStatement [v] (list))
(defmethod subtrees Type [v] (list))
(defmethod subtrees TypeDeclaration [v] (list))
(defmethod subtrees TypeDeclarationStatement [v] (list))
(defmethod subtrees TypeLiteral [v] (list))
(defmethod subtrees TypeParameter [v] (list))
(defmethod subtrees UnionType [v] (list))
(defmethod subtrees VariableDeclaration [v] (list))
(defmethod subtrees VariableDeclarationExpression [v] (list))
(defmethod subtrees VariableDeclarationFragment [v] (list))
(defmethod subtrees VariableDeclarationStatement [v] (list))
(defmethod subtrees WhileStatement [v] (list))
(defmethod subtrees WildcardType [v] (list))
  
