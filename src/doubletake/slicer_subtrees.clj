(ns doubletake.slicer-subtrees
  (:require [clojure.pprint])
  (:import  [org.eclipse.jdt.core.dom 
               AbstractTypeDeclaration
               Annotation
               AnnotationTypeDeclaration
               AnnotationTypeMemberDeclaration
               AnonymousClassDeclaration
               ArrayAccess
               ArrayCreation
               ArrayInitializer
               ArrayType
               AssertStatement
               Assignment
               Assignment
               Block
               BlockComment
               BodyDeclaration
               BooleanLiteral
               BreakStatement
               CastExpression
               CatchClause
               CharacterLiteral
               ChildListPropertyDescriptor
               ChildPropertyDescriptor
               ClassInstanceCreation
               Comment
               CompilationUnit
               ConditionalExpression
               ConstructorInvocation
               ContinueStatement
               DoStatement
               EmptyStatement
               EnhancedForStatement
               EnumConstantDeclaration
               EnumDeclaration
               Expression
               ExpressionStatement
               FieldAccess
               FieldDeclaration
               FileASTRequestor
               ForStatement
               IfStatement
               ImportDeclaration
               InfixExpression
               InfixExpression
               Initializer
               InstanceofExpression
               Javadoc
               LabeledStatement
               LineComment
               MarkerAnnotation
               MemberRef
               MemberValuePair
               Message
               MethodDeclaration
               MethodInvocation
               MethodRef
               MethodRefParameter
               Modifier
               Modifier
               Name
               NodeFinder
               NormalAnnotation
               NullLiteral
               NumberLiteral
               PackageDeclaration
               ParameterizedType
               ParenthesizedExpression
               PostfixExpression
               PostfixExpression
               PrefixExpression
               PrefixExpression
               PrimitiveType
               PrimitiveType
               QualifiedName
               QualifiedType
               ReturnStatement
               SimpleName
               SimplePropertyDescriptor
               SimpleType
               SingleMemberAnnotation
               SingleVariableDeclaration
               Statement
               StringLiteral
               StructuralPropertyDescriptor
               SuperConstructorInvocation
               SuperFieldAccess
               SuperMethodInvocation
               SwitchCase
               SwitchStatement
               SynchronizedStatement
               TagElement
               TextElement
               ThisExpression
               ThrowStatement
               TryStatement
               Type
               TypeDeclaration
               TypeDeclarationStatement
               TypeLiteral
               TypeParameter
               UnionType
               VariableDeclaration
               VariableDeclarationExpression
               VariableDeclarationFragment
               VariableDeclarationStatement
               WhileStatement
               WildcardType
              ]))

(defn alternates [& args]
  (concat '(:alt) args))

(defn sequence [& args]
  (concat '(:seq) args))

(defmulti subtrees 
  "About:
    The subtrees multimethod is a tool for retrieving the subtrees of any
    arbitrary ASTNode as they would be encountered during program execution.
  Arguments:
    An object which subclasses the Eclipse ASTNode
  Returns:
    An ordered, formatted list beginning either with :alt or :seq which is
    appropriate for feeding directly to doubletake.slicer/paths."
  class)

    (defmethod subtrees AbstractTypeDeclaration
      ; Parent class of TypeDeclaration
      ;                 EnumDeclaration
      ;                 AnnotationTypeDeclaration
      ;
      ; and should not be encountered in an AST.
      nil)

    (defmethod subtrees Annotation 
      ; Parent class of NormalAnnotation
      ;                 MarkerAnnotation
      ;                 SingleMemberAnnotation
      ;
      ; and should not be encountered in an AST.
      nil)

    (defmethod subtrees AnnotationTypeDeclaration 
      ; Used to declare custom annotatons. As annotations provide code metadata
      ; compile time warnings and runtime wrappers. They can add data to
      ; the runtime environment which is accessable via introspection but
      ; unlike Python annotations or Lisp wrappers it seems that they cannot
      ; define any executable code or other change of state.
      nil)

    (defmethod subtrees AnnotationTypeMemberDeclaration 
      ; As the AnnotationTypeDeclaration has null subtrees this should be
      ; unreachable but null anyway.
      nil)

    (defmethod subtrees AnonymousClassDeclaration 
      ; Because of all the strange and evil things one can do with anon.
      ; classes, DeWayne OK'd me to just ignore them alltogether. If you do
      ; stupid and dangerous things, we are not bound to help you.
      nil)

    (defmethod subtrees ArrayAccess nil)
    (defmethod subtrees ArrayCreation nil)
    (defmethod subtrees ArrayInitializer nil)
    (defmethod subtrees ArrayType nil)
    (defmethod subtrees AssertStatement nil)
    (defmethod subtrees Assignment nil)
    (defmethod subtrees Assignment nil)
    (defmethod subtrees Block nil)
    (defmethod subtrees BlockComment nil)
    (defmethod subtrees BodyDeclaration nil)
    (defmethod subtrees BooleanLiteral nil)
    (defmethod subtrees BreakStatement nil)
    (defmethod subtrees CastExpression nil)
    (defmethod subtrees CatchClause nil)
    (defmethod subtrees CharacterLiteral nil)
    (defmethod subtrees ChildListPropertyDescriptor nil)
    (defmethod subtrees ChildPropertyDescriptor nil)
    (defmethod subtrees ClassInstanceCreation nil)
    (defmethod subtrees Comment nil)
    (defmethod subtrees CompilationUnit nil)
    (defmethod subtrees ConditionalExpression nil)
    (defmethod subtrees ConstructorInvocation nil)
    (defmethod subtrees ContinueStatement nil)
    (defmethod subtrees DoStatement nil)
    (defmethod subtrees EmptyStatement nil)
    (defmethod subtrees EnhancedForStatement nil)
    (defmethod subtrees EnumConstantDeclaration nil)
    (defmethod subtrees EnumDeclaration nil)
    (defmethod subtrees Expression nil)
    (defmethod subtrees ExpressionStatement nil)
    (defmethod subtrees FieldAccess nil)
    (defmethod subtrees FieldDeclaration nil)
    (defmethod subtrees FileASTRequestor nil)
    (defmethod subtrees ForStatement nil)
    (defmethod subtrees IfStatement nil)
    (defmethod subtrees ImportDeclaration nil)
    (defmethod subtrees InfixExpression nil)
    (defmethod subtrees InfixExpression nil)
    (defmethod subtrees Initializer nil)
    (defmethod subtrees InstanceofExpression nil)
    (defmethod subtrees Javadoc nil)
    (defmethod subtrees LabeledStatement nil)
    (defmethod subtrees LineComment nil)
    (defmethod subtrees MarkerAnnotation nil)
    (defmethod subtrees MemberRef nil)
    (defmethod subtrees MemberValuePair nil)
    (defmethod subtrees Message nil)
    (defmethod subtrees MethodDeclaration nil)
    (defmethod subtrees MethodInvocation nil)
    (defmethod subtrees MethodRef nil)
    (defmethod subtrees MethodRefParameter nil)
    (defmethod subtrees Modifier nil)
    (defmethod subtrees Modifier nil)
    (defmethod subtrees Name nil)
    (defmethod subtrees NodeFinder nil)
    (defmethod subtrees NormalAnnotation nil)
    (defmethod subtrees NullLiteral nil)
    (defmethod subtrees NumberLiteral nil)
    (defmethod subtrees PackageDeclaration nil)
    (defmethod subtrees ParameterizedType nil)
    (defmethod subtrees ParenthesizedExpression nil)
    (defmethod subtrees PostfixExpression nil)
    (defmethod subtrees PostfixExpression nil)
    (defmethod subtrees PrefixExpression nil)
    (defmethod subtrees PrefixExpression nil)
    (defmethod subtrees PrimitiveType nil)
    (defmethod subtrees PrimitiveType nil)
    (defmethod subtrees QualifiedName nil)
    (defmethod subtrees QualifiedType nil)
    (defmethod subtrees ReturnStatement nil)
    (defmethod subtrees SimpleName nil)
    (defmethod subtrees SimplePropertyDescriptor nil)
    (defmethod subtrees SimpleType nil)
    (defmethod subtrees SingleMemberAnnotation nil)
    (defmethod subtrees SingleVariableDeclaration nil)
    (defmethod subtrees Statement nil)
    (defmethod subtrees StringLiteral nil)
    (defmethod subtrees StructuralPropertyDescriptor nil)
    (defmethod subtrees SuperConstructorInvocation nil)
    (defmethod subtrees SuperFieldAccess nil)
    (defmethod subtrees SuperMethodInvocation nil)
    (defmethod subtrees SwitchCase nil)
    (defmethod subtrees SwitchStatement nil)
    (defmethod subtrees SynchronizedStatement nil)
    (defmethod subtrees TagElement nil)
    (defmethod subtrees TextElement nil)
    (defmethod subtrees ThisExpression nil)
    (defmethod subtrees ThrowStatement nil)
    (defmethod subtrees TryStatement nil)
    (defmethod subtrees Type nil)
    (defmethod subtrees TypeDeclaration nil)
    (defmethod subtrees TypeDeclarationStatement nil)
    (defmethod subtrees TypeLiteral nil)
    (defmethod subtrees TypeParameter nil)
    (defmethod subtrees UnionType nil)
    (defmethod subtrees VariableDeclaration nil)
    (defmethod subtrees VariableDeclarationExpression nil)
    (defmethod subtrees VariableDeclarationFragment nil)
    (defmethod subtrees VariableDeclarationStatement nil)
    (defmethod subtrees WhileStatement nil)
    (defmethod subtrees WildcardType nil)
