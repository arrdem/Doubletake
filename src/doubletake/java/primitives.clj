(ns doubletake.java.primitives
  (:use 
    ; stdlib
    clojure.set
    ; third party
    name.choi.joshua.fnparse))

; This file details an fnparse parser for Java source code which couls be able
; to process Java 1.7 and below source code.
; The format for each rule is as follows
;
;(def rule-name
;  ; bnf definition
;  *expr)
;
; Where they are not compatible with Cojure due to namespace conflicts, BNF rule
; names shall be prefixed with "java-".
;
; The Java language implemented here is derived from the Java Language Spec.
; http://docs.oracle.com/javase/specs/jls/se7/html/index.html

;------------------------------------------------------------------------------
;                             3.3 Unicode Escapes
;------------------------------------------------------------------------------
(def HexDigit
  ; HexDigit: one of
  ;  0 1 2 3 4 5 6 7 8 9 a b c d e f A B C D E F
  (term #(re-matches #"[0-9a-fA-F]" %)))

(def NonZeroDigit
  (term #(re-matches #"[1-9]" %)))

(def Digit
  (term #(re-matches #"[0-9]" %)))

(def OctalDigit
  (term #(re-matches #"[0-7]" %)))

(def ZeroToThree
  (term #(re-matches #"[0-3]" %)))

(def Sub
  ; the ASCII SUB character, also known as "control-z"
  (lit (char 26)))

(def RawInputCharacter
  ; any input character
  (term #(re-matches #"[^\n\r]" %)))

(def UnicodeMarker
  ; UnicodeMarker:
  ;    u
  ;    UnicodeMarker u
  (alt
    (conc (lit "u") UnicodeMarker)
    (lit "u")))

(def UnicodeEscape
  ; UnicodeEscape:
  ;    \ UnicodeMarker HexDigit HexDigit HexDigit HexDigit
  (conc
    (lit "\\")
    UnicodeMarker
    HexDigit
    HexDigit
    HexDigit
    HexDigit))

(def UnicodeInputcharacter
  ;UnicodeInputCharacter:
  ;    UnicodeEscape
  ;    RawInputCharacter
  (alt
    UnicodeEscape
    RawInputCharacter))

;------------------------------------------------------------------------------
;                           3.10.6 Escape Sequences
;------------------------------------------------------------------------------
(def OctalEscape
  ; OctalEscape:
  ;    \ OctalDigit
  ;    \ OctalDigit OctalDigit
  ;    \ ZeroToThree OctalDigit OctalDigit
  (conc (lit "\\")
        (alt (conc ZeroToThree OctalDigit OctalDigit)
             (conc OctalDigit OctalDigit)
             OctalDigit)))

(def EscapeSequence
  ; EscapeSequence:
  ;    \ b    /* \u0008: backspace BS */
  ;    \ t    /* \u0009: horizontal tab HT */
  ;    \ n    /* \u000a: linefeed LF */
  ;    \ f    /* \u000c: form feed FF */
  ;    \ r    /* \u000d: carriage return CR */
  ;    \ "    /* \u0022: double quote " */
  ;    \ '    /* \u0027: single quote ' */
  ;    \ \              /* \u005c: backslash \ */
  ;    OctalEscape        /* \u0000 to \u00ff: from octal value */
  (alt
    (conc
      (lit "\\")
      (alt
        (lit "b") (lit "t") (lit "n") (lit "f") (lit "r") (lit "\"") (lit "'") (lit "\\")))
    OctalEscape))
;------------------------------------------------------------------------------
;                           3.4 Line Terminators
;------------------------------------------------------------------------------
(def LineTerminator
  ; LineTerminator:
  ;    the ASCII LF character, also known as "newline"
  ;    the ASCII CR character, also known as "return"
  ;    the ASCII CR character followed by the ASCII LF character
  (alt
    (conc (lit "\n") (lit "\r"))
    (lit "\n")
    (lit "\r")))

(def InputCharacter
  ; InputCharacter:
  ;    UnicodeInputCharacter but not CR or LF
  UnicodeInputCharacter)

;------------------------------------------------------------------------------
;                           3.6 White Space
;------------------------------------------------------------------------------
(def WhiteSpace
  ; WhiteSpace:
  ;    the ASCII SP character, also known as "space"
  ;    the ASCII HT character, also known as "horizontal tab"
  ;    the ASCII FF character, also known as "form feed"
  ;    LineTerminator
  (alt
    (lit " ")
    (lit "\t")
    (lit (char 12))
    LineTerminator))

;------------------------------------------------------------------------------
;                           3.7 Comments
;------------------------------------------------------------------------------
(def CharactersInLine
  ; CharactersInLine:
  ;    InputCharacter
  ;    CharactersInLine InputCharacter
  ;
  ; @EDIT
  ;    I changed this definition to ensure that CharactersInLine will __not__
  ;    consume characters past the end of line. This 0 or more greedy repetition
  ;    should ensure that the first occuring newline is the last character
  ;    consumed.
  (conc
    (rep* InputCharacter)
    LineTerminator))

(def NotStarNotSlash
  ; NotStarNotSlash:
  ;    InputCharacter but not * or /
  ;    LineTerminator
  (alt
    (term #(re-matches #"[^/\*]" %))
    LineTerminator))

(def NotStar
  ; NotStar:
  ;    InputCharacter but not *
  ;    LineTerminator
  (alt
    (term #(re-matches #"[^\*]" %))
    LineTerminator))

(def CommentTail)

(def CommentTailStar
  ; CommentTailStar:
  ;    /
  ;    * CommentTailStar
  ;    NotStarNotSlash CommentTail
  (alt
    (lit "/")
    (conc (lit "*") CommentTailStar)
    (conc NotStarSlash CommentTail)))

(def CommentTail
  ; CommentTail:
  ;    * CommentTailStar
  ;    NotStar CommentTail
  (alt
    (conc (lit "*") CommentTailStar)
    (conc NotStar CommentTail)))

(def EndOfLineComment
  ; EndOfLineComment:
  ;    / / CharactersInLine(opt)
  (conc
    (lit "/")
    (lit "/")
    CharactersInLine))

(def TraditionalComment
  ; TraditionalComment:
  ;    / * CommentTail
  (conc
    (lit "/")
    (lit "*")
    CommentTail))

(def Comment
  ; Comment:
  ;    TraditionalComment
  ;    EndOfLineComment
  (alt
    TraditionalComment
    EndOfLineComment))

;------------------------------------------------------------------------------
;                           3.9 Keywords
;------------------------------------------------------------------------------
(def Keyword
  ; Keyword: one of
  ;    abstract   continue   for          new         switch
  ;    assert     default    if           package     synchronized
  ;    boolean    do         goto         private     this
  ;    break      double     implements   protected   throw
  ;    byte       else       import       public      throws
  ;    case       enum       instanceof   return      transient
  ;    catch      extends    int          short       try
  ;    char       final      interface    static      void
  ;    class      finally    long         strictfp    volatile
  ;    const      float      native       super       while
  (appy alt
    (map lit-conc-seq (map seq 
                           ["abstract" "continue" "for" "new" "switch" 
                            "assert" "default" "if" "package" "synchronized"
                            "boolean" "do" "goto" "private" "this" "break"
                            "double" "implements" "protected" "throw" "byte"
                            "else" "import" "public" "throws" "case" "enum"
                            "instanceof" "return" "transient" "catch" "extends"
                            "int" "short" "try" "char" "final" "interface"
                            "static" "void" "class" "finally" "long" "strictfp"
                            "volatile" "const" "float" "native" "super" "while"
                            ]))))

;------------------------------------------------------------------------------
;                           3.9 Literals
;------------------------------------------------------------------------------
(def IntegerTypeSuffix
  ; IntegerTypeSuffix: one of
  ;    l L
  (alt
    (lit "l")
    (lit "L")))

;;;; Decimal Integer Literals
;;;;---------------------------------------------------------------------------

(def Digits
  (rep+ Digit))

(def DecimalNumeral
  ; DecimalNumeral:
  ;    0
  ;    NonZeroDigit Digit*
  ;    NonZeroDigit "_"* Digit*
  (alt
    (lit "0")
    (conc
      NonZeroDigit
      (rep* Digit))
    (conc
      NonZeroDigit
      (rep* (lit "_"))
      (rep* Digit))))

(def DecimalIntegerLiteral
  ; DecimalIntegerLiteral:
  ;    DecimalNumeral IntegerTypeSuffix?
  (conc
    DecimalNumeral
    (factor< 1 IntegerTypeSufifix)))

;;;; Binary Literals
;;;;---------------------------------------------------------------------------
(def BinaryNumeral
  ; BinaryNumeral:
  ;    0 b BinaryDigits 
  ;    0 B BinaryDigits
  ; BinaryDigits:
  ;    (0|\1|_)*
  (conc
    (lit "0")
    (alt (lit "b") (lit "B"))
    (rep+ (alt (lit "0") (lit "1") (lit "_")))))

(def BinaryIntegerLiteral
  ; BinaryIntegerLiteral:
  ;    BinaryNumeral IntegerTypeSuffix
  (conc
    BinaryNumeral
    (factor< 1  IntegerTypeSuffix)))

;;;; Octal Literals
;;;;---------------------------------------------------------------------------
(def OctalNumeral
  ; OctalNumeral:
  ;    0 OctalDigits
  ;    0 Underscores OctalDigits
  (conc
    (lit "0")
    (rep* (lit "_")) 
    (rep* OctalDigit)))

(def OctalIntegerLiteral
  ; OctalIntegerLiteral:	
  ;    OctalNumeral IntegerTypeSuffix?
  (conc
    OctalNumeral
    (factor< 1  IntegerTypeSuffix)))

;;;; Hex Literals
;;;;---------------------------------------------------------------------------
(def HexNumeral
  ; HexNumeral:
  ;    0 x HexDigits
  ;    0 X HexDigits
  (conc
    (lit "0")
    (lit "x")
    (rep+ 
      (alt
        (lit "_")
        HexDigit))))

(def HexIntegerLiteral
  ; HexIntegerLiteral:
  ;    HexNumeral IntegerTypeSuffix?
  (conc
    HexNumeral
    (factor< 1  IntegerTypeSuffix)))

;;;; Integer Literal
;;;;---------------------------------------------------------------------------
(def IntegerLiteral
  (alt
    DecimalIntegerLiteral
    HexIntegerLiteral
    OctalIntegerLiteral
    BinaryIntegerLiteral))

(def SignedInteger
  ; SignedInteger:
  ;    Sign? Digits
  (conc
    (factor= 1 (alt (lit "-") (lit "+"))) 
    Digits))

;;;; Floating Point Literals
;;;;---------------------------------------------------------------------------

(def FloatTypeSuffix
  ; FloatTypeSuffix: one of
  ;    f F d D
  (alt
    (lit "f")
    (lit "F")
    (lit "d")
    (lit "D")))

;;;;;;;; Decimal Floating Point Literals
;;;;;;;;-----------------------------------------------------------------------

(def ExponentPart
  ; ExponentPart:
  ;    ExponentIndicator SignedInteger
  (conc (alt (lit "e") (lit "E")) SignedInteger))

(def DecimalFloatingPointLiteral
  ; DecimalFloatingPointLiteral:
  ;    Digits . Digits? ExponentPart? FloatTypeSuffix?
  ;    . Digits ExponentPart? FloatTypeSuffix?
  ;    Digits ExponentPart FloatTypeSuffix?
  ;    Digits ExponentPart? FloatTypeSuffix
  (alt
    (conc Digits (lit ".") (factor= 1 Digits) (factor= 1 ExponentPart) (factor= 1 FloatTypeSuffix))
    (conc (lit ".") Digits (factor= 1 ExponentPart) (factor= 1 FloatTypeSuffix))
    (conc Digits ExponentPart (factor= 1 FloatTypeSuffix))
    (conc Digits (factor= 1 ExponentPart) (factor= 1 FloatTypeSuffix))))

;;;;;;;; Hex Floating Point Literals
;;;;;;;;-----------------------------------------------------------------------
(def HexDigits
  (rep+ HexDigit))

(def HexSignificand
  ; HexSignificand:
  ;    HexNumeral
  ;    HexNumeral .
  ;    0 x HexDigits? . HexDigits
  ;    0 X HexDigits? . HexDigits
  (alt
    HexNumeral
    (conc HexNumeral (lit "."))
    (conc (lit "0") 
          (alt (lit "x") 
               (lit "X")) 
          (rep* HexDigit) 
          (lit ".") 
          HexDigits)))

(def BinaryExponentIndicator
  ; BinaryExponentIndicator:one of
  ;    p P
  (alt (lit "p") (lit "P")))

(def BinaryExponent
  ; BinaryExponent:
  ;    BinaryExponentIndicator SignedInteger
  (conc
    BinaryExponentIndicator SignedInteger))

(def HexadecimalFloatingPointLiteral
  ; HexadecimalFloatingPointLiteral:
  ;    HexSignificand BinaryExponent FloatTypeSuffix?
  (conc
    HexSignificand
    BinaryExponent
    (factor= 1 FloatTypeSuffix)))

;;;;;;;; Genberal Floating Point Literal
;;;;;;;;-----------------------------------------------------------------------
(def FloatingPointLiteral
  ; FloatingPointLiteral:
  ;    DecimalFloatingPointLiteral
  ;    HexadecimalFloatingPointLiteral
  (alt DecimalFloatingPointLiteral
       HexadecimalFloatingPointLiteral))

;;;; Boolean Literal
;;;;---------------------------------------------------------------------------
(def BooleanLiteral
  ; one of "true", "false"
  (alt
    (conc (lit "t") (lit "r") (lit "u") (lit "e"))
    (conc (lit "f") (lit "a") (lit "s") (lit "e"))))

;;;; Character Literal
;;;;---------------------------------------------------------------------------

(def SingleCharacter
  ; SingleCharacter:
  ;    InputCharacter but not ' or \
  (term #(re-matches #"[^\\']" %)))

(def CharacterLiteral
  ; CharacterLiteral:
  ;    ' SingleCharacter '
  ;    ' EscapeSequence '
  (conc (lit "'")
        (alt
          SingleCharacter
          EscapeSequence)
        (lit "'")))

;;;; String Literal
;;;;---------------------------------------------------------------------------
(def StringCharacter
  ; StringCharacter:
  ;    InputCharacter but not " or \
  ;    EscapeSequence
  (alt
    (term #(re-matches #"[\"\\]"))
    EscapeSequence))

(def StringCharacters
  ; StringCharacters:
  ;    StringCharacter
  ;    StringCharacters StringCharacter
  (rep+ StringCharacter))

(def StringLiteral
  ; StringLiteral:
  ;    " StringCharactersopt "
  (conc
    (lit "\"")
    (factor= 1 StringCharacters)
    (lit "\""))) 

;;;; Null Literal
;;;;---------------------------------------------------------------------------

(def NullLiteral
  ; NullLiteral:
  ;    null
  (lit-conc-seq ["n" "u" "l" "l"]))

(def Literal
  ; Literal:
  ;     IntegerLiteral
  ;     FloatingPointLiteral
  ;     BooleanLiteral
  ;     CharacterLiteral
  ;     StringLiteral
  ;     NullLiteral
  (alt
    IntegerLiteral
    FloatingPointLiteral
    BooleanLiteral
    CharacterLiteral
    StringLiteral
    NullLiteral))

;------------------------------------------------------------------------------
;                           3.8 Identifiers
;------------------------------------------------------------------------------
(def Identifier
  ; An identifier is an unlimited-length sequence of Java letters and Java 
  ; digits, the first of which must be a Java letter.
  ;
  ; Excludes all keywords and the Boolean/Null literals
  (except
    (conc
      (term #(re-matches #"[a-zA-Z$_]" %))
      (rep* (term #(re-matches #"[a-zA-Z0-9$_]" %))))
    (alt
      Keyword
      BooleanLiteral
      NullLiteral)))

;------------------------------------------------------------------------------
;                           3.11 Separators
;------------------------------------------------------------------------------
(def Seperators
  ; Separator: one of
  ;    (    )    {    }    [    ]    ;    ,    .
  (lit-alt-seq ["(" ")" "[" "]" "{" "}" "," "." ";"]))

;------------------------------------------------------------------------------
;                           3.12 Tokens
;------------------------------------------------------------------------------
(def Operator
  ; Operator: one of
  ;    =   >   <   !   ~   ?   :
  ;    ==  <=  >=  !=  &&  ||  ++  --
  ;    +   -   *   /   &   |   ^   %   <<   >>   >>>
  ;    +=  -=  *=  /=  &=  |=  ^=  %=  <<=  >>=  >>>=
  (alt
    (map lit-conc-seq 
         (map seq ["=" ">" "<" "!" "~" "?" ":" "==" "<=" ">=" "!=" "&&" "||"
                   "++" "--" "+" "-" "*" "/" "&" "|" "^" "%" "<<" ">>" ">>>" 
                   "+=" "-=" "*=" "/=" "&=" "|=" "^=" "%=" "<<=" ">>=" ">>>="
                   ]))))

;------------------------------------------------------------------------------
;                           3.5 Input Elements and Tokens
;------------------------------------------------------------------------------

(def Token
  ; Token:
  ;    Identifier
  ;    Keyword
  ;    Literal
  ;    Separator
  ;    Operator
  (alt
    Identifier
    Keyword
    Literal
    Seperator
    Operator))

(def InputElement
  ; InputElement:
  ;    WhiteSpace
  ;    Comment
  ;    Token
  (alt
    WhiteSpace
    Comment
    Token))

(def InputElements
  ; InputElements:
  ;    InputElement
  ;    InputElements InputElement
  (alt
    (conc InputElements InputElement)
    InputElement))

(def Input
  ; Input:
  ;    InputElementsopt Subopt
  (rep+
    (conc InputElements Sub)))