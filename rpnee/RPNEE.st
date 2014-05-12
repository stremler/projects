"S. J. Stremler's stupid little RPN expression evaluator."

"---------------------------------------------------------------------------"
Object subclass: #RPNEval
       instanceVariableNames: 'stack opDictionary'
       classVariableNames: ''
       poolDictionaries: ''
       category: 'RPN'
!

RPNEval comment: 'I am an expression evaluator that uses a stack and
        reverse polish notation, with abilities to define words in
        terms of smalltalk classes and positional replacements of values.'
!


"---------------------------------------------------------------------------"
Object subclass: #RPNContext
       instanceVariableNames: 'opDictionary varDictionary tokenSource'
       classVariableNames: ''
       poolDictionaries: ''
       category: 'RPN'
!

RPNContext comment: 'I provide access to information that some words need.'
!

"---------------------------------------------------------------------------"
Object subclass: #RPNTokenSource
       instanceVariableNames: 'stack tokenstream'
       classVariableNames: ''
       poolDictionaries: ''
       category: 'RPN'
!

RPNTokenSource comment: 'I wrap a tokenstream to provide pushback capabilities.'
!

"---------------------------------------------------------------------------"

Object subclass: #RPNStack
       instanceVariableNames: 'stack'
       classVariableNames: ''
       poolDictionaries: ''
       category: 'RPN'
!

RPNStack comment: 'I am an internal class used by the RPNEval class to 
         manage stack-like behavior with standard stack-based syntax.'
!


"---------------------------------------------------------------------------"
"---------------------------------------------------------------------------"
!RPNTokenSource class methodsFor: 'instantiation'!

new: aTokenStream
   |me|
   me := super new.
   me initialize: aTokenStream.
   ^me
!

!RPNTokenSource methodsFor: 'initialization'!

initialize: aTokenStream
"   super init."
   tokenstream := aTokenStream.
   stack := RPNStack new.
   ^self
!

!RPNTokenSource methodsFor: 'access'!

next
   stack isEmpty ifFalse: [ ^stack pop ] ifTrue: [ ^tokenstream next ] 
!

atEnd
   ^stack isEmpty and: [tokenstream atEnd]
!

pushBack: aWord
   " I check to see if aWord is a string, and if so, I tokenize it,
     otherwise I do nothing, and push the results on to the internal
     stack for it to be handed back by next."
   aWord isString ifFalse: [ stack push: aWord ]
                  ifTrue:  [ |ls ts| 
                             ls := RPNStack new.
                             ts := TokenStream on: aWord.
                             [ ts atEnd   ] whileFalse: [ ls push: ts next ].
                             [ ls isEmpty ] whileFalse: [ stack push: ls pop ].
                           ]
!


"---------------------------------------------------------------------------"
!RPNStack class methodsFor: 'instantiation'!

new
   |me|
   me := super new.
   me init.
   ^me
!

!RPNStack methodsFor: 'initialization'!

init
   stack := OrderedCollection new.
!

!RPNStack methodsFor: 'basic'!

push: anObject
   stack addLast: anObject.
   ^self
!

pop
   stack isEmpty ifFalse: [ ^stack removeLast ].
   ^nil
!

peek
   | item |
   item := self pop.
   item = nil ifFalse: [ self push: item ].
   ^item
!

isEmpty
   ^stack isEmpty
!


"---------------------------------------------------------------------------"
"RPNEval methods"

!RPNEval class methodsFor: 'instantiation'!

new
   |me|
   me := super new.
   ^me initialize
!

!RPNEval methodsFor: 'initialization'!

initialize
   stack := RPNStack new.
   opDictionary := Dictionary new.
   self preloadDictionary.
   ^self
!

preloadDictionary
   " A word in the opDictionary is expected to be a block that takes
     the stack and the context as arguments. "

   opDictionary addAll: { "stack operators"
      'nop'  -> [ :s :c | ].
      'drop' -> [ :s :c | s pop ].
      'dup'  -> [ :s :c | s push: ( s peek ) ].
      'rot'  -> [ :s :c | |t1 t2 t3| t1 := s pop. t2 := s pop. t3 := s pop. 
                                     s push: t2. s push: t1. s push t3. ].
   }.

   opDictionary  "quoting construct"
      at: '{'    put: [ :s :c | | list count ts token |
                                count := 1. list := ''.
                                ts := c tokens.
                                [ token := ts next. 
                                  token = '{' ifTrue: [ count := count + 1 ].
                                  token = '}' ifTrue: [ count := count - 1 ].
                                  ((count = 0) and: [token = '}']) 
                                     ifFalse: [ list := list , ' ', token. 
                                               false ]
                                     ifTrue: [ true ]
                                ] whileFalse.
                                s push: list 
                      ];
      at: '}'    put: [ :s :c | 'found close brace - shouldn''t see' displayNl].

   opDictionary  addAll: { "math and display operators"
      '+'  -> [ :s :c | s push: (s pop asNumber + (s pop asNumber)) ].
      '-'  -> [ :s :c | s push: (s pop asNumber - (s pop asNumber)) ].
      '/'  -> [ :s :c | s push: (s pop asNumber / (s pop asNumber)) ].
      '*'  -> [ :s :c | s push: (s pop asNumber * (s pop asNumber)) ].
      '.'  -> [ :s :c | s pop display ].
      '..' -> [ :s :c | s pop displayNl ].
      '@'  -> [ :s :c | s pop ].
   }.

   opDictionary addAll: { "relational operators"
      '.gt.' -> [ :s :c | s push: (s pop asNumber <  (s pop asNumber)) ].
      '.lt.' -> [ :s :c | s push: (s pop asNumber >  (s pop asNumber)) ].
      '.ge.' -> [ :s :c | s push: (s pop asNumber <= (s pop asNumber)) ].
      '.le.' -> [ :s :c | s push: (s pop asNumber >= (s pop asNumber)) ].
      '.eq.' -> [ :s :c | s push: (s pop asNumber =  (s pop asNumber)) ].
      '<'    -> [ :s :c | s push: (s pop asNumber <  (s pop asNumber)) ].
      '>'    -> [ :s :c | s push: (s pop asNumber >  (s pop asNumber)) ].
      '<='   -> [ :s :c | s push: (s pop asNumber <= (s pop asNumber)) ].
      '>='   -> [ :s :c | s push: (s pop asNumber >= (s pop asNumber)) ].
      '=<'   -> [ :s :c | s push: (s pop asNumber <= (s pop asNumber)) ].
      '=>'   -> [ :s :c | s push: (s pop asNumber >= (s pop asNumber)) ].
      '='    -> [ :s :c | s push: (s pop asNumber =  (s pop asNumber)) ].
      "equality op should probably be modified!"
   }.

   opDictionary addAll: { "logical operators"
      'and' -> [ :s :c | s push: ((s pop) and: [s pop]) ].
      'or'  -> [ :s :c | s push: ((s pop) or:  [s pop]) ].
      'not' -> [ :s :c | s push: ((s pop) not ) ].
   }.

   opDictionary addAll: { "control structures"
      'if'     -> [ :s :c | | i j | i := s pop. j := s pop. 
                              j ifTrue: [ c tokens pushBack: i ] ].
      'ifelse' -> [ :s :c | | i j k | i := s pop. j := s pop. k := s pop.
                              k ifTrue:  [ c tokens pushBack: j ] 
                                ifFalse: [ c tokens pushBack: i ] ].
   }.

!


!RPNEval methodsFor: 'evaluating'!

evaluate: aStream with: aDictionary
  | tokenSource context token word |
   tokenSource := 
      RPNTokenSource new: ((aStream isKindOf: TokenStream )
                           ifTrue: [ aStream ]
                           ifFalse: [ TokenStream onStream: aStream ]).
  context := RPNContext new.
  context tokens: tokenSource; opDictionary: opDictionary.

  [ tokenSource atEnd ] whileFalse: [
     token := tokenSource next.
     (token isString and: [ token startsWith: '#' ])
        ifTrue:  [ stack push: (token copyFrom: 2 to: (token size)) ]
        ifFalse: [ (opDictionary at: token 
                                 ifAbsent: [[ :s :c | stack push: token]] ) 
                    value: stack
                    value: context
        ]
  ].
  ^stack pop
!

"---------------------------------------------------------------------------"
!RPNContext methodsFor: 'initialization'!

init
   varDictionary := Dictionary new.
   ^super init
!

!RPNContext methodsFor: 'access'!

opDictionary
   ^opDictionary
!

opDictionary: aDictionary
   opDictionary := aDictionary.
   ^self
!

tokens
   ^tokenSource
!

tokens: aTokenSource
   tokenSource := aTokenSource.
   ^self
!

addVars: aDictionary
   varDictionary addAll: aDictionary
!

set: aVariableName to: aValue
   varDictionary at: aVariableName put: aValue.
   ^self
!

get: aVariableName
   ^varDictionary at: aVariableName ifAbsent: [ nil ]
!

"---------------------------------------------------------------------------"
"---------------------------------------------------------------------------"

"Extend Number to simplify our syntax."
!Number methodsFor: 'hack'!
asNumber
   ^self
!

!!



(RPNEval new evaluate: (TokenStream on: 'a b dup . .') with: nil) printNl!
(RPNEval new evaluate: (TokenStream on: '1 2 3 * +') with: nil) printNl !
(RPNEval new evaluate: (TokenStream on: 'is .') with: nil) printNl !


(RPNEval new evaluate: (TokenStream on: ' { ______ } ..
5 5 * 
4 6 * 
< dup ..
  { { { 5 times 5 less than 4 times 6     } .. } } 
  { { { 5 times 5 not less than 4 times 6 } .. } } ifelse
') with: nil) printNl !


(RPNEval new evaluate: (TokenStream on: '3 3 * #hello ..') with: nil) printNl!
