DECLARE FUNCTION xMid$ (string1$, start1!, stop1!)
DECLARE FUNCTION xNot! (bool!)
DECLARE FUNCTION xStr$ (value!)
DECLARE FUNCTION xRange! (NUM!, lval!, uval!)
DECLARE FUNCTION lenPair! (pair$)
DECLARE FUNCTION errSig$ (eCode$)
DECLARE FUNCTION consPair$ (fst$, rest$)
DECLARE FUNCTION getDatype$ (typet$)
DECLARE FUNCTION opCheck$ (operator$)
DECLARE FUNCTION selPairE$ (pair$, selE!)
DECLARE FUNCTION fRead$ (void$)
DECLARE FUNCTION evPair$ (pair$)
DECLARE FUNCTION eForm$ (form$)
DECLARE FUNCTION evAtom$ (atom$)
DECLARE FUNCTION applyOp$ (lst$)

'
'   Skele's LISP Interpreter v3.0
'   copyright (c) Skeleville Software 2006 - 2007
'     This program emulates some of the functions found in a real LISP
'   interpreter and also directly imports some of the functions from QBasic.
'   It was created as a learning tool for LISP and so may contain some bugs
'   in the lisp syntax expressions areas or some functions and features may
'   not be present. Also, there will be some unique features.
'   Enjoy and modify to your heart's content only leaving the copyright intact
'   to show where the program is coming from.

' Created:
'  16/8/06
' Last Modified:
'  26/8/06

'   Function declarations (no subs) to ease reading and faccilitate
'   conversion to another language. All functions used must be compatible
'   with some other language.
'

'
' Main module. This initializes global variables and initiates the
' read-evaluate-print loop which is the major operation of LISP. There is
' also a facility for reading the required expression from the commmand line.
'
' Notes:
'  1/9/06: add application saving and loading.
'  add command line options.
'  add help features.
'  add auto-indenting to read phase.
'  modify system commands to prevent syntax clashes.
'  3/9/06: modify to add, access and update size info for list elements to
'   speed the car function. But would have to restructure most of the
'   existing routines to use and there could be incompatibilities. Therefore,
'   a converter is also in order.
'  7/9/06: add form & evaluation saving to memory. Can probably be used to
'   bypass the evaluation phase of the R-E-P loop.
'  9/9/06: modify to avoid using space limited commands that may lead to
'   qbasic errors, (eg. function calls, variables, etc.) instead, use
'   commands that limit calls (goto), make variable size hardware limited
'   (streams), etc.
'  optimize code to avoid unnecessary calls.
'  bug found where a single pair of numbers would also make a decimal, which
'   could lead to more bugs. Will have to currently avoid creating single
'   pairs of pure numbers.
'  12/9/06: bug found where a + signed number can be taken for a normal atom.
'
' History:
'  1/9/06: provided space for extended system commands.
'  7/9/06: added pre load and save commands.
'  added some data type constants.
'

' Global constants:
CONST HEAD = 1, TAIL = -1, EL$ = "()", LF$ = "NIL", LT$ = "T", OB$ = "("
CONST CB$ = ")", LP$ = "list", AP$ = "dot-pair", atmT$ = "atom"
CONST numT$ = "number", blnT$ = "boolean", procT$ = "procedure"
CONST strT$ = "string"

DIM SHARED varMem$
varMem$ = "((()) (()))"

cmdLn$ = COMMAND$
IF cmdLn$ <> "" THEN
'arguments present on the command line. modify to check input.
  PRINT eForm$(cmdLn$)

ELSE

  CLS
  PRINT
  PRINT "Skele's LISP Interpreter v3.0"
  PRINT "created by Skele Drew"
  PRINT "copyright (c) Skeleville Software 2006 - 2007"
  PRINT
  PRINT "Current date and time is: "; DATE$; " "; TIME$
  PRINT

  DO
  'interpreter read-eval-print loop. mod...

REPLoop:
'for debugging interpreter errors.
''ON ERROR GOTO ErrMan

    PRINT
    ''expr$ = fRead$(void$)
    INPUT expr$
   
    SELECT CASE expr$
    'check for any interpreter commands.
   
    CASE "sys:exit"
    'exit program.
      PRINT
      PRINT "Terminating Skele's LISP Interpreter!"
      END

    CASE "sys:pause"
    'pause execution for debugging or other stuff.
      STOP

    CASE "sys:help"
    'provide general help.
      PRINT "Feature under construction"

    CASE "sys:clear", " "
    'clear the screen.
      CLS

    CASE "sys:time"
    'print time and date.
      PRINT "Current date and time is: "; DATE$; " "; TIME$

    CASE ""
    'nothing given.
      STOP

    CASE "sys:reset"
    'clear and reinitialize program.
      varMem$ = "((()) (()))"
      PRINT "Interpreter memory reset!"

    CASE "sys:save"
    'write memory, forms and results to files.
      memh = FREEFILE
      OPEN "a:\memfile.rec" FOR OUTPUT AS memh
      PRINT #memh, varMem$
      CLOSE memh

    CASE "sys:load"
    'load memory file.
      memh = FREEFILE
      OPEN "a:\memfile.rec" FOR INPUT AS memh
      LINE INPUT #memh, memstring$
      'merge loaded memory contents with current memory.

    CASE ELSE
    'not a system command.
      retval$ = eForm$(expr$)
    END SELECT
   
    IF errID <> 0 THEN
    'print result and any errors.
      retval$ = retval$ + CHR$(13) + CHR$(10) + errMsg$
    END IF
    PRINT
    PRINT retval$
  LOOP
END IF

ErrMan:
  STOP
  PRINT "Error in interpreter coded"; ERR
  RESUME REPLoop

'obsolete scrap code:


    IF expr$ = "exit" THEN
    'exit program
      PRINT
      PRINT "Goodbye!"
      END
    END IF

FUNCTION applyOp$ (list$)
'applies an operator to its operands.
' Created:
'  26/8/06
' Last Modified:
'  3/9/06
'
' Input:
'  application list.
' Output:
'  application result.
'
' Notes:
'  26/8/06: largs$ - list arguments.
'  lOpr$ - list operator.
'  retval - result of mathematical procedures.
'  modify math cases to check numbers and use long working methods.
'  to upgrade all retro version cases.
'  2/9/06: after fixing a variable creation bug, it was noted that several
'   bugs had been caused by uninitialized variables.
'  3/9/06: modify array-using routines to use the speed selector instead.
'  7/9/06: to add essential predicate procedures.
'  to add looping forms.
'  9/9/06: modify procedure?
'
' History:
'  27/8/06: corrected minor bug.
'  2/9/06: corrected bug preventing variable creation.
'  3/9/06: added if conditional.
'  corrected bugs in set operator.
'  5/9/06: corrected setq operator bugs.
'  9/9/06: fixed variable modification bug in set.
'  added lambda procedure.
'

  ''lArgs$ = OB$+selPairE$(list$, -1)+CB$
  ''lOpr$ = selPairE$(list$, 1)
  lstLen = VAL(selPairE$(list$, 0))
  eleInd = lstLen - 1
  DIM opList$(eleInd)
  tmpInd = 0
  dvarMem$ = EL$

  DO
  'store elements in array. ok.
    opList$(tmpInd) = selPairE$(list$, tmpInd + 1)
    tmpInd = tmpInd + 1

    IF tmpInd > eleInd THEN
    'finished creating array.
      EXIT DO
    END IF
  LOOP
 
  SELECT CASE opList$(0)
  
  CASE "eval"
  'evaluate argument.
    applyOp$ = eForm$(opList$(1))

  CASE "+"
  'add stuff. ok.
    retval = 0
 
    FOR inc = 0 TO eleInd
    'reference elements.
      retval = retval + VAL(opList$(inc))
    NEXT
    applyOp$ = xStr$(retval)
 
  CASE "+"
  'add stuff. test. discarded for retro method (less work).
    retval = 0
   
    DO
    'reference elements.
      retval = retval + VAL(selPairE$(largs$, 1))
     
      IF largs$ = EL$ THEN
      'nothing more to add.
        applyOp$ = xStr$(retval)
        EXIT DO
      END IF
      largs$ = OB$ + selPairE$(largs$, -1) + CB$
    LOOP
 
  CASE "-"
  'subtract stuff. ok

    IF eleInd = 0 THEN
    'no args.
      retval = 0
    ELSE

      retval = VAL(opList$(1))
     
      IF eleInd > 1 THEN
      'more than 1 args present.
     
        FOR inc = 2 TO eleInd
        'reference args.
          retval = retval - VAL(opList$(inc))
        NEXT
      END IF
    END IF
    applyOp$ = xStr$(retval)

  CASE "*"
  'multiply stuff. ok
    
    retval = 1

    IF eleInd > 0 THEN
    'args present.
   
      FOR inc = 1 TO eleInd
        retval = retval * VAL(opList$(inc))
      NEXT

    END IF
    applyOp$ = xStr$(retval)
 
  CASE "/"
  'divide stuff. ok.

    IF eleInd > 1 THEN
    'at least 2 args.

      IF VAL(opList$(1)) = 0 THEN
      'dividing 0.
        retval = 0
     
      ELSE
        retval = VAL(opList$(1))

        FOR inc = 2 TO eleInd
        'divide args.

          IF VAL(opList$(inc)) = 0 THEN
          'division by 0!
            void$ = errSig$("dbz")
            errMsg$ = "Division by 0!"
            EXIT FOR
          ELSE
            retval = retval / VAL(opList$(inc))
          END IF
        NEXT
      END IF

    ELSE
     
      IF eleInd = 1 THEN
      'single arg.
        retval = 1 / VAL(opList$(1))
     
      ELSE
      'no args.
        retval = 0
      END IF
    END IF
    applyOp$ = xStr$(retval)

  CASE "\", "rem"
  'integer division. returns a list of result and remainder. ok.
  'modify to return one of its two results.

    IF eleInd > 1 THEN
    'at least 2 args.

      IF VAL(opList$(1)) = 0 THEN
      'dividing 0.
        retval = 0
    
      ELSE
        remval = VAL(opList$(1))

        FOR inc = 2 TO eleInd
        'divide args.

          IF VAL(opList$(inc)) = 0 THEN
          'division by 0!
            void$ = errSig$("dbz")
            errMsg$ = "Division by 0!"
            EXIT FOR
          ELSE
          'special division by repeated subtraction.
         
            IF inc > 2 THEN
            'not first division (over 2 args.)
              remval = retval
            END IF
            retval = 0
            divval = VAL(opList$(inc))

            WHILE remval >= divval
            'subtract until remainder is less than divisor.
              remval = remval - divval
              retval = retval + 1
            WEND

          END IF
        NEXT
      END IF

    ELSE
    
      IF eleInd = 1 THEN
      'single arg.
        retval = 1 / VAL(opList$(1))
    
      ELSE
      'no args.
        retval = 0
      END IF
    END IF
   
    IF opList$(0) = "/" THEN
    'return result.
      applyOp$ = xStr$(retval)

    ELSE
    'return remainder.
      applyOp$ = xStr$(remval)
    END IF
   
  CASE "x-power-y"
  'finds one value to the power of another. mod...
    applyOp$ = xStr$(VAL(opList$(1)) ^ VAL(opList$(2)))

  CASE "x-root-y"
  'finds the required root of a value.
    applyOp$ = xStr$(VAL(opList$(1)) ^ (1 / (VAL(opList$(2)))))
    
  CASE "abs"
  'returns the absolute value.
    applyOp$ = xStr$(ABS(VAL(opList$(1))))

  CASE "cos"
  'returns the cosine of an angle given in radians.
    applyOp$ = xStr$(COS(VAL(opList$(1))))
 
  CASE "tan"
  'returns the tangent of an angle given in radians.
    applyOp$ = xStr$(TAN(VAL(opList$(1))))
 
  CASE "sin"
  'returns the sine of a number given in radians.
    applyOp$ = xStr$(SIN(VAL(opList$(1))))
 
  CASE "arctan"
  'returns the arc tangent.
    applyOp$ = xStr$(ATN(VAL(opList$(1))))
 
  CASE "<"
  'ascending order (less than). ok.
    retval = VAL(opList$(1))

    FOR inc = 2 TO eleInd
    'compare.
      cmpval = VAL(opList$(inc))
     
      IF retval < cmpval THEN
      'in order check.
        retval = cmpval
        applyOp$ = LTRIM$(STR$(retval))
     
      ELSE
        applyOp$ = LF$
        EXIT FOR
      END IF
    NEXT

  CASE ">"
  'descending order (greater than). ok.
    retval = VAL(opList$(1))

    FOR inc = 2 TO eleInd
    'compare.
      cmpval = VAL(opList$(inc))
    
      IF retval > cmpval THEN
      'in order check.
        retval = cmpval
        applyOp$ = LTRIM$(STR$(retval))
    
      ELSE
        applyOp$ = LF$
        EXIT FOR
      END IF
    NEXT

  CASE "and"
  'evals up to nil value. ok.
    
    IF eleInd = 0 THEN
    'function only.
      applyOp$ = clf$

    ELSE

      FOR inc = 1 TO eleInd
      'eval args.
        evalstr$ = eForm$(opList$(inc))

        IF evalstr$ = clf$ THEN
        'arg gives nil.
          applyOp$ = clf$
          EXIT FOR

        ELSE
          applyOp$ = evalstr$
        END IF
      NEXT
    END IF

  CASE "or"
  'evals up to non-nil value. ok.
   
    IF eleInd = 0 THEN
    'function only.
      applyOp$ = clf$

    ELSE

      FOR inc = 1 TO eleInd
      'eval args.
        evalstr$ = eForm$(opList$(inc))

        IF evalstr$ = clf$ THEN
        'arg gives nil.
          applyOp$ = clf$

        ELSE
          applyOp$ = evalstr$
          EXIT FOR
        END IF
      NEXT
    END IF

  CASE "car"
  'return head element. ok.

    IF getDatype$(opList$(1)) = LP$ THEN
    'get first element only if list.
      applyOp$ = selPairE$(opList$(1), 1)

    ELSE
      void$ = errSig$("lst")
      errMsg$ = "Error: " + opList$(1) + " isn't of type list! Signalled by "
      errMsg$ = errMsg$ + curFunc$ + " at level " + xStr$(level)
    END IF

  CASE "cdr"
  'return any tail elements. ok.
 
    IF getDatype$(opList$(1)) = LP$ THEN
    'send to splitter only if list.
      applyOp$ = OB$ + selPairE$(opList$(1), -1) + CB$

    ELSE
      void$ = errSig$("tp")
      errMsg$ = "Error: " + opList$(1) + " isn't of type list! Signalled by "
      errMsg$ = errMsg$ + curFunc$ + " at level" + xStr$(level)
    END IF

  CASE "cons"
  'construct lists. ok.
    applyOp$ = consPair$(opList$(1), opList$(2))

  CASE "set", "setq"
  'binds values to variables. ok?
   
    IF opList$(0) = "setq" THEN
    'convert set guote to set and evaluate second argument.
      opList$(0) = "set"
      opList$(2) = eForm$(opList$(2))
    END IF
    svarMem$ = selPairE$(varMem$, 1)
    svarMem1$ = svarMem$ 'preserve.
    DIM savMem$(lenPair(svarMem$) - 1)
    memMark = 0
    finSrch = 0

    DO
    'search for symbol to check.
      tvarMem$ = selPairE$(svarMem$, 1)

      IF selPairE$(tvarMem$, 1) = opList$(1) THEN
      'delete current value stored, restore any other variables and add new.
        tvarMem$ = consPair$(opList$(1), consPair$(opList$(2), OB$ + selPairE$(tvarMem$, -2) + CB$))
        svarMem$ = consPair$(tvarMem$, OB$ + selPairE$(svarMem$, -1) + CB$)
        'can float changed value to most recent by restoring any other variables
        'first.

        IF memMark > 0 THEN
        '2/9/06: ensures something was found before adding it.
       
          FOR inc = 0 TO memMark
          'recreate the rest of the variable memory. recheck!
            svarMem$ = consPair$(savMem$(inc), svarMem$)
          NEXT
        END IF
        varMem$ = consPair$(svarMem$, OB$ + selPairE$(varMem$, -1) + CB$)
        finSrch = 1

      ELSE
      'symbol not found, discard current car and continue.
        svarMem$ = OB$ + selPairE$(svarMem$, -1) + CB$
        savMem$(memMark) = tvarMem$
        memMark = memMark + 1

        IF svarMem$ = EL$ THEN
        'variable not found at all, create it. ok.
          varMem$ = consPair$(consPair$(consPair$(opList$(1), consPair$(opList$(2), dvarMem$)), selPairE$(varMem$, 1)), OB$ + selPairE$(varMem$, -1) + CB$)
          finSrch = 1
        END IF
      END IF
    LOOP WHILE finSrch = 0
    applyOp$ = opList$(2)

  CASE "if"
  'conditional execution. ok.
    tForm$ = eForm$(opList$(1))
   
    IF tForm$ <> LF$ THEN
    'test evaluates to true, evaluate consequence.
      applyOp$ = eForm$(opList$(2))

    ELSE
    'test is false.

      IF eleInd = 3 THEN
      'alternative present.
        applyOp$ = eForm$(opList$(3))

      ELSE
      'no alternative.
        applyOp$ = LF$
      END IF
    END IF
  
  CASE "get"
  'form gets a byte of data from a given device.
   
    SELECT CASE opList$(1)
    'action based on device.

    CASE "kbrd"
    'return character from keyboard.

      DO
      'wait for character.
        Char$ = INKEY$
      LOOP WHILE Char$ = ""
      applyOp$ = RIGHT$(Char$, 1)
      EXIT FUNCTION

    CASE "memory"
    'return character from memory.
      DEF SEG = VAL(opList$(2))
      applyOp$ = CHR$(PEEK(VAL(opList$(3))))
      DEF SEG
      EXIT FUNCTION

    CASE "screen"
    'return character or pixel from screen.

    END SELECT
 
  CASE "number?"
  'number predicate. ok.

    IF eleInd < 1 THEN
    'no args.
      applyOp$ = LF$
      EXIT FUNCTION
    END IF

    IF getDatype$(opList$(1)) = numT$ THEN
      applyOp$ = LT$

    ELSE
      applyOp$ = LF$
    END IF

  CASE "symbol?"
  'symbol predicate. ok.

    IF eleInd < 1 THEN
    'no args.
      applyOp$ = LF$
      EXIT FUNCTION
    END IF

    IF getDatype$(opList$(1)) = atmT$ THEN
      applyOp$ = LT$

    ELSE
      applyOp$ = LF$
    END IF
 
  CASE "boolean?"
  'boolean predicate. ok?

    IF eleInd < 1 THEN
    'no args.
      applyOp$ = LF$
      EXIT FUNCTION
    END IF

    IF getDatype$(opList$(1)) = blnT$ THEN
      applyOp$ = LT$

    ELSE
      applyOp$ = LF$
    END IF

  CASE "pair?"
  'pair predicate. ok?

    IF eleInd < 1 THEN
    'no args.
      applyOp$ = LF$
      EXIT FUNCTION
    END IF
   
    IF opList$(1) = EL$ THEN
    'empty list isn't a pair.
      applyOp$ = LF$
      EXIT FUNCTION
    END IF
    rType$ = getDatype$(opList$(1))

    IF rType$ = AP$ OR rType$ = LP$ THEN
      applyOp$ = LT$

    ELSE
      applyOp$ = LF$
    END IF

  CASE "null?"
  'empty list predicate. ok.

    IF eleInd < 1 THEN
    'no args.
      applyOp$ = LF$
      EXIT FUNCTION
    END IF

    IF opList$(1) = EL$ THEN
      applyOp$ = LT$

    ELSE
      applyOp$ = LF$
    END IF

  CASE "procedure?"
  'procedure predicate. fix.

    IF eleInd < 1 THEN
    'no args.
      applyOp$ = LF$
      EXIT FUNCTION
    END IF

    IF getDatype$(opList$(1)) = procT$ THEN
      applyOp$ = LT$

    ELSE
      applyOp$ = LF$
    END IF
                                    
  CASE "="
  'predicate for numerical equality. ok.

    IF getDatype$(opList$(1)) = numT$ AND getDatype$(opList$(2)) = numT$ THEN
    'both args are numbers.

      IF VAL(opList$(1)) = VAL(opList$(2)) THEN
      'both equal.
        applyOp$ = LT$

      ELSE
      'not equal.
        applyOp$ = LF$
      END IF

    ELSE
    'non-number found.
      void$ = errSig$("num")
      applyOp$ = LF$
    END IF
 
  CASE "eq?"
  'predicate for symbol equality.
    void$ = errSig$("!")
    applyOp$ = LF$
    'under construction.

  CASE "eqv?"
  'predicate for equality in all objects but lists.
    void$ = errSig$("!")
    applyOp$ = LF$
    'under construction.

  CASE "equal?"
  'predicate for equality in all objects. ok?

    IF opList$(1) = opList$(2) THEN
    'both equal.
      applyOp$ = LT$

    ELSE
    'not equal.
      applyOp$ = LF$
    END IF

  CASE "lambda"
  'defines a procedure.
    applyOp$ = list$

  CASE ELSE
  'attempts to apply a lambda procedure. ok?
    parList$ = selPairE$(opList$(0), 2)
    procBody$ = selPairE$(opList$(0), 3)
    ltChars$ = " ()."
    argRd = 1
   
    DO
    'get parameter.
     
      IF parList$ = EL$ THEN
      'no parameters.
        EXIT DO
      END IF
      parCar$ = selPairE$(parList$, HEAD)
      lenPar = LEN(parCar$)
      nPos = 1
     
      DO
      'replace parameter with argument.
        lenBody = LEN(procBody$)
        parPos = INSTR(nPos, procBody$, parCar$)
       
        IF parPos = 0 THEN
        'not found.
          EXIT DO
        END IF
        lChar$ = MID$(procBody$, parPos - 1, 1)
        tChar$ = MID$(procBody$, parPos + lenPar, 1)

        IF INSTR(ltChars$, lChar$) > 0 AND INSTR(ltChars$, tChar$) > 0 THEN
        'parameter found.
          p1Body$ = xMid$(procBody$, 1, parPos - 1)
          p2Body$ = xMid$(procBody$, parPos + lenPar, lenBody)
          procBody$ = p1Body$ + opList$(argRd) + p2Body$

        ELSE
        'not a parameter.
          nPos = parPos + 1
        END IF
      LOOP
      parList$ = OB$ + selPairE$(parList$, TAIL) + CB$
      argRd = argRd + 1
    LOOP
    applyOp$ = eForm$(procBody$)
  END SELECT
END FUNCTION

FUNCTION consPair$ (fst$, rest$)
'constructs a pair. ok.
' Created:
'  26/8/06
' Last Modified:
'  9/9/06
'
' Input:
'  element to be paired.
'  rest to be paired to.
' Output:
'  constructed pair.
'
' Notes:
'  27/8/06: to prevent the insersion of a space when consing to an empty list.
'
' History:
'  27/8/06: corrected empty list process bug.
'  9/9/06: fixed atomic pair constructor to take other atomic pairs along
'   with the atom.
'  fixed null car to return cdr unchanged.
'

  IF fst$ = "" THEN
  'nothing to add.
    consPair$ = rest$
    EXIT FUNCTION
  END IF
  rType$ = getDatype$(rest$)
 
  IF rType$ <> LP$ THEN
  'construct atomic pair.
    consPair$ = fst$ + "." + rest$

  ELSE
  'construct list pair.
   
    IF rest$ = EL$ THEN
    'consing an empty list.
      consPair$ = OB$ + fst$ + CB$

    ELSE
      consPair$ = OB$ + fst$ + " " + MID$(rest$, 2)
    END IF
  END IF
END FUNCTION

FUNCTION eForm$ (form$)
'evaluates the given form.
'
' Created:
'  16/8/06
' Last Modified:
'  16/8/06
'
' Input:
'  expression to be evaluated.
' Output:
'  evaluation of the expression.
'
' Notes:
'  20/8/06: modify to check for the special characters ( ) [ ] { } ; , " ' `
'   # \ in symbols.
'  26/8/06: modify to use data type check procedure.
'  modify to process quoted forms separately (the data type procedure needs
'   to be generalized to work with more routines).
'
' History:
'

  IF MID$(form$, 1, 1) = "'" THEN
  'evaluate quoted expression. ok.
    eForm$ = MID$(form$, 2)
  END IF
 
  SELECT CASE MID$(form$, 1, 1)
  'evaluates the form based on its type.

  CASE OB$
  'evaluates a list pair.
    eForm$ = evPair$(form$)
   
  CASE CHR$(39)
  'evaluates a quoted expression. 'ok.
   eForm$ = MID$(form$, 2)
   'prints expression w/out quote.

  CASE "#"
  'evaluate a special data structure.
    'under development.
    STOP
    'currently returns an error of "unimplemented future data type".

  CASE ELSE
  'evaluates an atom. 'ok.
    eForm$ = evAtom$(form$)
    'passes control to atom evaluation.
  END SELECT

END FUNCTION

FUNCTION errSig$ (eCode$)
'signals errors.
' Created:
'  26/8/06
' Last Modified:
'  1/9/06
'
' Input:
'  error code.
' Output:
'  nothing.
'
' Notes:
'  1/9/06: modify to handle all user trappable errors and provide recovery
'   for those possible by maybe using qbasic listing and methods.
'  7/9/06: modify to take relevant info and create more comprehensive error
'   signals and handlers.
'
' History:
'  7/9/06: added some messages.
'

  STOP
 
  SELECT CASE eCode$
  'create message based on code.

  CASE "var"
  'unbound.
    errt$ = "unbound variable."

  CASE "proc"
  'procedure not found.
    errt$ = "undefined procedure."

  CASE "dbz"
  'division.
    errt$ = "division by zero!"

  CASE "lst"
  'list.
    errt$ = "not of type list."
   
  CASE ELSE
  'unknown error.
    errt$ = "unknown or fatal error!"
 
  END SELECT
  COLOR 20
  PRINT "ERROR: "; errt$
  COLOR 7
END FUNCTION

FUNCTION evAtom$ (atom$)
'evaluate an atom. ok?
' Created:
'  16/8/06
' Last Modified:
'  25/8/06
' Input:
'  atom to evaluate.
' Output:
'  result of evaluation.
' atomType:
'  type of atom found: 0 = none, 1 = number, 2 = stored value, 3 = predefined.
' Notes:
'
' History:
'  25/8/06: changed "memory" searching code to directly use pair selector.
'  26/8/06: clarified and optimized variable code.
'  27/8/06: made number type atom independent of applyOp.
'  7/9/06: major code optimizations and restructuring.
'

  IF getDatype$(atom$) = numT$ THEN
  'numeric constant evaluates to itself. ok.
    evAtom$ = atom$
    EXIT FUNCTION
  END IF

  SELECT CASE atom$
  'check for predefined constant.
   
  CASE LT$
  'true...
    evAtom$ = LT$
    EXIT FUNCTION

  CASE LF$
  'not true...
    evAtom$ = LF$
    EXIT FUNCTION

  CASE ELSE
  'not a predefined constant, search working variables. ok.
    svarMem$ = selPairE$(varMem$, 1)
   
    DO
    'search for symbol to check.
      tvarMem$ = selPairE$(svarMem$, 1)

      IF selPairE$(tvarMem$, 1) = atom$ THEN
      'get symbol value if found.
        evAtom$ = selPairE$(tvarMem$, 2)
        EXIT FUNCTION

      ELSE
      'symbol not found, discard current car and continue.
        svarMem$ = OB$ + selPairE$(svarMem$, -1) + CB$

        IF svarMem$ = EL$ THEN
        'not in variables, signal error.
          void$ = errSig("var")
          evAtom$ = ""
          EXIT FUNCTION
        END IF
      END IF
    LOOP
 
  END SELECT
 
END FUNCTION

FUNCTION evPair$ (pair$)
'evaluates a pair. ok?
' Created:
'  17/8/06
' Last Modified:
'  26/8/06
' Input:
'  pair to evaluate.
' Output:
'  result of evaluation.
' Notes:
'  This is a sequence of cons'd values and its type depends on operand2 of a
'   cons expression. A list is created if it is a null list, but a dotted
'   pair is created if its an atom.
'
' History:
'  18/8/06: modified to evaluate dotted pairs separetely from lists.
'  19/8/06: edited included constants.
'  20/8/06: corrected bugs in code.
'  26/8/06: corrected bugs in code.
'

  level = level + 1
  eleCnt = 0

  IF pair$ = EL$ THEN
  'empty list pair. ok.
    evPair$ = LF$
  ELSE
  'evaluate pair contents.

    IF getDatype$(pair$) = AP$ THEN
    'check for atomic dotted pair type.
      lPair$ = ""

      DO
      'gets dotted pair elements. ok.
        pairEle$ = selPairE$(pair$, HEAD)
        pair$ = selPairE$(pair$, TAIL)
   
        IF pairEle$ = "" THEN
        'completed reading elements.
          lPair$ = MID$(lPair$, 1, LEN(lPair$) - 1)
          EXIT DO
        ELSE
        lPair$ = lPair$ + eForm$(pairEle$) + "."
        eleCnt = eleCnt + 1
        END IF
      LOOP
      evPair$ = lPair$

    ELSE

      DO
      'gets list pair elements. de-bloat by abstracting common statements.

        IF eleCnt = 0 THEN
        'check first element, ie. operator. ok.
          pairEle$ = selPairE$(pair$, HEAD)
          opPar$ = opCheck$(pairEle$)
          lPair$ = OB$ + selPairE$(opPar$, HEAD) + " "
          eleCnt = 1
 
          IF selPairE$(opPar$, 2) = LF$ THEN
          'pass rest of list unevaled. ok.
            lPair$ = lPair$ + selPairE$(pair$, TAIL) + CB$
            EXIT DO
          END IF

        ELSE
        'evaluate all operands. ok.
          pair$ = OB$ + selPairE$(pair$, TAIL) + CB$
          pairEle$ = selPairE$(pair$, HEAD)
  
          IF pairEle$ = "" THEN
          'completed reading elements.
            lPair$ = RTRIM$(lPair$) + CB$
            EXIT DO
          END IF
          lPair$ = lPair$ + eForm$(pairEle$) + " "
          eleCnt = eleCnt + 1
        END IF
      LOOP

      evPair$ = applyOp$(lPair$)
    END IF
  END IF
  level = level - 1
END FUNCTION

FUNCTION fRead$ (void$)
'reads a LISP form checking the syntax and formatting the display.
' Created:
'  19/8/06
' Last Modified:
'  2/9/06
' Input:
'  number of forms evaluated in the current session.
' Output:
'  pre-checked and pre-formatted expression.
' <variable(s) created in this module>
'  <explanation of variable(s)>
' Notes:
'  add interactive nest level checking.
'  automatically indent nested forms and procedures.
'  convert case and symbols to internal representation.
'  2/9/06: modify delete to clear the end of the line.
'  7/9/06: convert none-strings to lower case characters.
'
' History:
'  <history of changes, date, version, etc. (main module only)>
'
 
  CONST UARROW = 72, DARROW = 80, LARROW = 75, RARROW = 77
  cCol = POS(0)
  cRow = CSRLIN
  hCol = 1
  iform$ = ""
  nLvl = 0
  qtread = 0
  rform$ = ""
  pChar$ = ""

  PRINT CHR$(175);

  DO
  'create input form.
   
    DO
    'get character. ok.
      cChar$ = INKEY$
    LOOP WHILE cChar$ = ""
    iChar = ASC(RIGHT$(cChar$, 1))
   
    SELECT CASE iChar
    'character based action.

    CASE ASC(OB$), ASC(CB$)
    'increase or decrease nest level. ok.

      IF qtread = 0 THEN
      'outside quote.
       
        IF iChar = ASC(OB$) THEN
        'increase.
          nLvl = nLvl + 1
          iform$ = iform$ + cChar$
          PRINT OB$;
         
        ELSE
        'decrease.
          nLvl = nLvl - 1
          iform$ = iform$ + cChar$
          PRINT CB$;
        END IF
     
      ELSE
      'same level.
       
        IF cChar$ = OB$ THEN
          PRINT OB$;
         
        ELSE
          PRINT CB$;
        END IF
      END IF
      cCol = cCol + 1

    CASE 34
    'quote. ok.
      qtread = xNot(qtread)
      PRINT CHR$(34);
      cCol = cCol + 1

    CASE 13
    'carriage return. ok?

      IF nLvl = 0 THEN
      'valid form entered. ok.
        fRead$ = iform$ + rform$
        EXIT FUNCTION

      ELSE
      'nesting form. ok?
        cRow = cRow + 1
        hCol = hCol + 2
        cCol = hCol
        LOCATE cRow, cCol
      END IF

    CASE 8
    'delete character from current position.
      iform$ = LEFT$(iform$, LEN(iform$) - 1)
      cCol = cCol - 1
      LOCATE , cCol
      PRINT " "
      LOCATE , cCol

    CASE LARROW, RARROW
    'line edition...

      IF iChar = LARROW THEN
      'move cursor left and change accordingly.
        rform$ = RIGHT$(iform$, 1) + rform$
        iform$ = MID$(iform$, 1, LEN(iform$) - 1)
        cCol = cCol - 1
        LOCATE cRow, cCol
        
      ELSE
      'move cursor right and change accordingly.
        iform$ = iform$ + LEFT$(rform$, 1)
        rform$ = MID$(rform$, 2)
        cCol = cCol + 1
        LOCATE cRow, cCol
      END IF

    CASE UARROW, DARROW
    'access other forms previously entered...
   
    CASE ELSE
    'normal case. ok?
      iform$ = iform$ + cChar$
      PRINT cChar$; rform$;
      cCol = cCol + 1
     
    END SELECT
  LOOP
END FUNCTION

FUNCTION getDatype$ (typet$)
'finds the type of the given data.
'
' Created:
'  26/8/06
' Last Modified:
'  26/8/06
'
' Input:
'  data to be analysed.
' Output:
'  data type found after analysis.
'
' Notes:
'  Should later return a list of types from most specidic to the least.
'  27/8/06: add code to find all major types.
'  12/9/06: need to do further checking as atoms sometimes returned as
'   numbers.
'
' History:
'  27/8/06: slightly corrected number finding code.
'  added atom finding code.
'  7/9/06: modified number finding code.
'  added boolean finding code.
'  added procedure finding code.
'
 
  var$ = "variant"
  datype$ = var$
  datsize = LEN(typet$)
  datmark = 1
  estype$ = "variant"
  nLvl = 0
 
  IF MID$(typet$, datsize, 1) = CB$ THEN
  'list pair found. ok.
    getDatype$ = LP$
    EXIT FUNCTION
  END IF
 
  IF typet$ = LT$ OR typet$ = LF$ THEN
  'boolean type found. ok?
    getDatype$ = blnT$
    EXIT FUNCTION
  END IF

  DO
  'check for number. Give differentiating ability between number types.
    datchar$ = MID$(typet$, datmark, 1)

    SELECT CASE datchar$
    'search single extracted character.

    CASE "0", "1", "2", "3", "4", "5", "6", "7", "8", "9"
    'number found.
   
    CASE "."
    'period found.

      pntCnt = pntCnt + 1

      IF pntCnt > 1 THEN
      'not a number.
        datype$ = var$
        EXIT DO
      END IF
   
    CASE "-", "+"
    'signed number.
      
      IF datmark <> 1 THEN
      'not a signed number.
        datype$ = var$
        EXIT DO
      END IF

    CASE ELSE
    'non-numeric character.
      datype$ = var$
      EXIT DO

    END SELECT
    datmark = datmark + 1

    IF datmark > datsize THEN
    'finished reading data.
      getDatype$ = numT$
      EXIT FUNCTION
    END IF
  LOOP

  IF INSTR(typet$, ".") > 0 THEN
  'atomic pair found. ok?
    getDatype$ = AP$
    EXIT FUNCTION
  END IF

  IF datype$ = var$ THEN
  'search procedure memory. imported from evAtom. ok?
   
    SELECT CASE typet$
    'check predefined functions.
     
    CASE "+", "-", "*", "/", "\", "<", ">", "car", "cdr", "set", "cons"
      getDatype$ = procT$
      EXIT FUNCTION

    CASE "and", "or", "setq", "if", "eval"
      getDatype$ = procT$
      EXIT FUNCTION

    CASE "number?", "symbol?", "boolean?", "pair?", "null?", "procedure?"
      getDatype$ = procT$
      EXIT FUNCTION

    CASE ELSE
    'search procedure memory for user declared function.
    svarMem$ = selPairE$(varMem$, 2)
  
    DO
    'extract current procedures to test.
    tvarMem$ = selPairE$(svarMem$, 1)

      IF selPairE$(tvarMem$, 1) = typet$ THEN
      'procedure found, return.
        getDatype$ = procT$
        EXIT FUNCTION

      ELSE
      'procedure not found, discard current car and continue.
        svarMem$ = OB$ + selPairE$(svarMem$, -1) + CB$

        IF svarMem$ = EL$ THEN
        'move on if finished searching functions.
          EXIT DO
        END IF
      END IF
    LOOP

    END SELECT
  END IF
 
  IF datype$ = var$ THEN
  'currently dafaults to atom type. ok.
    getDatype$ = atmT$
  END IF
END FUNCTION

FUNCTION lenPair (pair$)
'finds the length of a pair using an extension of the pair selector. ok.
' Created:
'  29/8/06
' Last Modified:
'  29/8/06
'
' Input:
'  pair to analyse.
' Output:
'  length of given pair.
'
' Notes:
'
' History:
'

lenPair = VAL(selPairE$(pair$, 0))
END FUNCTION

FUNCTION opCheck$ (operator$)
'performs a preliminary check on an operator to find if its a form or a
'lambda procedure then prepares a preliminary list with the data. To be
'modified each time a new function is added.
'
' Created:
'  20/8/06
' Last Modified:
'  20/8/06
'
' Input:
'  first element of a list.
' Output:
'  list of operator name & type, # of operands expected.
'
' Notes:
'  26/8/06: indirectly recursive if given a list operand! Prevents the
'   passage of a list as an operator.
'  28/8/06: modify to cons outside case.
'  10/9/06: modify to check for lambda expression before attempting to
'   evaluate a list.
'
' History:
'  5/9/06: temporarily put setq with or & and.
'  12/9/06: put setq by itself.
'  fixed bugs in list evaluation.
'

  DO
  'get lambda or primitive procedure.
 
  DO
  'evaluate any list that is not a lambda procedure.
 
    IF getDatype$(operator$) = LP$ THEN
    'check and manage list. ok?
   
      IF selPairE$(operator$, 1) = "lambda" THEN
      'defined procedure found. ok?
        opAddLen$ = consPair$(selPairE$(selPairE$(operator$, 2), 0), EL$)
        opAddEvOpt$ = consPair$(LF$, opAddLen$)
        opCheck$ = consPair$(operator$, opAddEvOpt$)
        EXIT FUNCTION
 
      ELSE
        operator$ = evPair$(operator$)
      END IF
   
    ELSE
    'not list.
      EXIT DO
    END IF
  LOOP
 
  IF opType = 0 THEN
  'check for primitive operator.
 
    SELECT CASE operator$
    'gets info for the appropriate primitive procedure or special form.

    CASE "+", "-", "*", "/", "\", "<", ">"
    'unlimited argument procedures.
      opLen$ = "-1"
      opEvOpt$ = LT$

    CASE "car", "cdr", "eval", "abs", "cos", "tan", "sin", "arctan"
    'single argument procedures.
      opLen$ = "1"
      opEvOpt$ = LT$

    CASE "set", "cons", "=", "eq?", "eqv?", "equal?", "x-power-y", "x-root-y"
    'double argument procedures.
      opLen$ = "2"
      opEvOpt$ = LT$

    CASE "and", "or"
    'special forms. To put setq in its own case.
      opLen$ = "-1"
      opEvOpt$ = LF$

    CASE "if"
    '3 argument forms.
      opLen$ = "3"
      opEvOpt$ = LF$
   
    CASE "number?", "symbol?", "boolean?", "pair?", "null?", "procedure?"
    'procedure prediates.
      opLen$ = "1"
      opEvOpt$ = LT$

    CASE "lambda"
    'defining a lambda procedure.
      opLen$ = "2"
      opEvOpt$ = LF$
   
    CASE "setq"
    '2 argument special forms.
      opLen$ = "2"
      opEvOpt$ = LF$
  
    CASE ELSE
    'not primitive, search for definition.
      operator$ = evAtom$(operator$) 'recheck to probably use eForm.
      opType = 1
    END SELECT
   
    IF opType = 0 THEN
    'found primitive. recheck!
      opCheck$ = consPair$(operator$, consPair$(opEvOpt$, consPair$(opLen$, EL$)))
      EXIT DO
    END IF
  END IF
  opType = 0
  LOOP
  EXIT FUNCTION

'probably obsolete code.
    DO
    'check for definition with pair selector.
      sprocMem$ = selPairE$(varMem$, 2)
      finSrch = 0

      DO
      'search for symbol to check.
        testOp$ = selPairE$(sprocMem$, 1)
      
        IF selPairE$(testOp$, 1) = operator$ THEN
        'get operator by searching the 1st element of the 2nd variable store.
          opCheck$ = testOp$
          EXIT FUNCTION

        ELSE
        'symbol not found, discard current car and continue.
          sprocMem$ = OB$ + selPairE$(sprocMem$, -1) + CB$

          IF sprocMem$ = EL$ THEN
          'check if list is empty to move on.
            opCheck$ = errSig$("proc")
            EXIT FUNCTION
          END IF
        END IF
       LOOP WHILE finSrch = 0
    LOOP

END FUNCTION

FUNCTION selPairE$ (pair$, selE)
'returns the indicated portion of a pair. ok?
' Created:
'  19/8/06
' Last Modified:
'  19/8/06
' Input:
'  str pair to access.
'  int part to return: -# = rest w/out # of firsts, 1 = first, 2 = second...
' Output:
'  selected portion of the pair.
' <variable(s) created in this module>
'  <explanation of variable(s)>
' Notes:
'  modify to distinguish between dotted and list pairs.
'  20/8/06: returns empty string for erroneous input.
'  27/8/06: modify to return a count of the elements.
'  2/9/06: must note that cdr requests are returned w/out brackets. This
'   makes it compatible with dotted pairs. Can modify to rebracket lists by
'   checking on the data type before doing so.
'
' History:
'  20/8/06: modified to check the type of pair.
'   modified to return empty string for out of range selections.
'  27/8/06: corrected cdr finding error.
'  corrected code bugs.
'  3/9/06: modifed cdr returner to just read the rest of the string after
'   passing # of elements.
'

  datype$ = getDatype$(pair$)

  IF (datype$ = AP$ OR datype$ = LP$) AND (pair$ <> EL$) THEN
  'check for atomic dotted or list pair.
    rdPos = 1
    eRdPos = LEN(pair$)
    eleNum = 0

    IF datype$ = LP$ THEN
    'eliminate enclosing brackets if is a list. ok.
      rdPos = 2
      eRdPos = eRdPos - 1
    END IF
    retPair$ = ""

    DO
    'get elements. ok?

      FOR inc = rdPos TO eRdPos
      'read element if available. ok.
        eChar$ = MID$(pair$, inc, 1)
  
        IF qtread = 0 AND nestCnt = 0 AND ((datype$ = LP$ AND eChar$ = " ") OR (datype$ = AP$ AND eChar$ = ".")) THEN
        'pair element ends, increment element counter and leave for loop.
          EXIT FOR

        ELSE

          IF nestCnt = 0 AND eChar$ = CHR$(34) THEN
          'string starts or ends.
            qtread = xNot(qtread)
            curEle$ = curEle$ + eChar$

          ELSE

            IF qtread = 0 AND eChar$ = OB$ THEN
            'nested list begins.
              nestCnt = nestCnt + 1
              curEle$ = curEle$ + eChar$

            ELSE

              IF qtread = 0 AND eChar$ = CB$ THEN
              'nested list ends.
                nestCnt = nestCnt - 1
                curEle$ = curEle$ + eChar$

              ELSE

                'normal circumstances.
                curEle$ = curEle$ + eChar$
                ''PRINT "element: "; curEle$
              END IF
            END IF
          END IF
        END IF
      NEXT
      eleNum = eleNum + 1
      rdPos = inc + 1

      IF ABS(selE) > eleNum AND inc > eRdPos THEN
      'selection out of range, return empty. ok.
        selPairE$ = ""
        EXIT FUNCTION
      END IF

      SELECT CASE selE
      'first or rest selection.

      CASE IS > 0
      'gets specified element.

        IF eleNum = selE THEN
        'just got requested element. ok.
          selPairE$ = curEle$
          EXIT FUNCTION
        END IF
        curEle$ = ""

      CASE IS < 0
      'gets pair w/out specified # of firsts.
       
        IF eleNum = ABS(selE) THEN
        'get requested cdr w/out single additions. new!
          cdrSize = eRdPos - rdPos + 1
         
          IF cdrSize < 1 THEN
          'list has under 2 elements.
            selPairE$ = ""
            EXIT FUNCTION

          ELSE
          'cdr isn't empty.
            selPairE$ = MID$(pair$, rdPos, cdrSize)
            EXIT FUNCTION
          END IF
       
        ELSE
          curEle$ = ""
        END IF

        IF inc > eRdPos THEN 'now obsolete?
        'remove trailing space and return when finished reading pair elements.
          STOP 'only reaches if section is accessed.
          selPairE$ = RTRIM$(retPair$)
          EXIT FUNCTION
        END IF

      CASE ELSE
      'no portion specified.
       
        IF inc > eRdPos THEN
        'return the length of the list.
          selPairE$ = xStr$(eleNum)
          EXIT FUNCTION
        END IF
        
      END SELECT
    LOOP
 
  ELSE
  'not a pair.
    selPairE$ = ""
  END IF
  EXIT FUNCTION

  'obsolete code...
        IF eleNum > ABS(selE) THEN
        'start creating pair when past excludes.
          retPair$ = retPair$ + curEle$ + " "
          curEle$ = ""
      
        ELSE
          curEle$ = ""
        END IF

END FUNCTION

FUNCTION xMid$ (string1$, start1, stop1)
'extended version of mid to return a substring given start and stop
'positions.
' Created:
'  10/9/06
' Last Modified:
'  10/9/06
' Input:
'  string to search.
'  start position in string.
'  stop position in string.
' Output:
'  substring if found.
'
' Notes:
'  substring can be reversed by making the stop smaller than the start.
'  a null string is returned for out of range positions.
'

  strStart = 1
  strStop = LEN(string1$)
  retString$ = ""
  dirStep = 1

  IF string1$ = "" THEN
  'return null string
    xMid$ = ""
    EXIT FUNCTION
  END IF

  SELECT CASE start1

  CASE IS < 1, IS > strStop
    xMid$ = ""
    EXIT FUNCTION

  CASE IS > stop1
    dirStep = -1

  CASE IS = stop1
    xMid$ = MID$(string1$, start1, 1)
    EXIT FUNCTION
  END SELECT

  SELECT CASE stop1

  CASE IS < 1, IS > strStop
    xMid$ = ""
    EXIT FUNCTION
 
  END SELECT
 
  FOR inc = start1 TO stop1 STEP dirStep
  'create substring.
    retString$ = retString$ + MID$(string1$, inc, 1)
  NEXT
  xMid$ = retString$
END FUNCTION

FUNCTION xNot (bool)
'boolean operator modified to override qbasic bug. ok.
' Created:
'  1/8/06
' Last Modified:
'  1/8/06
' Input:
'  int to apply not operation.
' Output:
'  not result of expression passed.
'

  IF bool = 0 THEN
  'check if the expression is 0.
    xNot = -1
  ELSE
  'returns a 0.
    xNot = 0
  END IF
END FUNCTION

FUNCTION xRange (NUM, lval, uval)
'checks if a number falls within range of the given values. ok.
' Created:
'  1/8/06
' Last Modified:
'  1/8/06
' Input:
'  number to check.
'  lower value.
'  upper value.
' Output:
'  1 if the number is in range, 0 otherwise.
' <variable(s) created in this module>
'  <explanation of variable(s)>
' Notes:
'  <features to implement, known bugs, operation, etc. (main module only)>
' History:
'  <history of changes, date, version, etc. (main module only)>
'
  IF NUM >= lval OR NUM <= uval THEN
  'check if in range.
    xRange = 1

  ELSE
    xRange = 0
  END IF

END FUNCTION

FUNCTION xStr$ (value)
'remove space from in front strings converted from numbers. ok.
' Created:
'  <unknown>
' Last Modified:
'  1/7/06
' Input:
'  number to convert.
' Output:
'  numeric string.
'
' Notes:
'  27/8/06: converts a number to a string then removes the space in front.
'

  xStr$ = LTRIM$(STR$(value))
END FUNCTION
