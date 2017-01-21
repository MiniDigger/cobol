      ******************************************************************
      * Author:
      * Date:
      * Purpose:
      * Tectonics: cobc
      ******************************************************************
       IDENTIFICATION DIVISION.
       PROGRAM-ID. InputHandling.
       ENVIRONMENT DIVISION.
       INPUT-OUTPUT SECTION.
           FILE-CONTROL.
               SELECT INPUTF ASSIGN TO "input.txt"
               FILE STATUS IS FILE-STATUS
               ORGANIZATION IS LINE SEQUENTIAL.
       DATA DIVISION.
       FILE SECTION.
           FD INPUTF
           DATA RECORD IS INPUT-FILE.
           01 INPUT-FILE.
               05 INPUT-LINE PIC X(80).
       WORKING-STORAGE SECTION.
      *    FILE-EOF fungiert als boolean um bis zum ende der Datei zu iterieren
           01 FILE-EOF PIC 9 VALUE 0.
           01 FILE-STATUS PIC XX.
           01 COUNTER PIC 99 VALUE 0.
           01 DUMMY PIC X(80).
           01 OPTION-FOUND PIC 9.
           01 VECTOR-POINTER PIC 999.
           01 ROW PIC 9999 VALUE 1.
           01 SKIP-COPY PIC 999 VALUE 0.
       LINKAGE SECTION.
           COPY "InputMatrix.cpy".
           COPY "Abbruch.cpy".
           COPY "VectorDim.cpy".
       PROCEDURE DIVISION USING INPUT-VEKTOR ABBRUCH MATRIX SKIP.
       MAIN-PROCEDURE.
           INITIALIZE INPUT-VEKTOR
           OPEN INPUT INPUTF
           IF FILE-STATUS NOT = '00'
                THEN PERFORM HANDLE-ERROR STOP RUN.

           MOVE SKIP TO SKIP-COPY
      *  jede zeile einzeln einlesen
           PERFORM UNTIL FILE-EOF=1
              READ INPUTF
                  AT END  MOVE 1 TO FILE-EOF
                  NOT AT END PERFORM READLINE
              END-READ
           END-PERFORM
           MOVE 0 TO FILE-EOF
           MOVE 1 TO ROW
           MOVE 1 TO VECTOR-POINTER
           ADD 1 TO SKIP
           CLOSE INPUTF
           EXIT PROGRAM
           .
      * liest eine zeile und entscheidet wie sie weiter verarbeitet werden soll
       READLINE.
           DISPLAY INPUT-LINE
           MOVE 0 TO OPTION-FOUND

           MOVE 0 TO COUNTER
           INSPECT INPUT-LINE TALLYING COUNTER FOR LEADING "==========".
           IF COUNTER > 0 THEN PERFORM FOUND-NEW MOVE 1 TO OPTION-FOUND
           END-IF
      * solange skippen bis wir am richtigen record sind
           IF SKIP-COPY NOT = 0 THEN MOVE 1 TO OPTION-FOUND END-IF

      * werte für dim, n, epsilon und den start vektor
           MOVE 0 TO COUNTER
           INSPECT INPUT-LINE TALLYING COUNTER FOR LEADING "dim=".
           IF COUNTER > 0 THEN PERFORM FOUND-DIM MOVE 1 TO OPTION-FOUND
           END-IF
           MOVE 0 TO COUNTER
           INSPECT INPUT-LINE TALLYING COUNTER FOR LEADING "n=".
           IF COUNTER > 0 THEN PERFORM FOUND-N MOVE 1 TO OPTION-FOUND
           END-IF
           MOVE 0 TO COUNTER
           INSPECT INPUT-LINE TALLYING COUNTER FOR LEADING "e=".
           IF COUNTER > 0 THEN PERFORM FOUND-E MOVE 1 TO OPTION-FOUND
           END-IF
           MOVE 0 TO COUNTER
           INSPECT INPUT-LINE TALLYING COUNTER FOR LEADING "x=".
           IF COUNTER > 0 THEN PERFORM FOUND-X MOVE 1 TO OPTION-FOUND
           END-IF
      * else: eine weitere zeile der input matrix
           IF OPTION-FOUND = 0
             MOVE 1 TO VECTOR-POINTER
             MOVE 1 TO COUNTER
             PERFORM UNTIL COUNTER > DIM-M
               UNSTRING INPUT-LINE DELIMITED BY SPACES INTO DUMMY
               WITH POINTER VECTOR-POINTER END-UNSTRING
      *       IF DUMMY IS NUMERIC THEN
               MOVE FUNCTION NUMVAL(DUMMY) TO ELEM(ROW,COUNTER)
      *       ELSE
      *          DISPLAY "Error: matrix " COUNTER " " DUMMY " ist nicht"
      *          " numerisch!"
      *          CLOSE INPUTF
      *          EXIT PROGRAM
      *       END-IF  TODO BESSERE FEHLERBEHANDLUNG MATRIX PARSING
             MOVE SPACES TO DUMMY
             ADD 1 TO COUNTER
             END-PERFORM
             ADD 1 TO ROW
           END-IF
           .
      * liest die dimension der matrix ein
       FOUND-DIM.
      * Prefix eliminieren
           INSPECT INPUT-LINE REPLACING ALL "dim=" BY SPACE
      * Zum sicherstellen das der String numerisch ist
           INSPECT INPUT-LINE REPLACING LEADING SPACE BY ZEROES
           MOVE FUNCTION NUMVAL(INPUT-LINE) TO DIM-I
           MOVE FUNCTION NUMVAL(INPUT-LINE) TO DIM-M
           IF DIM-I = 0 THEN
               STRING "Error:" INPUT-LINE DELIMITED BY SPACE
               ". dim ist nicht numerisch oder ist 0!" INTO ERRORMSG
               DISPLAY ERRORMSG
               CLOSE INPUTF
               EXIT PROGRAM
               ELSE DISPLAY "Found dim " DIM-I
           END-IF
           .
      * liest den wert fuer nmax ein
       FOUND-N.
      * Prefix eliminieren
           INSPECT INPUT-LINE REPLACING ALL "n=" BY SPACE
      * Zum sicherstellen das der String numerisch ist
           INSPECT INPUT-LINE REPLACING LEADING SPACE BY ZEROES
           MOVE FUNCTION NUMVAL(INPUT-LINE) TO N
           IF N = 0 THEN
               STRING "Error:" INPUT-LINE DELIMITED BY SPACE
               ". n ist nicht numerisch oder ist 0!" INTO ERRORMSG
               DISPLAY ERRORMSG
               CLOSE INPUTF
               EXIT PROGRAM
           ELSE DISPLAY "Found n " N
           END-IF
           .
      * liest epsilon ein
       FOUND-E.
      * Prefix eliminieren
           INSPECT INPUT-LINE REPLACING ALL "e=" BY SPACE
      * Zum sicherstellen das der String numerisch ist
           INSPECT INPUT-LINE REPLACING LEADING SPACE BY ZEROES
           MOVE FUNCTION NUMVAL(INPUT-LINE) TO EPSILON
           IF EPSILON = 0 THEN
               STRING "Error:" INPUT-LINE DELIMITED BY SPACE
               ". e ist nicht numerisch oder ist 0!" INTO ERRORMSG
               DISPLAY ERRORMSG
               CLOSE INPUTF
               EXIT PROGRAM
           ELSE DISPLAY "Found e " EPSILON
           END-IF
           .
      * liest den startvektor ein
       FOUND-X.
      * Prefix eliminieren
           INSPECT INPUT-LINE REPLACING ALL "x=" BY SPACE
      * Zum sicherstellen das der String numerisch ist
           INSPECT INPUT-LINE REPLACING LEADING SPACE BY ZEROES

           MOVE 1 TO VECTOR-POINTER
           MOVE 1 TO COUNTER
      * splittet den string bei leerzeichen ein
      * iteriert dann durch alle parts
          PERFORM UNTIL COUNTER > DIM-I
             UNSTRING INPUT-LINE DELIMITED BY SPACES INTO DUMMY
             WITH POINTER VECTOR-POINTER END-UNSTRING
      *       IF DUMMY IS NUMERIC THEN
             MOVE FUNCTION NUMVAL-C(DUMMY) TO XI(COUNTER)
             MOVE XI(COUNTER) TO XI-ORIG(COUNTER)
      *       ELSE
      *          DISPLAY "Error: xi " COUNTER " " DUMMY " ist nicht"
      *          " numerisch!"
      *          CLOSE INPUTF
      *          EXIT PROGRAM
      *       END-IF  TODO BESSERE FEHLERBEHANDLUNG XI PARSING
             MOVE SPACES TO DUMMY
             ADD 1 TO COUNTER
           END-PERFORM
           .
      * ende einer input zeile
       FOUND-NEW.
           DISPLAY "zuende gelese"
           SUBTRACT 1 FROM SKIP-COPY
           DISPLAY SKIP-COPY
           CLOSE INPUTF
           EXIT PROGRAM
           .
      * gibt einige fehlermeldungen fuer haeufige file errors aus
       HANDLE-ERROR.
           IF FILE-STATUS = '35'
           THEN STRING "Die Eingabedatei konnte nicht gefunden werden!"
               INTO ERRORMSG
               DISPLAY ERRORMSG
           ELSE STRING "Fehler " FILE-STATUS INTO ERRORMSG
               DISPLAY ERRORMSG
           CLOSE INPUTF
           EXIT PROGRAM
           .
       END PROGRAM InputHandling.
