      ******************************************************************
      * Author:
      * Date:
      * Purpose:
      * Tectonics: cobc
      ******************************************************************
       IDENTIFICATION DIVISION.
       PROGRAM-ID. OutputHandling.
       ENVIRONMENT DIVISION.
       INPUT-OUTPUT SECTION.
           FILE-CONTROL.
               SELECT OUTPUTF ASSIGN TO "output.txt"
               FILE STATUS IS FILE-STATUS
               ORGANIZATION IS LINE SEQUENTIAL
               ACCESS MODE IS SEQUENTIAL.
       DATA DIVISION.
       FILE SECTION.
           FD OUTPUTF
           DATA RECORD IS OUTPUT-FILE.
           01 OUTPUT-FILE PIC X(200).
       WORKING-STORAGE SECTION.
           01 FILE-STATUS PIC XX.
           01 COUNTER PIC 9(4) COMP-3 VALUE 1.
           01 WS-NUM PIC S9(06)V9(16).
           01 PRINT PIC X(200).
           01 PRETTY-NUM PIC -Z(8)9.9999.
           01 I PIC 9(4).
           01 J PIC 9(4).
           01 P PIC 9(4).
       LINKAGE SECTION.
           COPY "InputMatrix.cpy".
           COPY "Abbruch.cpy".
           COPY "VectorDim.cpy".
       PROCEDURE DIVISION USING INPUT-VEKTOR ABBRUCH MATRIX.
       MAIN-PROCEDURE.
           IF ERRORMSG = "CLEAR" THEN
            OPEN OUTPUT OUTPUTF
            CLOSE OUTPUTF
            EXIT PROGRAM
           END-IF

            OPEN EXTEND OUTPUTF
            IF FILE-STATUS NOT = '00'
               THEN PERFORM HANDLE-ERROR STOP RUN
            END-IF

            WRITE OUTPUT-FILE FROM "=========="

            IF ERRORMSG = "FINE" THEN
                PERFORM PRINT-OUTPUT
                ELSE PERFORM PRINT-ERROR
            END-IF

            WRITE OUTPUT-FILE FROM "=========="

            IF FILE-STATUS NOT = '00'
               THEN PERFORM HANDLE-ERROR STOP RUN
            END-IF
            CLOSE OUTPUTF
            EXIT PROGRAM
            .

       PRINT-ERROR.
           WRITE OUTPUT-FILE FROM ERRORMSG
           .
       PRINT-OUTPUT.
           WRITE OUTPUT-FILE FROM "Matrix:"
            MOVE 1 TO I
            PERFORM UNTIL I > DIM-M
               MOVE 1 TO J
               MOVE 1 TO P
               MOVE SPACE TO PRINT
               PERFORM UNTIL J > DIM-M
                   MOVE ELEM(I,J) TO PRETTY-NUM
                   STRING PRETTY-NUM " " INTO PRINT WITH POINTER P
                  ADD 1 TO J
               END-PERFORM
               ADD 1 TO I
            WRITE OUTPUT-FILE FROM PRINT
            END-PERFORM

            WRITE OUTPUT-FILE FROM "Start Vektor:"
            MOVE 1 TO I
            MOVE SPACE TO PRINT
            MOVE 1 TO P
            PERFORM UNTIL I > DIM-I
                MOVE XI-ORIG(I) TO WS-NUM
                MOVE WS-NUM TO PRETTY-NUM
                STRING PRETTY-NUM " " INTO PRINT WITH POINTER P
                ADD 1 TO I
            END-PERFORM
            WRITE OUTPUT-FILE FROM PRINT

            WRITE OUTPUT-FILE FROM "Erwartungswert:"
            MOVE RET-EW TO PRETTY-NUM
            MOVE PRETTY-NUM TO PRINT
            WRITE OUTPUT-FILE FROM PRINT
           .

      * gibt einige fehlermeldungen fuer haeufige file errors aus
       HANDLE-ERROR.
           IF FILE-STATUS = '35'
           THEN STRING "Die Ausgabedatei konnte nicht gefunden werden!"
               INTO ERRORMSG
               DISPLAY ERRORMSG
           ELSE STRING "Fehler " FILE-STATUS INTO ERRORMSG
               DISPLAY ERRORMSG
           CLOSE OUTPUTF
           EXIT PROGRAM
           .

       END PROGRAM OutputHandling.
