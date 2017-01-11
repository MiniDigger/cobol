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
           01 OUTPUT-FILE PIC X(100).
       WORKING-STORAGE SECTION.
           01 FILE-STATUS PIC XX.
           01 COUNTER PIC 9(4) COMP-3 VALUE 1.
           01 WS-RET-EW PIC S9(06)V9(16).
           01 PRINT PIC X(100).
           01 I PIC 9(4).
           01 J PIC 9(4).
           01 P PIC 9(4).
       LINKAGE SECTION.
           COPY "VectorDim.cpy".
           COPY "Abbruch.cpy".
           COPY "InputMatrix.cpy".
       PROCEDURE DIVISION USING ABBRUCH INPUT-VEKTOR MATRIX.
       MAIN-PROCEDURE.
           MOVE RET-EW TO WS-RET-EW
      *     MOVE WS-RET-EW TO PRINT

            OPEN OUTPUT OUTPUTF
            IF FILE-STATUS NOT = '00'
               THEN PERFORM HANDLE-ERROR STOP RUN
            END-IF

            WRITE OUTPUT-FILE FROM "=========="
            WRITE OUTPUT-FILE FROM "Matrix:"

            MOVE 1 TO I
            PERFORM UNTIL I > DIM-M
               MOVE 1 TO J
               MOVE 1 TO P
               MOVE "" TO PRINT
               PERFORM UNTIL J > DIM-M
                  STRING ELEM(I,J) " " INTO PRINT WITH POINTER P
               END-PERFORM
            WRITE OUTPUT-FILE FROM PRINT
            END-PERFORM

            WRITE OUTPUT-FILE FROM "Start Vektor:"
            WRITE OUTPUT-FILE FROM "Erwartungswert:"
            MOVE "test" TO PRINT
            WRITE OUTPUT-FILE FROM PRINT
            WRITE OUTPUT-FILE FROM "WS-RET-EW"
            WRITE OUTPUT-FILE FROM "=========="

            IF FILE-STATUS NOT = '00'
               THEN PERFORM HANDLE-ERROR STOP RUN
            END-IF
            CLOSE OUTPUTF
            STOP RUN
            .

      * gibt einige fehlermeldungen fuer haeufige file errors aus
       HANDLE-ERROR.
      *    TODO Mehr Fehler abfangen
           IF FILE-STATUS = '35'
           THEN DISPLAY "Die Ausgabedatei konnte nicht gefunden werden!"
           ELSE DISPLAY "Fehler " FILE-STATUS
           CLOSE OUTPUTF
           STOP RUN
           .

       END PROGRAM OutputHandling.
