      ******************************************************************
      * Author:
      * Date:
      * Purpose:
      * Tectonics: cobc
      ******************************************************************
       IDENTIFICATION DIVISION.
       PROGRAM-ID. Validation.
       DATA DIVISION.
       FILE SECTION.
       WORKING-STORAGE SECTION.
           01 ZERO-COUNTER PIC 9(10) VALUE 0.
           01 I PIC 9(4) VALUE 1.
           01 J PIC 9(4) VALUE 1.
       LINKAGE SECTION.
           COPY "InputMatrix.cpy".
           COPY "Abbruch.cpy".
       PROCEDURE DIVISION USING MATRIX, ABBRUCH.
       MAIN-PROCEDURE.
            PERFORM UNTIL I > DIM-M
                PERFORM UNTIL J > DIM-M
                    IF ELEM(I,J) = 0
                        THEN ADD 1 TO ZERO-COUNTER
                    END-IF

                    IF ELEM(I,J) NOT = ELEM(J,I) THEN
             STRING "Matrix nicht symmtrisch " I " " J "!" INTO ERRORMSG
                    DISPLAY ERRORMSG
                    STOP RUN
                    END-IF
                END-PERFORM
            END-PERFORM.
       END PROGRAM Validation.
