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
           01 PERCENTAGE PIC 99V9(2) VALUE 0.
       LINKAGE SECTION.
           COPY "InputMatrix.cpy".
           COPY "Abbruch.cpy".
       PROCEDURE DIVISION USING MATRIX, ABBRUCH.
       MAIN-PROCEDURE.
            PERFORM UNTIL I > DIM-M
                MOVE 1 TO J
                PERFORM UNTIL J > DIM-M
                    IF ELEM(I,J) = 0
                        THEN ADD 1 TO ZERO-COUNTER
                    END-IF

                    IF ELEM(I,J) NOT = ELEM(J,I) THEN
             STRING "Matrix nicht symmtrisch " I " " J "!" INTO ERRORMSG
                    DISPLAY ERRORMSG
                    EXIT PROGRAM
                    END-IF
                    ADD 1 TO J
                END-PERFORM
                ADD 1 TO I
            END-PERFORM
            COMPUTE PERCENTAGE = (ZERO-COUNTER * 100) / (DIM-M*DIM-M)

            IF PERCENTAGE < 30 THEN
                STRING "Matrix ist nicht duenn besetzt, nur " PERCENTAGE
                "% der Elemente sind 0" INTO ERRORMSG
                DISPLAY ERRORMSG
                EXIT PROGRAM
            END-IF
            .
       END PROGRAM Validation.
