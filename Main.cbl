      ******************************************************************
      * Author:
      * Date:
      * Purpose:
      * Tectonics: cobc
      ******************************************************************
       IDENTIFICATION DIVISION.
       PROGRAM-ID. MAIN.
       DATA DIVISION.
       WORKING-STORAGE SECTION.
           01 END-DATA PIC 9 VALUE 0.
           COPY "CRS.cpy".
           COPY "Abbruch.cpy".
           COPY "VectorDim.cpy".
           COPY "InputMatrix.cpy".
           COPY "OutputVector.cpy".
       PROCEDURE DIVISION.
       MAIN-PROCEDURE.
      * output datei leeren
           MOVE "CLEAR" TO ERRORMSG
           CALL "OutputHandling" USING INPUT-VEKTOR, ABBRUCH, MATRIX
           MOVE "FINE" TO ERRORMSG

           PERFORM UNTIL END-DATA = 2
            DISPLAY "================================="
            MOVE "FINE" TO ERRORMSG
            INITIALIZE CRS-COLS
            INITIALIZE CRS-ROW-PTR
            INITIALIZE MATRIX
            INITIALIZE OUT-VEKTOR
            INITIALIZE INPUT-VEKTOR

            DISPLAY "Processing input..."
            CALL "InputHandling" USING INPUT-VEKTOR, ABBRUCH, MATRIX,
            SKIP

            IF ERRORMSG = "FINE" THEN
            DISPLAY "Validating input..."
            CALL "Validation" USING MATRIX, ABBRUCH
            END-IF

            IF ERRORMSG = "FINE" THEN
            DISPLAY "Build CRS..."
            CALL "CompressedRowStorage" USING CRS-VALS, CRS-COLS,
                  CRS-ROW-PTR, MATRIX
            END-IF

            IF ERRORMSG = "FINE" THEN
            DISPLAY "Vectoriteration..."
            CALL "Vectoriteration" USING CRS-VALS, CRS-COLS,
                  CRS-ROW-PTR, INPUT-VEKTOR, ABBRUCH
            END-IF

            DISPLAY "Output..."
            CALL "OutputHandling" USING INPUT-VEKTOR, ABBRUCH, MATRIX

            ADD 1 TO END-DATA
            ADD 1 TO SKIP
            DISPLAY "================================="
           END-PERFORM

            STOP RUN.
       END PROGRAM MAIN.
