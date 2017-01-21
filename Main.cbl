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
           COPY "CRS.cpy".
           COPY "Abbruch.cpy".
           COPY "VectorDim.cpy".
           COPY "InputMatrix.cpy".
       PROCEDURE DIVISION.
       MAIN-PROCEDURE.
            MOVE "FINE" TO ERRORMSG

            DISPLAY "Processing input..."
            CALL "InputHandling" USING INPUT-VEKTOR, ABBRUCH, MATRIX

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

            STOP RUN.
       END PROGRAM MAIN.
