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
            DISPLAY "Processing input..."
            CALL "InputHandeling" USING INPUT-VEKTOR, ABBRUCH, MATRIX

            DISPLAY "Validating input..."
            CALL "Validation" USING MATRIX, ABBRUCH

            DISPLAY "Build CRS..."
            CALL "CompressedRowStorage" USING CRS-VALS, CRS-COLS,
                  CRS-ROW-PTR, ABBRUCH, MATRIX

            DISPLAY "Vectoriteration..."
            CALL "Vectoriteration" USING CRS-VALS, CRS-COLS,
                  CRS-ROW-PTR, INPUT-VEKTOR, ABBRUCH

            DISPLAY "Output..."
            CALL "OutputHandeling" USING INPUT-VEKTOR, ABBRUCH, MATRIX

      * Retrun code setzen?
            STOP RUN.
       END PROGRAM MAIN.
