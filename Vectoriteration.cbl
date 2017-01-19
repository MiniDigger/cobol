      ******************************************************************
      * Author: Domenic Kuhene
      * Date: 25.09.2016
      * Tectonics: cobc
      ******************************************************************
       IDENTIFICATION DIVISION.
       PROGRAM-ID. Vectoriteration.
       DATA DIVISION.
       FILE SECTION.
       WORKING-STORAGE SECTION.
           01 EIGENWERT.
               05 LAST-EW PIC S9(09)V9(16) COMP-3.
               05 EW PIC S9(09)V9(16) COMP-3.
           01 ABBRUCH-KRIT.
               05 ABS-DIF PIC S9(09)V9(16) COMP-3.
               05 ABS-EW PIC S9(09)V9(16) COMP-3.
           Copy "OutputVector.cpy".
           01 ZAEHLER.
               05 COUNTER-N PIC 9(06) COMP-3.
               05 COUNTER-EW PIC 9 COMP-3.
               05 I PIC 9(04) COMP-3.
           01 SCALAR PIC 9(09)V9(16) COMP-3.
       LINKAGE SECTION.
           COPY "CRS.cpy".
           COPY "VectorDim.cpy".
           COPY "Abbruch.cpy".
       PROCEDURE DIVISION
            USING CRS-VALS CRS-COLS CRS-ROW-PTR INPUT-VEKTOR ABBRUCH.
       MAIN-PROCEDURE.
            IF DIM-VAL = 0
                DISPLAY "TODO, evtl. RET-CODE setzen fuer Nullmatrix"
                MOVE 0.0 TO RET-EW
            ELSE
               MOVE DIM-I TO DIM-O
               MOVE 1 TO COUNTER-N
               MOVE 0 TO COUNTER-EW

      *        Erster Durchlauf um einen Wert fuer ew zu bekommen, damit
      *        anschliessend Abbruchkriterien geprueft werden koennen
               MOVE 0.0 TO SCALAR
               PERFORM VARYING I FROM 1 BY 1 UNTIL I > DIM-I
                  COMPUTE SCALAR = SCALAR + XI(I)*XI(I)
               END-PERFORM
               COMPUTE SCALAR = SCALAR ** 0.5
      *        Normalisierung des Vektors
               PERFORM VARYING I FROM 1 BY 1 UNTIL I > DIM-I
                  COMPUTE XI(I) = XI(I) / SCALAR
               END-PERFORM
               CALL "CRSMatrixVectorMultiply" USING CRS-VALS,
                        CRS-COLS, CRS-ROW-PTR, INPUT-VEKTOR, OUT-VEKTOR
               PERFORM DOT
               MOVE SCALAR TO EW
               PERFORM NORMALIZE

               PERFORM UNTIL COUNTER-N > N OR COUNTER-EW > 2
                  MOVE EW TO LAST-EW
                  CALL "CRSMatrixVectorMultiply" USING CRS-VALS,
                        CRS-COLS, CRS-ROW-PTR, INPUT-VEKTOR, OUT-VEKTOR
                  PERFORM DOT
                  MOVE SCALAR TO EW
                  PERFORM NORMALIZE

      *           Inkrementierung der Abbruchbedingung wenn noetig
                  MOVE EW TO ABS-EW
                  COMPUTE ABS-DIF = EW - LAST-EW
                  IF ABS-DIF < 0.0
                      COMPUTE ABS-DIF = ABS-DIF * (-1.0)
                  END-IF
                  IF ABS-EW < 0.0
                      COMPUTE ABS-EW = ABS-EW * (-1.0)
                  END-IF
                  IF ABS-DIF <= EPSILON * ABS-EW
                      COMPUTE COUNTER-EW = COUNTER-EW + 1
                  ELSE
                      MOVE 0 TO COUNTER-EW
                  END-IF
                  COMPUTE COUNTER-N = COUNTER-N + 1
      *           Ende der Abbruchbedingungen
               END-PERFORM
               MOVE EW TO RET-EW
            END-IF

      *     KANN ENTFERNT WERDEN!!!
            Display "Debug-Ausgabe in Vectoriteration.cbl"
            Display "Rueckgabe-EW: " RET-EW
            DISPLAY "Differenz der letzten beiden EW: " ABS-DIF
            display "Anzahl Iterationen: " COUNTER-N
            Display "End-Debug-Ausgabe in Vectoriteration.cbl"

            Exit PROGRAM.

      * Bildet das Skalarprodukt von XI und XK
       DOT.
           MOVE 0.0 TO SCALAR
           PERFORM VARYING I FROM 1 BY 1 UNTIL I > DIM-I
              COMPUTE SCALAR = SCALAR + XI(I) * XK(I)
           END-PERFORM
           .

      * Normalisiert den Vektor XK und schreibt ihn in direkt in XI
      * damit weiter iteriert werden kann.
       NORMALIZE.
           MOVE 0 TO SCALAR
           PERFORM VARYING I FROM 1 BY 1 UNTIL I > DIM-I
              COMPUTE SCALAR = SCALAR + XK(I) * XK(I)
           END-PERFORM
           COMPUTE SCALAR = SCALAR ** 0.5
           PERFORM VARYING I FROM 1 BY 1 UNTIL I > DIM-I
              COMPUTE XI(I) = XK(I) / SCALAR
           END-PERFORM
           .
