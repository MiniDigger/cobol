      ******************************************************************
      * Author: Domenic Kuehne
      * Date: 03.10.2016
      * Tectonics: cobc
      ******************************************************************
       IDENTIFICATION DIVISION.
       PROGRAM-ID. CompressedRowStorage.
       DATA DIVISION.
       FILE SECTION.
       WORKING-STORAGE SECTION.
           01 ZAEHLER.
               05 I PIC 9(03) COMP-3 VALUE 1.
               05 J PIC 9(03) COMP-3 VALUE 1.
               05 NUM-NOT-ZERO PIC 9(04) COMP-3 VALUE 0.
               05 COUNTER-NULL-ZEILE PIC 9(04) COMP-3 VALUE 0.
      *        IS-NULL-ZEILE fungiert als Boolean
               05 IS-NULL-ZEILE PIC 9 COMP-3 VALUE 1.
               05 KORREKTUR PIC S9(04) COMP-3 VALUE 0.
               05 VAL-COUNTER PIC 9(05) COMP-3 VALUE 1.
               05 COLS-COUNTER PIC 9(04) COMP-3 VALUE 1.
       LINKAGE SECTION.
           COPY "CRS.cpy".
           COPY "VectorDim.cpy".
           Copy "Abbruch.cpy".
           COPY "InputMatrix.cpy".
       PROCEDURE DIVISION USING CRS-VALS CRS-COLS CRS-ROW-PTR
                                ABBRUCH MATRIX.
       MAIN-PROCEDURE.
            PERFORM INITIALIZE-CRS

            MOVE VAL-COUNTER TO RPTR(1)
            MOVE 1 TO I
            PERFORM VARYING I FROM 1 BY 1 UNTIL I > DIM-M
      *        COLS-COUNTER kann immer nur Werte zwischen I und DIM-M
      *        annehmen, da nur alle Elemente oberhalb der Diagonale
      *        gespeichert werden sollen
               MOVE I TO COLS-COUNTER
               MOVE 1 TO IS-NULL-ZEILE
               MOVE I TO J
               PERFORM VARYING J FROM I BY 1 UNTIL J > DIM-M
                  IF NOT ELEM(I, J) = 0
      *                 Falls Nullzeilen vorhanden sind ist der
      *                 VAL-COUNTER um die Korrektur-Anzahl hoeher,
      *                 daher muss diese hier addiert werden da der Wert
      *                 stets <= 0 ist
                        MOVE ELEM(I, J) TO VAL(VAL-COUNTER + KORREKTUR)
                        MOVE COLS-COUNTER TO COLMN(VAL-COUNTER)
                        Compute VAL-COUNTER = VAL-COUNTER + 1
                        MOVE 0 TO IS-NULL-ZEILE
                     END-IF
                     COMPUTE COLS-COUNTER = COLS-COUNTER + 1
               END-PERFORM
               IF IS-NULL-ZEILE = 1
      *            Falls Nullzeile vorliegt wird -1 in COLMN geschrieben
      *            damit Matrix-Vektor-Multiplikation machbar wird
                   MOVE -1 TO COLMN(VAL-COUNTER)
                   COMPUTE VAL-COUNTER = VAL-COUNTER + 1
      *            Korrektur noetig da bei Nullzeile ein Eintrag gesetzt
      *            wird um spaeter Nullzeilen bei Multiplikation zu
      *            erkennen
                   COMPUTE KORREKTUR = KORREKTUR - 1
                   END-IF
      *        Kennzeichnung der naechsten Zeile durch Eintrag in RPTR
               MOVE VAL-COUNTER TO RPTR(I + 1)
            END-PERFORM
            .

       INITIALIZE-CRS.
           PERFORM VARYING I FROM 1 BY 1 UNTIL I > DIM-M
              MOVE 1 TO IS-NULL-ZEILE
              PERFORM VARYING J FROM I BY 1 UNTIL J > DIM-M
                 IF NOT ELEM(I, J) = 0
      *                 Zaehlt die Variablen != 0 fuer CRS-Val
                        COMPUTE NUM-NOT-ZERO = NUM-NOT-ZERO + 1
                        MOVE 0 TO IS-NULL-ZEILE
                     END-IF
              END-PERFORM
      *       Wenn Nullzeile vorliegt Counter erhoehen damit nachher
      *       Anzahl Cols entsprechend gesetzt werden kann
              IF IS-NULL-ZEILE = 1
                  COMPUTE COUNTER-NULL-ZEILE = COUNTER-NULL-ZEILE + 1
              END-IF
           END-PERFORM
           MOVE NUM-NOT-ZERO TO DIM-VAL
           COMPUTE DIM-COLS = NUM-NOT-ZERO + COUNTER-NULL-ZEILE
           COMPUTE DIM-ROWPTR = DIM-M + 1
           .
       END PROGRAM CompressedRowStorage.
