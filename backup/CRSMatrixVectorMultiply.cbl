      ******************************************************************
      * Author:
      * Date:
      * Purpose:
      * Tectonics: cobc
      ******************************************************************
       IDENTIFICATION DIVISION.
       PROGRAM-ID. CRSMatrixVectorMultiply.
       DATA DIVISION.
       FILE SECTION.
       WORKING-STORAGE SECTION.
      *    Differenz um herauszufinden wie viele Elemente in Zeile sind
           01 DIFF Pic 9(04) COMP-3.
           01 ZAEHLER.
              05 I Pic 9(04) COMP-3.
              05 J Pic 9(04) COMP-3.
              05 K Pic 9(04) COMP-3.
      *       Dient bloß dazu einen DUMMEN FEHLER zu beheben...
              05 KAPSEL PIC 9(04) COMP-3.
           01 KORREKTUR.
      *       Korrektur fuer Nullzeilen ab aii
              05 NULL-SUB Pic S9(04) COMP-3.
      *       Korrektur fuer Nullzeilen fuer Multiplikation bis aii
      *       Wird in jeder Iteration wieder auf Null gesetzt, da
      *       fuer eine jeweilige Zeile die Nullzeilen davor ausfindig
      *       gemacht werden muessen
              05 NULL-SUB-INLINE Pic S9(04) COMP-3.
       LINKAGE SECTION.
           COPY "CRS.cpy".
           COPY "VectorDim.cpy".
           COPY "OutputVector.cpy".
       PROCEDURE DIVISION USING CRS-VALS CRS-COLS CRS-ROW-PTR
                                INPUT-VEKTOR OUT-VEKTOR.
       MAIN-PROCEDURE.
            MOVE 1 TO I
            PERFORM UNTIL I > DIM-I
               MOVE 0.0 TO XK(I)
               COMPUTE I = I + 1
            END-PERFORM

            MOVE 0 TO NULL-SUB
            MOVE 0 TO NULL-SUB-INLINE
            MOVE 0 TO DIFF

            PERFORM VARYING I FROM 1 BY 1 UNTIL I > DIM-I
      *     wenn an der Stelle von RPTR(I) in COLMN -1 steht, dann sind
      *     alle Eintraege von aii bis ain = 0, daher Multiplikation der
      *     Elemente vor aii
               IF COLMN(RPTR(I)) = -1
                  COMPUTE NULL-SUB = NULL-SUB - 1
                  MOVE 0 TO NULL-SUB-INLINE
                  PERFORM VARYING J FROM 1 BY 1 UNTIL J > I
      *           Nullzeilen werden ignoriert, da kein Eintrag
      *           gefunden werden kann
                     IF COLMN(RPTR(J)) = -1
                        COMPUTE NULL-SUB-INLINE
      -                          = NULL-SUB-INLINE - 1
      *              Bei nicht NUllzeilen wird Eintrag gesucht in der
      *              Spalte I einen Eintrag besitzt
                     ELSE
                        COMPUTE DIFF = RPTR(J + 1) - RPTR(J)
                        PERFORM VARYING K FROM 0 BY 1 UNTIL K >= DIFF
                           IF COLMN(RPTR(J) + K) = I
                              COMPUTE XK(I) = XK(I) + XI(J) *
      -                               VAL(RPTR(J)+ K + NULL-SUB-INLINE)
                           END-IF
                        END-PERFORM
                     END-IF
                  END-PERFORM
                ELSE
      *            Matrix-Vektormult. der Elemente ai1 bis aii-1
                   MOVE 0 TO NULL-SUB-INLINE
                   PERFORM VARYING J FROM 1 BY 1 UNTIL J >= I
      *            Nullzeilen werden ignoriert, da dort kein Eintrag
      *            gefunden werden kann
                      IF COLMN(RPTR(J)) = -1
                         COMPUTE NULL-SUB-INLINE = NULL-SUB-INLINE - 1
                      ELSE
                         COMPUTE DIFF = RPTR(J + 1) - RPTR(J)
                         PERFORM VARYING K FROM 0 BY 1 UNTIL K >= DIFF
                            IF COLMN(RPTR(J)+K) = I
                               COMPUTE XK(I) = XK(I) +  XI(J) *
                              VAL(RPTR(J) + K + NULL-SUB-INLINE)
                            END-IF
                         END-PERFORM
                      END-IF
                   END-PERFORM
                   COMPUTE DIFF = RPTR(I + 1) - RPTR(I)
      *            Produktbildung ab aii bis ain
                   PERFORM VARYING J FROM 0 BY 1 UNTIL J >= DIFF
                      COMPUTE KAPSEL = RPTR(I) + J
      *    Die nachfolgende Zeile produziert komischerweise einen Fehler
      *    Der Fehler lautet: libcob: 'XI' not numeric. Es ergibt KEINEN
      *    Sinn, absolut GAR KEINEN!!! Hier kann nach Konstruktion des
      *    Algorithmus NIEMALS!!! ein negativer Index entstehen! Das
      *    Problem wird bloß von Colmn(RPTR(I) + J) ausgelöst, schiebt
      *    man den Wert vorher in eine andere Variable, funktioniert
      *    alles einwandfrei... That's Cobol?
      *FEHLERZEILE!   COMPUTE XK(I) = XK(I) + XI(Colmn(RPTR(I) + J)) *
                      COMPUTE XK(I) = XK(I) + XI(COLMN(KAPSEL)) *
                              VAL(RPTR(I) + J + NULL-SUB)
                   END-PERFORM
                END-IF
            END-PERFORM
            EXIT PROGRAM.
