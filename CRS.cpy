      *    Dient der Speicherung der Werte != 0
      *    Maximal 300.000 Elemente moeglich, da Matrix-Dim auf
      *    1000 x 1000 beschraenkt, daher 0,3 * 10^6 = 300.000
           01 CRS-VALS.
               05 DIM-VAL PIC 9(03).
               05 VAL PIC S9(06)V9(04) COMP-3
                       OCCURS 1 TO 300000 DEPENDING ON DIM-VAL.
      *    Dient der Speicherung der Positionen in der jeweiligen Spalte
      *    Maximal 300.000 Elemente, siehe Crs-Vals.
           01 CRS-COLS.
               05 DIM-COLS PIC 9(03).
               05 COLMN PIC S9(06) COMP-3
                       OCCURS 1 TO 300000 DEPENDING ON DIM-COLS.
      *    Dient der Erkennung der jeweiligen naechsten Zeile, exakte
      *    groesse von Dim-Matrix + 1, also maximal 1001
           01 CRS-ROW-PTR.
               05 DIM-ROWPTR PIC 9(03).
               05 RPTR PIC 9(03) COMP-3
                       OCCURS 1 TO 1001 DEPENDING ON DIM-ROWPTR.
