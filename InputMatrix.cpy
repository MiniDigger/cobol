      *    Matrix der Eingabe: Max Size der Elemente: 2147483647
      *    Daher Beschraenkung Matrix noetig, setze Max auf 1000x1000
           01 MATRIX.
               05 DIM-M PIC 9(04) COMP-3.
               05 ZEILE OCCURS 1 TO 1000.
                   10 ELEM PIC S9(09)V9(16) OCCURS 1 TO 1000.
