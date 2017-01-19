           01 INPUT-VEKTOR.
      *        Input-Vector
               05 DIM-I PIC 9(04) COMP-3.
               05 VECTOR OCCURS 1 TO 1000 DEPENDING ON DIM-I.
                   10 XI PIC S9(06)V9(16) COMP-3.
                   10 XI-ORIG PIC S9(06)V9(16) COMP-3.
