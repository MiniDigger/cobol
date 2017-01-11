            01 OUT-VEKTOR.
               05 DIM-O PIC 9(04) COMP-3.
      *        Output-Vector
               05 XK PIC S9(06)V9(16)
                       OCCURS 1 TO 1000 DEPENDING ON DIM-O COMP-3.
