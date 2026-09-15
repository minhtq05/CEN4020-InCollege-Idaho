      *>*******************************************************
      *> InCollege-Common.cpy
      *> EXTERNAL session-lifetime state shared by every program
      *> in the InCollege run unit. No VALUE clauses (not portable
      *> on EXTERNAL items) -- InCollege.cob (the main program)
      *> explicitly initializes these once at startup.
      *>*******************************************************
       01  WS-EOF-INPUT                PIC X EXTERNAL.
           88  END-OF-INPUT                     VALUE 'Y'.

       01  WS-CURRENT-USERNAME         PIC X(20) EXTERNAL.

       01  WS-PROGRAM-DONE             PIC X EXTERNAL.
           88  PROGRAM-DONE                     VALUE 'Y'.
