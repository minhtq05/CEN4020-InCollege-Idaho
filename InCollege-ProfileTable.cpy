      *>*******************************************************
      *> InCollege-ProfileTable.cpy
      *> EXTERNAL in-memory profile table, shared by every
      *> profile-related program (InCollege-Profile.cob,
      *> InCollege-ProfileEntries.cob, InCollege-ProfileView.cob).
      *> Rows are keyed by WS-PROF-USERNAME, resolved via
      *> InCollege-ProfileFindRow.cpy -- not tied to any account
      *> table ordering.
      *>*******************************************************
       01  WS-PROFILE-COUNT            PIC 9 EXTERNAL.

       01  WS-PROFILE-TABLE            EXTERNAL.
           05  WS-PROFILE OCCURS 5 TIMES.
               10  WS-PROF-USERNAME        PIC X(20).
               10  WS-PROF-FIRST-NAME      PIC X(30).
               10  WS-PROF-LAST-NAME       PIC X(30).
               10  WS-PROF-UNIVERSITY      PIC X(50).
               10  WS-PROF-MAJOR           PIC X(50).
               10  WS-PROF-GRAD-YEAR       PIC 9(4).
               10  WS-PROF-ABOUT-ME        PIC X(200).
               10  WS-PROF-EXP-COUNT       PIC 9.
               10  WS-PROF-EXP OCCURS 3 TIMES.
                   15  WS-PROF-EXP-TITLE     PIC X(50).
                   15  WS-PROF-EXP-COMPANY   PIC X(50).
                   15  WS-PROF-EXP-DATES     PIC X(30).
                   15  WS-PROF-EXP-DESC      PIC X(100).
               10  WS-PROF-EDU-COUNT       PIC 9.
               10  WS-PROF-EDU OCCURS 3 TIMES.
                   15  WS-PROF-EDU-DEGREE      PIC X(50).
                   15  WS-PROF-EDU-UNIVERSITY  PIC X(50).
                   15  WS-PROF-EDU-YEARS       PIC X(20).
               10  WS-PROF-HAS-DATA        PIC X.
