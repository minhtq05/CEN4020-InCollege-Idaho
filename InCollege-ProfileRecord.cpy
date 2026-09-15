      *>*******************************************************
      *> InCollege-ProfileRecord.cpy
      *> PROFILES-FILE record layout. Include right after
      *> "FD PROFILES-FILE." in the FILE SECTION.
      *>*******************************************************
       01  PROFILE-RECORD.
           05  PROF-USERNAME           PIC X(20).
           05  PROF-FIRST-NAME         PIC X(30).
           05  PROF-LAST-NAME          PIC X(30).
           05  PROF-UNIVERSITY         PIC X(50).
           05  PROF-MAJOR              PIC X(50).
           05  PROF-GRAD-YEAR          PIC 9(4).
           05  PROF-ABOUT-ME           PIC X(200).
           05  PROF-EXP-COUNT          PIC 9.
           05  PROF-EXP OCCURS 3 TIMES.
               10  PROF-EXP-TITLE      PIC X(50).
               10  PROF-EXP-COMPANY    PIC X(50).
               10  PROF-EXP-DATES      PIC X(30).
               10  PROF-EXP-DESC       PIC X(100).
           05  PROF-EDU-COUNT          PIC 9.
           05  PROF-EDU OCCURS 3 TIMES.
               10  PROF-EDU-DEGREE     PIC X(50).
               10  PROF-EDU-UNIVERSITY PIC X(50).
               10  PROF-EDU-YEARS      PIC X(20).
           05  PROF-HAS-DATA           PIC X.
