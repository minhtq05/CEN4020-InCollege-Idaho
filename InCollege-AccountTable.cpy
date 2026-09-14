      *>*******************************************************
      *> InCollege-AccountTable.cpy
      *> EXTERNAL in-memory accounts table, shared by every
      *> program in InCollege-Account.cob (load, create, login).
      *>*******************************************************
       01  WS-ACCOUNT-COUNT            PIC 9 EXTERNAL.

       01  WS-ACCOUNTS-TABLE           EXTERNAL.
           05  WS-ACCOUNT OCCURS 5 TIMES.
               10  WS-ACCT-USERNAME    PIC X(20).
               10  WS-ACCT-PASSWORD    PIC X(12).
