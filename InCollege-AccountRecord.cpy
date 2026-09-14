      *>*******************************************************
      *> InCollege-AccountRecord.cpy
      *> ACCOUNTS-FILE record layout. Include right after
      *> "FD ACCOUNTS-FILE." in the FILE SECTION.
      *>*******************************************************
       01  ACCOUNT-RECORD.
           05  ACCT-REC-USERNAME       PIC X(20).
           05  ACCT-REC-PASSWORD       PIC X(12).
