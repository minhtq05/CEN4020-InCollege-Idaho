      *>*******************************************************
      *> InCollege-Account.cob
      *> Account management: load accounts at startup, create a
      *> new account (with password validation + uniqueness
      *> check), and log in (credential check loop).
      *>*******************************************************
       >>SOURCE FORMAT FREE
       IDENTIFICATION DIVISION.
       PROGRAM-ID. ACCOUNT-LOAD.

       ENVIRONMENT DIVISION.
       INPUT-OUTPUT SECTION.
       FILE-CONTROL.
           SELECT ACCOUNTS-FILE ASSIGN TO "InCollege-Accounts.txt"
               ORGANIZATION IS LINE SEQUENTIAL
               FILE STATUS IS WS-ACCT-FILE-STATUS.

       DATA DIVISION.
       FILE SECTION.
       FD  ACCOUNTS-FILE.
       COPY "InCollege-AccountRecord.cpy".

       WORKING-STORAGE SECTION.
       COPY "InCollege-AccountTable.cpy".

       01  WS-ACCT-FILE-STATUS         PIC XX.

       01  WS-ACCOUNTS-EOF             PIC X VALUE 'N'.
           88  END-OF-ACCOUNTS                  VALUE 'Y'.

       PROCEDURE DIVISION.
       ACCOUNT-LOAD-START.
           MOVE 0 TO WS-ACCOUNT-COUNT.
           MOVE 'N' TO WS-ACCOUNTS-EOF.
           OPEN INPUT ACCOUNTS-FILE.
           IF WS-ACCT-FILE-STATUS NOT = "35"
               PERFORM UNTIL END-OF-ACCOUNTS
                   READ ACCOUNTS-FILE
                       AT END
                           MOVE 'Y' TO WS-ACCOUNTS-EOF
                       NOT AT END
                           ADD 1 TO WS-ACCOUNT-COUNT
                           MOVE ACCT-REC-USERNAME
                               TO WS-ACCT-USERNAME(WS-ACCOUNT-COUNT)
                           MOVE ACCT-REC-PASSWORD
                               TO WS-ACCT-PASSWORD(WS-ACCOUNT-COUNT)
                   END-READ
               END-PERFORM
               CLOSE ACCOUNTS-FILE
           END-IF.
           GOBACK.
       END PROGRAM ACCOUNT-LOAD.

       IDENTIFICATION DIVISION.
       PROGRAM-ID. ACCOUNT-CREATE.

       ENVIRONMENT DIVISION.
       INPUT-OUTPUT SECTION.
       FILE-CONTROL.
           SELECT ACCOUNTS-FILE ASSIGN TO "InCollege-Accounts.txt"
               ORGANIZATION IS LINE SEQUENTIAL
               FILE STATUS IS WS-ACCT-FILE-STATUS.

       DATA DIVISION.
       FILE SECTION.
       FD  ACCOUNTS-FILE.
       COPY "InCollege-AccountRecord.cpy".

       WORKING-STORAGE SECTION.
       COPY "InCollege-Common.cpy".
       COPY "InCollege-AccountTable.cpy".

       01  WS-ACCT-FILE-STATUS         PIC XX.

       01  WS-LINE-TEXT                PIC X(100).
       01  WS-MESSAGE                  PIC X(100).

       01  WS-RAW-USERNAME             PIC X(100).
       01  WS-RAW-PASSWORD             PIC X(100).
       01  WS-USERNAME-TRIMMED         PIC X(100).
       01  WS-PASSWORD-TRIMMED         PIC X(100).
       01  WS-PASSWORD-LEN             PIC 9(3).

       01  WS-PASSWORD-VALID           PIC X VALUE 'N'.
           88  PASSWORD-IS-VALID               VALUE 'Y'.
       01  WS-HAS-UPPER                PIC X VALUE 'N'.
           88  HAS-UPPER                       VALUE 'Y'.
       01  WS-HAS-DIGIT                PIC X VALUE 'N'.
           88  HAS-DIGIT                       VALUE 'Y'.
       01  WS-HAS-SPECIAL              PIC X VALUE 'N'.
           88  HAS-SPECIAL                     VALUE 'Y'.
       01  WS-CUR-CHAR                 PIC X.

       01  WS-FOUND-IDX                PIC 9(2) VALUE 0.
       01  WS-IDX                      PIC 9(2).

       PROCEDURE DIVISION.
       ACCOUNT-CREATE-START.
           IF WS-ACCOUNT-COUNT >= 5
               MOVE "All permitted accounts have been created, please come back later"
                   TO WS-MESSAGE
               CALL "WRITE-LINE" USING WS-MESSAGE
           ELSE
               MOVE "Please enter your username:" TO WS-MESSAGE
               CALL "WRITE-LINE" USING WS-MESSAGE
               CALL "READ-LINE" USING WS-LINE-TEXT
               IF NOT END-OF-INPUT
                   MOVE WS-LINE-TEXT TO WS-RAW-USERNAME
                   MOVE "Please enter your password:" TO WS-MESSAGE
                   CALL "WRITE-LINE" USING WS-MESSAGE
                   CALL "READ-LINE" USING WS-LINE-TEXT
               END-IF
               IF NOT END-OF-INPUT
                   MOVE WS-LINE-TEXT TO WS-RAW-PASSWORD
                   PERFORM VALIDATE-PASSWORD
                   PERFORM CHECK-USERNAME-UNIQUE
                   IF NOT PASSWORD-IS-VALID
                       CALL "WRITE-LINE" USING WS-MESSAGE
                   ELSE
                       IF WS-FOUND-IDX > 0
                           MOVE "That username is already taken, please choose another."
                               TO WS-MESSAGE
                           CALL "WRITE-LINE" USING WS-MESSAGE
                       ELSE
                           PERFORM SAVE-ACCOUNT
                       END-IF
                   END-IF
               END-IF
           END-IF.
           GOBACK.

       CHECK-USERNAME-UNIQUE SECTION.
       CHECK-USERNAME-UNIQUE-START.
           MOVE 0 TO WS-FOUND-IDX.
           MOVE FUNCTION TRIM(WS-RAW-USERNAME) TO WS-USERNAME-TRIMMED.
           IF WS-ACCOUNT-COUNT > 0
               PERFORM VARYING WS-IDX FROM 1 BY 1
                       UNTIL WS-IDX > WS-ACCOUNT-COUNT
                   IF FUNCTION TRIM(WS-ACCT-USERNAME(WS-IDX)) =
                      FUNCTION TRIM(WS-USERNAME-TRIMMED)
                       MOVE WS-IDX TO WS-FOUND-IDX
                   END-IF
               END-PERFORM
           END-IF.

       SAVE-ACCOUNT SECTION.
       SAVE-ACCOUNT-START.
           ADD 1 TO WS-ACCOUNT-COUNT.
           MOVE WS-USERNAME-TRIMMED TO WS-ACCT-USERNAME(WS-ACCOUNT-COUNT).
           MOVE WS-PASSWORD-TRIMMED TO WS-ACCT-PASSWORD(WS-ACCOUNT-COUNT).

           OPEN EXTEND ACCOUNTS-FILE.
           IF WS-ACCT-FILE-STATUS = "35"
               OPEN OUTPUT ACCOUNTS-FILE
           END-IF.
           MOVE WS-ACCT-USERNAME(WS-ACCOUNT-COUNT) TO ACCT-REC-USERNAME.
           MOVE WS-ACCT-PASSWORD(WS-ACCOUNT-COUNT) TO ACCT-REC-PASSWORD.
           WRITE ACCOUNT-RECORD.
           CLOSE ACCOUNTS-FILE.

           MOVE "Your account has been created successfully."
               TO WS-MESSAGE.
           CALL "WRITE-LINE" USING WS-MESSAGE.

       VALIDATE-PASSWORD SECTION.
       VALIDATE-PASSWORD-START.
           MOVE 'N' TO WS-PASSWORD-VALID.
           MOVE 'N' TO WS-HAS-UPPER.
           MOVE 'N' TO WS-HAS-DIGIT.
           MOVE 'N' TO WS-HAS-SPECIAL.
           MOVE FUNCTION TRIM(WS-RAW-PASSWORD) TO WS-PASSWORD-TRIMMED.
           MOVE FUNCTION LENGTH(FUNCTION TRIM(WS-RAW-PASSWORD))
               TO WS-PASSWORD-LEN.

           IF WS-PASSWORD-LEN < 8 OR WS-PASSWORD-LEN > 12
               MOVE "Password must be between 8 and 12 characters long."
                   TO WS-MESSAGE
           ELSE
               PERFORM VARYING WS-IDX FROM 1 BY 1
                       UNTIL WS-IDX > WS-PASSWORD-LEN
                   MOVE WS-PASSWORD-TRIMMED(WS-IDX:1) TO WS-CUR-CHAR
                   EVALUATE TRUE
                       WHEN WS-CUR-CHAR >= 'A' AND WS-CUR-CHAR <= 'Z'
                           MOVE 'Y' TO WS-HAS-UPPER
                       WHEN WS-CUR-CHAR >= '0' AND WS-CUR-CHAR <= '9'
                           MOVE 'Y' TO WS-HAS-DIGIT
                       WHEN WS-CUR-CHAR >= 'a' AND WS-CUR-CHAR <= 'z'
                           CONTINUE
                       WHEN OTHER
                           MOVE 'Y' TO WS-HAS-SPECIAL
                   END-EVALUATE
               END-PERFORM
               IF HAS-UPPER AND HAS-DIGIT AND HAS-SPECIAL
                   MOVE 'Y' TO WS-PASSWORD-VALID
               ELSE
                   MOVE "Password does not meet requirements (needs an uppercase letter, a digit, and a special character)."
                       TO WS-MESSAGE
               END-IF
           END-IF.
       END PROGRAM ACCOUNT-CREATE.

       IDENTIFICATION DIVISION.
       PROGRAM-ID. ACCOUNT-LOGIN.

       DATA DIVISION.
       WORKING-STORAGE SECTION.
       COPY "InCollege-Common.cpy".
       COPY "InCollege-AccountTable.cpy".

       01  WS-LINE-TEXT                PIC X(100).
       01  WS-MESSAGE                  PIC X(100).

       01  WS-RAW-USERNAME             PIC X(100).
       01  WS-RAW-PASSWORD             PIC X(100).
       01  WS-USERNAME-TRIMMED         PIC X(100).
       01  WS-PASSWORD-TRIMMED         PIC X(100).
       01  WS-IDX                      PIC 9(2).

       01  WS-LOGIN-SUCCESS            PIC X VALUE 'N'.
           88  LOGIN-SUCCESSFUL                VALUE 'Y'.

       LINKAGE SECTION.
       01  LS-LOGIN-OK                 PIC X.

       PROCEDURE DIVISION USING LS-LOGIN-OK.
       ACCOUNT-LOGIN-START.
           MOVE 'N' TO WS-LOGIN-SUCCESS.
           MOVE 'N' TO LS-LOGIN-OK.
           PERFORM UNTIL LOGIN-SUCCESSFUL OR END-OF-INPUT
               MOVE "Please enter your username:" TO WS-MESSAGE
               CALL "WRITE-LINE" USING WS-MESSAGE
               CALL "READ-LINE" USING WS-LINE-TEXT
               IF NOT END-OF-INPUT
                   MOVE WS-LINE-TEXT TO WS-RAW-USERNAME
                   MOVE "Please enter your password:" TO WS-MESSAGE
                   CALL "WRITE-LINE" USING WS-MESSAGE
                   CALL "READ-LINE" USING WS-LINE-TEXT
               END-IF
               IF NOT END-OF-INPUT
                   MOVE WS-LINE-TEXT TO WS-RAW-PASSWORD
                   PERFORM CHECK-CREDENTIALS
                   IF LOGIN-SUCCESSFUL
                       MOVE "You have successfully logged in." TO WS-MESSAGE
                       CALL "WRITE-LINE" USING WS-MESSAGE
                       MOVE SPACES TO WS-MESSAGE
                       STRING "Welcome, " DELIMITED BY SIZE
                              FUNCTION TRIM(WS-CURRENT-USERNAME) DELIMITED BY SIZE
                              "!" DELIMITED BY SIZE
                              INTO WS-MESSAGE
                       CALL "WRITE-LINE" USING WS-MESSAGE
                       MOVE 'Y' TO LS-LOGIN-OK
                   ELSE
                       MOVE "Incorrect username/password, please try again."
                           TO WS-MESSAGE
                       CALL "WRITE-LINE" USING WS-MESSAGE
                   END-IF
               END-IF
           END-PERFORM.
           GOBACK.

       CHECK-CREDENTIALS SECTION.
       CHECK-CREDENTIALS-START.
           MOVE 'N' TO WS-LOGIN-SUCCESS.
           MOVE FUNCTION TRIM(WS-RAW-USERNAME) TO WS-USERNAME-TRIMMED.
           MOVE FUNCTION TRIM(WS-RAW-PASSWORD) TO WS-PASSWORD-TRIMMED.
           IF WS-ACCOUNT-COUNT > 0
               PERFORM VARYING WS-IDX FROM 1 BY 1
                       UNTIL WS-IDX > WS-ACCOUNT-COUNT
                   IF FUNCTION TRIM(WS-ACCT-USERNAME(WS-IDX)) =
                      FUNCTION TRIM(WS-USERNAME-TRIMMED)
                      AND FUNCTION TRIM(WS-ACCT-PASSWORD(WS-IDX)) =
                          FUNCTION TRIM(WS-PASSWORD-TRIMMED)
                       MOVE 'Y' TO WS-LOGIN-SUCCESS
                       MOVE WS-ACCT-USERNAME(WS-IDX) TO WS-CURRENT-USERNAME
                   END-IF
               END-PERFORM
           END-IF.
       END PROGRAM ACCOUNT-LOGIN.
