      *>*******************************************************
      *> InCollege - Epic #1: Log In, Part 1
      *> Authentication, account persistence, and initial
      *> post-login navigation for the InCollege alpha CLI.
      *>*******************************************************
       >>SOURCE FORMAT FREE
       IDENTIFICATION DIVISION.
       PROGRAM-ID. INCOLLEGE.

       ENVIRONMENT DIVISION.
       INPUT-OUTPUT SECTION.
       FILE-CONTROL.
           SELECT INPUT-FILE ASSIGN TO "InCollege-Input.txt"
               ORGANIZATION IS LINE SEQUENTIAL.

           SELECT OUTPUT-FILE ASSIGN TO "InCollege-Output.txt"
               ORGANIZATION IS LINE SEQUENTIAL.

           SELECT ACCOUNTS-FILE ASSIGN TO "InCollege-Accounts.txt"
               ORGANIZATION IS LINE SEQUENTIAL
               FILE STATUS IS WS-ACCT-FILE-STATUS.

       DATA DIVISION.
       FILE SECTION.
       FD  INPUT-FILE.
       01  INPUT-RECORD                PIC X(100).

       FD  OUTPUT-FILE.
       01  OUTPUT-RECORD               PIC X(100).

       FD  ACCOUNTS-FILE.
       01  ACCOUNT-RECORD.
           05  ACCT-REC-USERNAME       PIC X(20).
           05  ACCT-REC-PASSWORD       PIC X(12).

       WORKING-STORAGE SECTION.

       01  WS-ACCT-FILE-STATUS         PIC XX.

       01  WS-EOF-INPUT                PIC X VALUE 'N'.
           88  END-OF-INPUT                     VALUE 'Y'.

       01  WS-ACCOUNTS-EOF             PIC X VALUE 'N'.
           88  END-OF-ACCOUNTS                  VALUE 'Y'.

       01  WS-PROGRAM-DONE             PIC X VALUE 'N'.
           88  PROGRAM-DONE                     VALUE 'Y'.

       01  WS-INPUT-LINE               PIC X(100).

       01  WS-ACCOUNT-COUNT            PIC 9 VALUE 0.
       01  WS-ACCOUNTS-TABLE.
           05  WS-ACCOUNT OCCURS 5 TIMES.
               10  WS-ACCT-USERNAME    PIC X(20).
               10  WS-ACCT-PASSWORD    PIC X(12).

       01  WS-SKILLS-TABLE.
           05  WS-SKILL OCCURS 5 TIMES PIC X(30) VALUE SPACES.

       01  WS-IDX                      PIC 9(2).
       01  WS-FOUND-IDX                PIC 9(2) VALUE 0.

       01  WS-CURRENT-USERNAME         PIC X(20).
       01  WS-TOP-CHOICE               PIC X(20).
       01  WS-POST-CHOICE              PIC X(20).
       01  WS-SKILL-CHOICE             PIC X(20).

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

       01  WS-LOGIN-SUCCESS            PIC X VALUE 'N'.
           88  LOGIN-SUCCESSFUL                VALUE 'Y'.

       01  WS-LOGOUT-FLAG              PIC X VALUE 'N'.
           88  LOGOUT-REQUESTED                VALUE 'Y'.

       01  WS-SKILL-MENU-DONE          PIC X VALUE 'N'.
           88  SKILL-MENU-DONE                 VALUE 'Y'.

       01  WS-WRITE-MSG                PIC X(100).

       PROCEDURE DIVISION.

       0000-MAIN SECTION.
       0000-MAIN-START.
           OPEN INPUT INPUT-FILE.
           OPEN OUTPUT OUTPUT-FILE.
           PERFORM 1000-INITIALIZE.
           PERFORM 1100-LOAD-SKILLS.
           PERFORM UNTIL PROGRAM-DONE OR END-OF-INPUT
               PERFORM 2000-TOP-LEVEL-MENU
           END-PERFORM.
           CLOSE INPUT-FILE.
           CLOSE OUTPUT-FILE.
           STOP RUN.

       1000-INITIALIZE SECTION.
       1000-INITIALIZE-START.
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

       1100-LOAD-SKILLS SECTION.
       1100-LOAD-SKILLS-START.
           MOVE "Python Programming" TO WS-SKILL(1).
           MOVE "Public Speaking"    TO WS-SKILL(2).
           MOVE "Resume Writing"     TO WS-SKILL(3).
           MOVE "Data Analysis"      TO WS-SKILL(4).
           MOVE "Networking"         TO WS-SKILL(5).

       2000-TOP-LEVEL-MENU SECTION.
       2000-TOP-LEVEL-MENU-START.
           MOVE "Welcome to InCollege!" TO WS-WRITE-MSG
           PERFORM 9000-WRITE-LINE.
           MOVE "Log In" TO WS-WRITE-MSG
           PERFORM 9000-WRITE-LINE.
           MOVE "Create New Account" TO WS-WRITE-MSG
           PERFORM 9000-WRITE-LINE.
           MOVE "Enter your choice:" TO WS-WRITE-MSG
           PERFORM 9000-WRITE-LINE.
           PERFORM 9100-READ-LINE.
           IF NOT END-OF-INPUT
               MOVE WS-INPUT-LINE TO WS-TOP-CHOICE
               EVALUATE FUNCTION TRIM(WS-TOP-CHOICE)
                   WHEN "1"
                       PERFORM 5000-LOGIN
                   WHEN "2"
                       PERFORM 3000-CREATE-ACCOUNT
                   WHEN OTHER
                       MOVE "Invalid choice, please try again."
                           TO WS-WRITE-MSG
                       PERFORM 9000-WRITE-LINE
               END-EVALUATE
           END-IF.

       3000-CREATE-ACCOUNT SECTION.
       3000-CREATE-ACCOUNT-START.
           IF WS-ACCOUNT-COUNT >= 5
               MOVE "All permitted accounts have been created, please come back later"
                   TO WS-WRITE-MSG
               PERFORM 9000-WRITE-LINE
           ELSE
               MOVE "Please enter your username:" TO WS-WRITE-MSG
               PERFORM 9000-WRITE-LINE
               PERFORM 9100-READ-LINE
               IF NOT END-OF-INPUT
                   MOVE WS-INPUT-LINE TO WS-RAW-USERNAME
                   MOVE "Please enter your password:" TO WS-WRITE-MSG
                   PERFORM 9000-WRITE-LINE
                   PERFORM 9100-READ-LINE
               END-IF
               IF NOT END-OF-INPUT
                   MOVE WS-INPUT-LINE TO WS-RAW-PASSWORD
                   PERFORM 4000-VALIDATE-PASSWORD
                   PERFORM 3100-CHECK-USERNAME-UNIQUE
                   IF NOT PASSWORD-IS-VALID
                       PERFORM 9000-WRITE-LINE
                   ELSE
                       IF WS-FOUND-IDX > 0
                           MOVE "That username is already taken, please choose another."
                               TO WS-WRITE-MSG
                           PERFORM 9000-WRITE-LINE
                       ELSE
                           PERFORM 3200-SAVE-ACCOUNT
                       END-IF
                   END-IF
               END-IF
           END-IF.

       3100-CHECK-USERNAME-UNIQUE SECTION.
       3100-CHECK-USERNAME-UNIQUE-START.
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

       3200-SAVE-ACCOUNT SECTION.
       3200-SAVE-ACCOUNT-START.
           ADD 1 TO WS-ACCOUNT-COUNT.
           MOVE WS-USERNAME-TRIMMED TO WS-ACCT-USERNAME(WS-ACCOUNT-COUNT).
           MOVE WS-PASSWORD-TRIMMED TO WS-ACCT-PASSWORD(WS-ACCOUNT-COUNT).
           MOVE WS-USERNAME-TRIMMED TO WS-CURRENT-USERNAME.

           OPEN EXTEND ACCOUNTS-FILE.
           IF WS-ACCT-FILE-STATUS = "35"
               OPEN OUTPUT ACCOUNTS-FILE
           END-IF.
           MOVE WS-ACCT-USERNAME(WS-ACCOUNT-COUNT) TO ACCT-REC-USERNAME.
           MOVE WS-ACCT-PASSWORD(WS-ACCOUNT-COUNT) TO ACCT-REC-PASSWORD.
           WRITE ACCOUNT-RECORD.
           CLOSE ACCOUNTS-FILE.

           MOVE "Your account has been created successfully."
               TO WS-WRITE-MSG.
           PERFORM 9000-WRITE-LINE.

       4000-VALIDATE-PASSWORD SECTION.
       4000-VALIDATE-PASSWORD-START.
           MOVE 'N' TO WS-PASSWORD-VALID.
           MOVE 'N' TO WS-HAS-UPPER.
           MOVE 'N' TO WS-HAS-DIGIT.
           MOVE 'N' TO WS-HAS-SPECIAL.
           MOVE FUNCTION TRIM(WS-RAW-PASSWORD) TO WS-PASSWORD-TRIMMED.
           MOVE FUNCTION LENGTH(FUNCTION TRIM(WS-RAW-PASSWORD))
               TO WS-PASSWORD-LEN.

           IF WS-PASSWORD-LEN < 8 OR WS-PASSWORD-LEN > 12
               MOVE "Password must be between 8 and 12 characters long."
                   TO WS-WRITE-MSG
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
                       TO WS-WRITE-MSG
               END-IF
           END-IF.

       5000-LOGIN SECTION.
       5000-LOGIN-START.
           MOVE 'N' TO WS-LOGIN-SUCCESS.
           PERFORM UNTIL LOGIN-SUCCESSFUL OR END-OF-INPUT
               MOVE "Please enter your username:" TO WS-WRITE-MSG
               PERFORM 9000-WRITE-LINE
               PERFORM 9100-READ-LINE
               IF NOT END-OF-INPUT
                   MOVE WS-INPUT-LINE TO WS-RAW-USERNAME
                   MOVE "Please enter your password:" TO WS-WRITE-MSG
                   PERFORM 9000-WRITE-LINE
                   PERFORM 9100-READ-LINE
               END-IF
               IF NOT END-OF-INPUT
                   MOVE WS-INPUT-LINE TO WS-RAW-PASSWORD
                   PERFORM 5100-CHECK-CREDENTIALS
                   IF LOGIN-SUCCESSFUL
                       MOVE "You have successfully logged in." TO WS-WRITE-MSG
                       PERFORM 9000-WRITE-LINE
                       STRING "Welcome, " DELIMITED BY SIZE
                              FUNCTION TRIM(WS-CURRENT-USERNAME) DELIMITED BY SIZE
                              "!" DELIMITED BY SIZE
                              INTO WS-WRITE-MSG
                       PERFORM 9000-WRITE-LINE
                       PERFORM 6000-POST-LOGIN-MENU
                   ELSE
                       MOVE "Incorrect username/password, please try again."
                           TO WS-WRITE-MSG
                       PERFORM 9000-WRITE-LINE
                   END-IF
               END-IF
           END-PERFORM.

       5100-CHECK-CREDENTIALS SECTION.
       5100-CHECK-CREDENTIALS-START.
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

       6000-POST-LOGIN-MENU SECTION.
       6000-POST-LOGIN-MENU-START.
           MOVE 'N' TO WS-LOGOUT-FLAG.
           PERFORM UNTIL LOGOUT-REQUESTED OR END-OF-INPUT
               MOVE "1. Search for a job" TO WS-WRITE-MSG
               PERFORM 9000-WRITE-LINE
               MOVE "2. Find someone you know" TO WS-WRITE-MSG
               PERFORM 9000-WRITE-LINE
               MOVE "3. Learn a new skill" TO WS-WRITE-MSG
               PERFORM 9000-WRITE-LINE
               MOVE "4. Logout" TO WS-WRITE-MSG
               PERFORM 9000-WRITE-LINE
               MOVE "Enter your choice:" TO WS-WRITE-MSG
               PERFORM 9000-WRITE-LINE
               PERFORM 9100-READ-LINE
               IF NOT END-OF-INPUT
                   MOVE WS-INPUT-LINE TO WS-POST-CHOICE
                   EVALUATE FUNCTION TRIM(WS-POST-CHOICE)
                       WHEN "1"
                           MOVE "Job search/internship is under construction."
                               TO WS-WRITE-MSG
                           PERFORM 9000-WRITE-LINE
                       WHEN "2"
                           MOVE "Find someone you know is under construction."
                               TO WS-WRITE-MSG
                           PERFORM 9000-WRITE-LINE
                       WHEN "3"
                           PERFORM 7000-LEARN-SKILL-MENU
                       WHEN "4"
                           MOVE 'Y' TO WS-LOGOUT-FLAG
                           MOVE 'Y' TO WS-PROGRAM-DONE
                       WHEN OTHER
                           MOVE "Invalid choice, please try again."
                               TO WS-WRITE-MSG
                           PERFORM 9000-WRITE-LINE
                   END-EVALUATE
               END-IF
           END-PERFORM.

       7000-LEARN-SKILL-MENU SECTION.
       7000-LEARN-SKILL-MENU-START.
           MOVE 'N' TO WS-SKILL-MENU-DONE.
           PERFORM UNTIL SKILL-MENU-DONE OR END-OF-INPUT OR LOGOUT-REQUESTED
               MOVE "Learn a New Skill:" TO WS-WRITE-MSG
               PERFORM 9000-WRITE-LINE
               PERFORM VARYING WS-IDX FROM 1 BY 1 UNTIL WS-IDX > 5
                   MOVE WS-SKILL(WS-IDX) TO WS-WRITE-MSG
                   PERFORM 9000-WRITE-LINE
               END-PERFORM
               MOVE "Go Back" TO WS-WRITE-MSG
               PERFORM 9000-WRITE-LINE
               MOVE "Enter your choice:" TO WS-WRITE-MSG
               PERFORM 9000-WRITE-LINE
               PERFORM 9100-READ-LINE
               IF NOT END-OF-INPUT
                   MOVE WS-INPUT-LINE TO WS-SKILL-CHOICE
                   EVALUATE FUNCTION TRIM(WS-SKILL-CHOICE)
                       WHEN "1" THRU "5"
                           MOVE "This skill is under construction."
                               TO WS-WRITE-MSG
                           PERFORM 9000-WRITE-LINE
                       WHEN "6"
                           MOVE 'Y' TO WS-SKILL-MENU-DONE
                       WHEN OTHER
                           MOVE "Invalid choice, please try again."
                               TO WS-WRITE-MSG
                           PERFORM 9000-WRITE-LINE
                   END-EVALUATE
               END-IF
           END-PERFORM.

       9000-WRITE-LINE SECTION.
       9000-WRITE-LINE-START.
           DISPLAY FUNCTION TRIM(WS-WRITE-MSG).
           MOVE FUNCTION TRIM(WS-WRITE-MSG) TO OUTPUT-RECORD.
           WRITE OUTPUT-RECORD.

       9100-READ-LINE SECTION.
       9100-READ-LINE-START.
           READ INPUT-FILE INTO WS-INPUT-LINE
               AT END
                   MOVE 'Y' TO WS-EOF-INPUT
                   MOVE SPACES TO WS-INPUT-LINE
               NOT AT END
                   MOVE FUNCTION TRIM(WS-INPUT-LINE) TO WS-WRITE-MSG
                   PERFORM 9000-WRITE-LINE
           END-READ.
