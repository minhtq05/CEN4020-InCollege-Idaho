      *>*******************************************************
      *> InCollege-Profile.cob
      *> Profile persistence (load whole table / save whole table)
      *> and the create/edit flow for required fields + graduation
      *> year + About Me. Delegates experience/education entry
      *> collection to InCollege-ProfileEntries.cob.
      *>*******************************************************
       >>SOURCE FORMAT FREE
       IDENTIFICATION DIVISION.
       PROGRAM-ID. PROFILE-LOAD.

       ENVIRONMENT DIVISION.
       INPUT-OUTPUT SECTION.
       FILE-CONTROL.
           SELECT PROFILES-FILE ASSIGN TO "InCollege-Profiles.txt"
               ORGANIZATION IS LINE SEQUENTIAL
               FILE STATUS IS WS-PROF-FILE-STATUS.

       DATA DIVISION.
       FILE SECTION.
       FD  PROFILES-FILE.
       COPY "InCollege-ProfileRecord.cpy".

       WORKING-STORAGE SECTION.
       COPY "InCollege-ProfileTable.cpy".

       01  WS-PROF-FILE-STATUS         PIC XX.

       01  WS-PROFILES-EOF             PIC X VALUE 'N'.
           88  END-OF-PROFILES                  VALUE 'Y'.

       PROCEDURE DIVISION.
       PROFILE-LOAD-START.
           INITIALIZE WS-PROFILE-TABLE.
           MOVE 0 TO WS-PROFILE-COUNT.
           MOVE 'N' TO WS-PROFILES-EOF.
           OPEN INPUT PROFILES-FILE.
           IF WS-PROF-FILE-STATUS NOT = "35"
               PERFORM UNTIL END-OF-PROFILES
                   READ PROFILES-FILE
                       AT END
                           MOVE 'Y' TO WS-PROFILES-EOF
                       NOT AT END
                           IF WS-PROFILE-COUNT < 5
                               ADD 1 TO WS-PROFILE-COUNT
                               MOVE PROFILE-RECORD
                                   TO WS-PROFILE(WS-PROFILE-COUNT)
                           END-IF
                   END-READ
               END-PERFORM
               CLOSE PROFILES-FILE
           END-IF.
           GOBACK.
       END PROGRAM PROFILE-LOAD.

       IDENTIFICATION DIVISION.
       PROGRAM-ID. PROFILE-SAVE.

       ENVIRONMENT DIVISION.
       INPUT-OUTPUT SECTION.
       FILE-CONTROL.
           SELECT PROFILES-FILE ASSIGN TO "InCollege-Profiles.txt"
               ORGANIZATION IS LINE SEQUENTIAL.

       DATA DIVISION.
       FILE SECTION.
       FD  PROFILES-FILE.
       COPY "InCollege-ProfileRecord.cpy".

       WORKING-STORAGE SECTION.
       COPY "InCollege-ProfileTable.cpy".

       01  WS-IDX                      PIC 9(2).

       PROCEDURE DIVISION.
       PROFILE-SAVE-START.
           OPEN OUTPUT PROFILES-FILE.
           PERFORM VARYING WS-IDX FROM 1 BY 1 UNTIL WS-IDX > WS-PROFILE-COUNT
               MOVE WS-PROFILE(WS-IDX) TO PROFILE-RECORD
               WRITE PROFILE-RECORD
           END-PERFORM.
           CLOSE PROFILES-FILE.
           GOBACK.
       END PROGRAM PROFILE-SAVE.

       IDENTIFICATION DIVISION.
       PROGRAM-ID. PROFILE-EDIT.

       DATA DIVISION.
       WORKING-STORAGE SECTION.
       COPY "InCollege-Common.cpy".
       COPY "InCollege-ProfileTable.cpy".

       01  WS-LINE-TEXT                PIC X(100).
       01  WS-MESSAGE                  PIC X(100).

       01  WS-PROFILE-IDX              PIC 9(2).
       01  WS-FINDROW-APPEND           PIC X.
       01  WS-IDX                      PIC 9(2).

       01  WS-PROFILE-IS-NEW           PIC X VALUE 'N'.
           88  PROFILE-IS-NEW                    VALUE 'Y'.
       01  WS-PROFILE-BACKUP           PIC X(1437).

       01  WS-PROFILE-ABORT            PIC X VALUE 'N'.
           88  PROFILE-ABORTED                  VALUE 'Y'.

       01  WS-YEAR-VALID               PIC X VALUE 'N'.
           88  YEAR-IS-VALID                     VALUE 'Y'.
       01  WS-YEAR-LEN                 PIC 9(3).
       01  WS-YEAR-CHECK-IDX           PIC 9(3).
       01  WS-YEAR-RAW                 PIC X(4).
       01  WS-YEAR-NUM                 PIC 9(4).
       01  WS-CUR-CHAR                 PIC X.

       PROCEDURE DIVISION.
       PROFILE-EDIT-START.
           MOVE 'N' TO WS-PROFILE-ABORT.
           MOVE 'N' TO WS-PROFILE-IS-NEW.
           MOVE 'N' TO WS-FINDROW-APPEND.
           PERFORM PROFILE-FIND-ROW.
           IF WS-PROFILE-IDX = 0
               MOVE 'Y' TO WS-FINDROW-APPEND
               PERFORM PROFILE-FIND-ROW
               IF WS-PROFILE-IDX = 0
                   MOVE "Unable to create profile: profile capacity has been reached."
                       TO WS-MESSAGE
                   CALL "WRITE-LINE" USING WS-MESSAGE
                   GOBACK
               END-IF
               MOVE 'Y' TO WS-PROFILE-IS-NEW
               INITIALIZE WS-PROFILE(WS-PROFILE-IDX)
               MOVE FUNCTION TRIM(WS-CURRENT-USERNAME)
                   TO WS-PROF-USERNAME(WS-PROFILE-IDX)
           ELSE
               MOVE WS-PROFILE(WS-PROFILE-IDX) TO WS-PROFILE-BACKUP
           END-IF.
           MOVE "--- Create/Edit Profile ---" TO WS-MESSAGE.
           CALL "WRITE-LINE" USING WS-MESSAGE.

           MOVE "Enter First Name:" TO WS-MESSAGE.
           CALL "WRITE-LINE" USING WS-MESSAGE.
           CALL "READ-LINE" USING WS-LINE-TEXT.
           IF END-OF-INPUT
               MOVE 'Y' TO WS-PROFILE-ABORT
           ELSE
               IF FUNCTION TRIM(WS-LINE-TEXT) = SPACES
                   MOVE "First Name is required. Profile not saved."
                       TO WS-MESSAGE
                   CALL "WRITE-LINE" USING WS-MESSAGE
                   MOVE 'Y' TO WS-PROFILE-ABORT
               ELSE
                   MOVE FUNCTION TRIM(WS-LINE-TEXT)
                       TO WS-PROF-FIRST-NAME(WS-PROFILE-IDX)
               END-IF
           END-IF.

           IF NOT PROFILE-ABORTED AND NOT END-OF-INPUT
               MOVE "Enter Last Name:" TO WS-MESSAGE
               CALL "WRITE-LINE" USING WS-MESSAGE
               CALL "READ-LINE" USING WS-LINE-TEXT
               IF NOT END-OF-INPUT
                   IF FUNCTION TRIM(WS-LINE-TEXT) = SPACES
                       MOVE "Last Name is required. Profile not saved."
                           TO WS-MESSAGE
                       CALL "WRITE-LINE" USING WS-MESSAGE
                       MOVE 'Y' TO WS-PROFILE-ABORT
                   ELSE
                       MOVE FUNCTION TRIM(WS-LINE-TEXT)
                           TO WS-PROF-LAST-NAME(WS-PROFILE-IDX)
                   END-IF
               END-IF
           END-IF.

           IF NOT PROFILE-ABORTED AND NOT END-OF-INPUT
               MOVE "Enter University/College Attended:" TO WS-MESSAGE
               CALL "WRITE-LINE" USING WS-MESSAGE
               CALL "READ-LINE" USING WS-LINE-TEXT
               IF NOT END-OF-INPUT
                   IF FUNCTION TRIM(WS-LINE-TEXT) = SPACES
                       MOVE "University/College Attended is required. Profile not saved."
                           TO WS-MESSAGE
                       CALL "WRITE-LINE" USING WS-MESSAGE
                       MOVE 'Y' TO WS-PROFILE-ABORT
                   ELSE
                       MOVE FUNCTION TRIM(WS-LINE-TEXT)
                           TO WS-PROF-UNIVERSITY(WS-PROFILE-IDX)
                   END-IF
               END-IF
           END-IF.

           IF NOT PROFILE-ABORTED AND NOT END-OF-INPUT
               MOVE "Enter Major:" TO WS-MESSAGE
               CALL "WRITE-LINE" USING WS-MESSAGE
               CALL "READ-LINE" USING WS-LINE-TEXT
               IF NOT END-OF-INPUT
                   IF FUNCTION TRIM(WS-LINE-TEXT) = SPACES
                       MOVE "Major is required. Profile not saved."
                           TO WS-MESSAGE
                       CALL "WRITE-LINE" USING WS-MESSAGE
                       MOVE 'Y' TO WS-PROFILE-ABORT
                   ELSE
                       MOVE FUNCTION TRIM(WS-LINE-TEXT)
                           TO WS-PROF-MAJOR(WS-PROFILE-IDX)
                   END-IF
               END-IF
           END-IF.

           IF NOT PROFILE-ABORTED AND NOT END-OF-INPUT
               MOVE "Enter Graduation Year (YYYY):" TO WS-MESSAGE
               CALL "WRITE-LINE" USING WS-MESSAGE
               CALL "READ-LINE" USING WS-LINE-TEXT
               IF NOT END-OF-INPUT
                   PERFORM VALIDATE-GRAD-YEAR
                   IF YEAR-IS-VALID
                       MOVE WS-YEAR-NUM TO WS-PROF-GRAD-YEAR(WS-PROFILE-IDX)
                   ELSE
                       MOVE 'Y' TO WS-PROFILE-ABORT
                   END-IF
               END-IF
           END-IF.

           IF NOT PROFILE-ABORTED AND NOT END-OF-INPUT
               MOVE "Enter About Me (optional, max 200 chars, enter blank line to skip):"
                   TO WS-MESSAGE
               CALL "WRITE-LINE" USING WS-MESSAGE
               CALL "READ-LINE" USING WS-LINE-TEXT
               IF NOT END-OF-INPUT
                   MOVE FUNCTION TRIM(WS-LINE-TEXT)
                       TO WS-PROF-ABOUT-ME(WS-PROFILE-IDX)
               END-IF
           END-IF.

           IF NOT PROFILE-ABORTED AND NOT END-OF-INPUT
               CALL "PROFILE-EXPERIENCE" USING WS-PROFILE-IDX WS-PROFILE-ABORT
           END-IF.

           IF NOT PROFILE-ABORTED AND NOT END-OF-INPUT
               CALL "PROFILE-EDUCATION" USING WS-PROFILE-IDX WS-PROFILE-ABORT
           END-IF.

           IF NOT PROFILE-ABORTED AND NOT END-OF-INPUT
               MOVE FUNCTION TRIM(WS-CURRENT-USERNAME)
                   TO WS-PROF-USERNAME(WS-PROFILE-IDX)
               MOVE 'Y' TO WS-PROF-HAS-DATA(WS-PROFILE-IDX)
               CALL "PROFILE-SAVE"
               MOVE "Profile saved successfully!" TO WS-MESSAGE
               CALL "WRITE-LINE" USING WS-MESSAGE
           ELSE
               IF PROFILE-IS-NEW
                   INITIALIZE WS-PROFILE(WS-PROFILE-IDX)
                   SUBTRACT 1 FROM WS-PROFILE-COUNT
               ELSE
                   MOVE WS-PROFILE-BACKUP TO WS-PROFILE(WS-PROFILE-IDX)
               END-IF
           END-IF.
           GOBACK.

       VALIDATE-GRAD-YEAR SECTION.
       VALIDATE-GRAD-YEAR-START.
           MOVE 'N' TO WS-YEAR-VALID.
           MOVE FUNCTION LENGTH(FUNCTION TRIM(WS-LINE-TEXT)) TO WS-YEAR-LEN.
           IF WS-YEAR-LEN NOT = 4
               MOVE "Graduation Year must be a valid 4-digit year. Profile not saved."
                   TO WS-MESSAGE
               CALL "WRITE-LINE" USING WS-MESSAGE
           ELSE
               MOVE FUNCTION TRIM(WS-LINE-TEXT) TO WS-YEAR-RAW
               MOVE 'Y' TO WS-YEAR-VALID
               PERFORM VARYING WS-YEAR-CHECK-IDX FROM 1 BY 1
                       UNTIL WS-YEAR-CHECK-IDX > 4
                   MOVE WS-YEAR-RAW(WS-YEAR-CHECK-IDX:1) TO WS-CUR-CHAR
                   IF WS-CUR-CHAR < '0' OR WS-CUR-CHAR > '9'
                       MOVE 'N' TO WS-YEAR-VALID
                   END-IF
               END-PERFORM
               IF YEAR-IS-VALID
                   MOVE WS-YEAR-RAW TO WS-YEAR-NUM
                   IF WS-YEAR-NUM <= 2025 OR WS-YEAR-NUM >= 2034
                       MOVE 'N' TO WS-YEAR-VALID
                       MOVE "Graduation Year must be greater than 2025 and less than 2034. Profile not saved."
                           TO WS-MESSAGE
                       CALL "WRITE-LINE" USING WS-MESSAGE
                   END-IF
               ELSE
                   MOVE "Graduation Year must be numeric. Profile not saved."
                       TO WS-MESSAGE
                   CALL "WRITE-LINE" USING WS-MESSAGE
               END-IF
           END-IF.

       COPY "InCollege-ProfileFindRow.cpy".
       END PROGRAM PROFILE-EDIT.
