      *>*******************************************************
      *> InCollege-ProfileEntries.cob
      *> Repeatable-entry collection for a profile: up to 3 work
      *> experience entries and up to 3 education entries, each
      *> gated by a "DONE to finish" prompt. Called by
      *> InCollege-Profile.cob's PROFILE-EDIT with the resolved
      *> profile row index.
      *>*******************************************************
       >>SOURCE FORMAT FREE
       IDENTIFICATION DIVISION.
       PROGRAM-ID. PROFILE-EXPERIENCE.

       DATA DIVISION.
       WORKING-STORAGE SECTION.
       COPY "InCollege-Common.cpy".
       COPY "InCollege-ProfileTable.cpy".

       01  WS-LINE-TEXT                PIC X(100).
       01  WS-MESSAGE                  PIC X(100).
       01  WS-EXP-IDX                  PIC 9.
       01  WS-EXP-DONE                 PIC X VALUE 'N'.
           88  EXPERIENCE-DONE                   VALUE 'Y'.

       LINKAGE SECTION.
       01  LS-PROFILE-IDX              PIC 9(2).
       01  LS-PROFILE-ABORT            PIC X.
           88  LS-PROFILE-ABORTED               VALUE 'Y'.

       PROCEDURE DIVISION USING LS-PROFILE-IDX LS-PROFILE-ABORT.
       PROFILE-EXPERIENCE-START.
           MOVE 'N' TO WS-EXP-DONE.
           MOVE 0 TO WS-PROF-EXP-COUNT(LS-PROFILE-IDX).
           MOVE 1 TO WS-EXP-IDX.
           PERFORM UNTIL EXPERIENCE-DONE OR WS-EXP-IDX > 3
                         OR LS-PROFILE-ABORTED OR END-OF-INPUT
               MOVE "Add Experience (optional, max 3 entries. Enter 'DONE' to finish):"
                   TO WS-MESSAGE
               CALL "WRITE-LINE" USING WS-MESSAGE
               CALL "READ-LINE" USING WS-LINE-TEXT
               IF END-OF-INPUT
                   MOVE 'Y' TO LS-PROFILE-ABORT
               ELSE
                   IF FUNCTION UPPER-CASE(FUNCTION TRIM(WS-LINE-TEXT)) = "DONE"
                       MOVE 'Y' TO WS-EXP-DONE
                   ELSE
                       PERFORM COLLECT-ONE-EXPERIENCE
                       IF NOT LS-PROFILE-ABORTED
                           ADD 1 TO WS-EXP-IDX
                       END-IF
                   END-IF
               END-IF
           END-PERFORM.
           GOBACK.

       COLLECT-ONE-EXPERIENCE SECTION.
       COLLECT-ONE-EXPERIENCE-START.
           MOVE SPACES TO WS-MESSAGE
           STRING "Experience #" DELIMITED BY SIZE
                  WS-EXP-IDX DELIMITED BY SIZE
                  " - Title:" DELIMITED BY SIZE
                  INTO WS-MESSAGE.
           CALL "WRITE-LINE" USING WS-MESSAGE.
           CALL "READ-LINE" USING WS-LINE-TEXT.
           IF END-OF-INPUT
               MOVE 'Y' TO LS-PROFILE-ABORT
           ELSE
               IF FUNCTION TRIM(WS-LINE-TEXT) = SPACES
                   MOVE "Experience Title is required. Profile not saved."
                       TO WS-MESSAGE
                   CALL "WRITE-LINE" USING WS-MESSAGE
                   MOVE 'Y' TO LS-PROFILE-ABORT
               ELSE
                   MOVE FUNCTION TRIM(WS-LINE-TEXT)
                       TO WS-PROF-EXP-TITLE(LS-PROFILE-IDX, WS-EXP-IDX)
               END-IF
           END-IF.

           IF NOT LS-PROFILE-ABORTED AND NOT END-OF-INPUT
               MOVE SPACES TO WS-MESSAGE
               STRING "Experience #" DELIMITED BY SIZE
                      WS-EXP-IDX DELIMITED BY SIZE
                      " - Company/Organization:" DELIMITED BY SIZE
                      INTO WS-MESSAGE
               CALL "WRITE-LINE" USING WS-MESSAGE
               CALL "READ-LINE" USING WS-LINE-TEXT
               IF NOT END-OF-INPUT
                   IF FUNCTION TRIM(WS-LINE-TEXT) = SPACES
                       MOVE "Experience Company/Organization is required. Profile not saved."
                           TO WS-MESSAGE
                       CALL "WRITE-LINE" USING WS-MESSAGE
                       MOVE 'Y' TO LS-PROFILE-ABORT
                   ELSE
                       MOVE FUNCTION TRIM(WS-LINE-TEXT)
                           TO WS-PROF-EXP-COMPANY(LS-PROFILE-IDX, WS-EXP-IDX)
                   END-IF
               END-IF
           END-IF.

           IF NOT LS-PROFILE-ABORTED AND NOT END-OF-INPUT
               MOVE SPACES TO WS-MESSAGE
               STRING "Experience #" DELIMITED BY SIZE
                      WS-EXP-IDX DELIMITED BY SIZE
                      " - Dates (e.g., Summer 2024):" DELIMITED BY SIZE
                      INTO WS-MESSAGE
               CALL "WRITE-LINE" USING WS-MESSAGE
               CALL "READ-LINE" USING WS-LINE-TEXT
               IF NOT END-OF-INPUT
                   IF FUNCTION TRIM(WS-LINE-TEXT) = SPACES
                       MOVE "Experience Dates is required. Profile not saved."
                           TO WS-MESSAGE
                       CALL "WRITE-LINE" USING WS-MESSAGE
                       MOVE 'Y' TO LS-PROFILE-ABORT
                   ELSE
                       MOVE FUNCTION TRIM(WS-LINE-TEXT)
                           TO WS-PROF-EXP-DATES(LS-PROFILE-IDX, WS-EXP-IDX)
                   END-IF
               END-IF
           END-IF.

           IF NOT LS-PROFILE-ABORTED AND NOT END-OF-INPUT
               MOVE SPACES TO WS-MESSAGE
               STRING "Experience #" DELIMITED BY SIZE
                      WS-EXP-IDX DELIMITED BY SIZE
                      " - Description (optional, max 100 chars, blank to skip):"
                      DELIMITED BY SIZE
                      INTO WS-MESSAGE
               CALL "WRITE-LINE" USING WS-MESSAGE
               CALL "READ-LINE" USING WS-LINE-TEXT
               IF NOT END-OF-INPUT
                   MOVE FUNCTION TRIM(WS-LINE-TEXT)
                       TO WS-PROF-EXP-DESC(LS-PROFILE-IDX, WS-EXP-IDX)
               END-IF
           END-IF.

           IF NOT LS-PROFILE-ABORTED AND NOT END-OF-INPUT
               ADD 1 TO WS-PROF-EXP-COUNT(LS-PROFILE-IDX)
           END-IF.
       END PROGRAM PROFILE-EXPERIENCE.

       IDENTIFICATION DIVISION.
       PROGRAM-ID. PROFILE-EDUCATION.

       DATA DIVISION.
       WORKING-STORAGE SECTION.
       COPY "InCollege-Common.cpy".
       COPY "InCollege-ProfileTable.cpy".

       01  WS-LINE-TEXT                PIC X(100).
       01  WS-MESSAGE                  PIC X(100).
       01  WS-EDU-IDX                  PIC 9.
       01  WS-EDU-DONE                 PIC X VALUE 'N'.
           88  EDUCATION-DONE                    VALUE 'Y'.

       LINKAGE SECTION.
       01  LS-PROFILE-IDX              PIC 9(2).
       01  LS-PROFILE-ABORT            PIC X.
           88  LS-PROFILE-ABORTED                VALUE 'Y'.

       PROCEDURE DIVISION USING LS-PROFILE-IDX LS-PROFILE-ABORT.
       PROFILE-EDUCATION-START.
           MOVE 'N' TO WS-EDU-DONE.
           MOVE 0 TO WS-PROF-EDU-COUNT(LS-PROFILE-IDX).
           MOVE 1 TO WS-EDU-IDX.
           PERFORM UNTIL EDUCATION-DONE OR WS-EDU-IDX > 3
                         OR LS-PROFILE-ABORTED OR END-OF-INPUT
               MOVE "Add Education (optional, max 3 entries. Enter 'DONE' to finish):"
                   TO WS-MESSAGE
               CALL "WRITE-LINE" USING WS-MESSAGE
               CALL "READ-LINE" USING WS-LINE-TEXT
               IF END-OF-INPUT
                   MOVE 'Y' TO LS-PROFILE-ABORT
               ELSE
                   IF FUNCTION UPPER-CASE(FUNCTION TRIM(WS-LINE-TEXT)) = "DONE"
                       MOVE 'Y' TO WS-EDU-DONE
                   ELSE
                       PERFORM COLLECT-ONE-EDUCATION
                       IF NOT LS-PROFILE-ABORTED
                           ADD 1 TO WS-EDU-IDX
                       END-IF
                   END-IF
               END-IF
           END-PERFORM.
           GOBACK.

       COLLECT-ONE-EDUCATION SECTION.
       COLLECT-ONE-EDUCATION-START.
           MOVE SPACES TO WS-MESSAGE
           STRING "Education #" DELIMITED BY SIZE
                  WS-EDU-IDX DELIMITED BY SIZE
                  " - Degree:" DELIMITED BY SIZE
                  INTO WS-MESSAGE.
           CALL "WRITE-LINE" USING WS-MESSAGE.
           CALL "READ-LINE" USING WS-LINE-TEXT.
           IF END-OF-INPUT
               MOVE 'Y' TO LS-PROFILE-ABORT
           ELSE
               IF FUNCTION TRIM(WS-LINE-TEXT) = SPACES
                   MOVE "Education Degree is required. Profile not saved."
                       TO WS-MESSAGE
                   CALL "WRITE-LINE" USING WS-MESSAGE
                   MOVE 'Y' TO LS-PROFILE-ABORT
               ELSE
                   MOVE FUNCTION TRIM(WS-LINE-TEXT)
                       TO WS-PROF-EDU-DEGREE(LS-PROFILE-IDX, WS-EDU-IDX)
               END-IF
           END-IF.

           IF NOT LS-PROFILE-ABORTED AND NOT END-OF-INPUT
               MOVE SPACES TO WS-MESSAGE
               STRING "Education #" DELIMITED BY SIZE
                      WS-EDU-IDX DELIMITED BY SIZE
                      " - University/College:" DELIMITED BY SIZE
                      INTO WS-MESSAGE
               CALL "WRITE-LINE" USING WS-MESSAGE
               CALL "READ-LINE" USING WS-LINE-TEXT
               IF NOT END-OF-INPUT
                   IF FUNCTION TRIM(WS-LINE-TEXT) = SPACES
                       MOVE "Education University/College is required. Profile not saved."
                           TO WS-MESSAGE
                       CALL "WRITE-LINE" USING WS-MESSAGE
                       MOVE 'Y' TO LS-PROFILE-ABORT
                   ELSE
                       MOVE FUNCTION TRIM(WS-LINE-TEXT)
                           TO WS-PROF-EDU-UNIVERSITY(LS-PROFILE-IDX, WS-EDU-IDX)
                   END-IF
               END-IF
           END-IF.

           IF NOT LS-PROFILE-ABORTED AND NOT END-OF-INPUT
               MOVE SPACES TO WS-MESSAGE
               STRING "Education #" DELIMITED BY SIZE
                      WS-EDU-IDX DELIMITED BY SIZE
                      " - Years Attended (e.g., 2023-2025):" DELIMITED BY SIZE
                      INTO WS-MESSAGE
               CALL "WRITE-LINE" USING WS-MESSAGE
               CALL "READ-LINE" USING WS-LINE-TEXT
               IF NOT END-OF-INPUT
                   IF FUNCTION TRIM(WS-LINE-TEXT) = SPACES
                       MOVE "Education Years Attended is required. Profile not saved."
                           TO WS-MESSAGE
                       CALL "WRITE-LINE" USING WS-MESSAGE
                       MOVE 'Y' TO LS-PROFILE-ABORT
                   ELSE
                       MOVE FUNCTION TRIM(WS-LINE-TEXT)
                           TO WS-PROF-EDU-YEARS(LS-PROFILE-IDX, WS-EDU-IDX)
                   END-IF
               END-IF
           END-IF.

           IF NOT LS-PROFILE-ABORTED AND NOT END-OF-INPUT
               ADD 1 TO WS-PROF-EDU-COUNT(LS-PROFILE-IDX)
           END-IF.
       END PROGRAM PROFILE-EDUCATION.
