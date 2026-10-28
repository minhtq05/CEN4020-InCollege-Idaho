      *>*******************************************************
      *> InCollege-SavedJobs.cob
      *> SAVED-JOBS-MENU: lets the logged-in user save a posted
      *> job, list their saved jobs, and unsave one. Saved jobs
      *> are stored as (username, job ID) pairs in
      *> InCollege-SavedJobs.txt so they persist across logins.
      *> Called from JOB-MENU, which has already loaded the shared
      *> job table (InCollege-JobTable.cpy).
      *>*******************************************************
       >>SOURCE FORMAT FREE
       IDENTIFICATION DIVISION.
       PROGRAM-ID. SAVED-JOBS-MENU.

       ENVIRONMENT DIVISION.
       INPUT-OUTPUT SECTION.
       FILE-CONTROL.
           SELECT SAVED-JOB-FILE ASSIGN TO "InCollege-SavedJobs.txt"
               ORGANIZATION IS LINE SEQUENTIAL
               FILE STATUS IS WS-SAVED-FILE-STATUS.

       DATA DIVISION.
       FILE SECTION.
       FD  SAVED-JOB-FILE.
       01  SAVED-JOB-RECORD.
           05  SAVED-REC-USERNAME      PIC X(20).
           05  SAVED-REC-JOB-ID        PIC 9(3).

       WORKING-STORAGE SECTION.
       COPY "InCollege-Common.cpy".
       COPY "InCollege-JobTable.cpy".

       01  WS-SAVED-COUNT              PIC 9(3).
       01  WS-SAVED-TABLE.
           05  WS-SAVED-ENTRY OCCURS 200 TIMES.
               10  WS-SAVED-USERNAME   PIC X(20).
               10  WS-SAVED-JOB-ID     PIC 9(3).

       01  WS-SAVED-FILE-STATUS        PIC XX.
       01  WS-SAVED-EOF                PIC X.
       01  WS-SAVED-WRITE-OK           PIC X.

       01  WS-SAVED-IDX                PIC 9(3).
       01  WS-SAVED-POS                PIC 9(3).
       01  WS-MY-SAVED-COUNT           PIC 9(3).
       01  WS-JOB-IDX                  PIC 99.
       01  WS-JOB-POS                  PIC 99.

       01  WS-ID-TEXT                  PIC X(3).
       01  WS-ID-LEN                   PIC 9(3).
       01  WS-ID-OK                    PIC X.
       01  WS-TARGET-ID                PIC 9(3).
       01  WS-ID-DISPLAY               PIC ZZ9.

       01  WS-MESSAGE                  PIC X(100).
       01  WS-LONG-MESSAGE             PIC X(250).
       01  WS-LINE-TEXT                PIC X(100).
       01  WS-SAVED-CHOICE             PIC X(20).

       01  WS-SAVED-MENU-DONE          PIC X VALUE 'N'.
           88  SAVED-MENU-DONE                   VALUE 'Y'.

       PROCEDURE DIVISION.
       SAVED-JOBS-START.
           PERFORM SAVED-LOAD.
           MOVE 'N' TO WS-SAVED-MENU-DONE.
           MOVE "--- Saved Jobs ---" TO WS-MESSAGE.
           CALL "WRITE-LINE" USING WS-MESSAGE.
           PERFORM UNTIL SAVED-MENU-DONE OR END-OF-INPUT
               MOVE "1. Save a Job" TO WS-MESSAGE
               CALL "WRITE-LINE" USING WS-MESSAGE
               MOVE "2. View My Saved Jobs" TO WS-MESSAGE
               CALL "WRITE-LINE" USING WS-MESSAGE
               MOVE "3. Unsave a Job" TO WS-MESSAGE
               CALL "WRITE-LINE" USING WS-MESSAGE
               MOVE "4. Back to Job Menu" TO WS-MESSAGE
               CALL "WRITE-LINE" USING WS-MESSAGE
               MOVE "Enter your choice:" TO WS-MESSAGE
               CALL "WRITE-LINE" USING WS-MESSAGE
               CALL "READ-LINE" USING WS-LINE-TEXT
               IF NOT END-OF-INPUT
                   MOVE WS-LINE-TEXT TO WS-SAVED-CHOICE
                   EVALUATE FUNCTION TRIM(WS-SAVED-CHOICE)
                       WHEN "1"
                           PERFORM SAVE-A-JOB
                       WHEN "2"
                           PERFORM VIEW-SAVED-JOBS
                       WHEN "3"
                           PERFORM UNSAVE-A-JOB
                       WHEN "4"
                           MOVE 'Y' TO WS-SAVED-MENU-DONE
                       WHEN OTHER
                           MOVE "Invalid choice, please try again."
                               TO WS-MESSAGE
                           CALL "WRITE-LINE" USING WS-MESSAGE
                   END-EVALUATE
               END-IF
           END-PERFORM.
           GOBACK.

       SAVE-A-JOB SECTION.
       SAVE-A-JOB-START.
           IF WS-JOB-COUNT = 0
               MOVE "There are no jobs available to save." TO WS-MESSAGE
               CALL "WRITE-LINE" USING WS-MESSAGE
           ELSE
               MOVE "--- Available Jobs ---" TO WS-MESSAGE
               CALL "WRITE-LINE" USING WS-MESSAGE
               PERFORM VARYING WS-JOB-POS FROM 1 BY 1
                       UNTIL WS-JOB-POS > WS-JOB-COUNT
                   PERFORM WRITE-JOB-LINE
               END-PERFORM
               MOVE "Enter the ID of the job to save:" TO WS-MESSAGE
               CALL "WRITE-LINE" USING WS-MESSAGE
               CALL "READ-LINE" USING WS-LINE-TEXT
               IF NOT END-OF-INPUT
                   PERFORM PARSE-JOB-ID
                   IF WS-ID-OK = 'Y'
                       PERFORM FIND-JOB
                   ELSE
                       MOVE 0 TO WS-JOB-POS
                   END-IF
                   IF WS-JOB-POS > 0
                       PERFORM FIND-SAVED
                   END-IF
                   EVALUATE TRUE
                       WHEN WS-JOB-POS = 0
                           MOVE "Invalid job ID." TO WS-MESSAGE
                       WHEN WS-SAVED-POS > 0
                           MOVE "You have already saved this job."
                               TO WS-MESSAGE
                       WHEN WS-SAVED-COUNT >= 200
                           MOVE "The saved jobs list is full. Job not saved."
                               TO WS-MESSAGE
                       WHEN OTHER
                           ADD 1 TO WS-SAVED-COUNT
                           MOVE WS-CURRENT-USERNAME
                               TO WS-SAVED-USERNAME(WS-SAVED-COUNT)
                           MOVE WS-TARGET-ID
                               TO WS-SAVED-JOB-ID(WS-SAVED-COUNT)
                           PERFORM SAVED-SAVE
                           IF WS-SAVED-WRITE-OK = 'Y'
                               MOVE "Job saved successfully!" TO WS-MESSAGE
                           ELSE
                               SUBTRACT 1 FROM WS-SAVED-COUNT
                               MOVE "Unable to save job." TO WS-MESSAGE
                           END-IF
                   END-EVALUATE
                   CALL "WRITE-LINE" USING WS-MESSAGE
               END-IF
           END-IF.

       VIEW-SAVED-JOBS SECTION.
       VIEW-SAVED-JOBS-START.
           MOVE "--- My Saved Jobs ---" TO WS-MESSAGE.
           CALL "WRITE-LINE" USING WS-MESSAGE.
           PERFORM LIST-MY-SAVED-JOBS.
           IF WS-MY-SAVED-COUNT = 0
               MOVE "You have no saved jobs." TO WS-MESSAGE
               CALL "WRITE-LINE" USING WS-MESSAGE
           END-IF.

       UNSAVE-A-JOB SECTION.
       UNSAVE-A-JOB-START.
           MOVE "--- My Saved Jobs ---" TO WS-MESSAGE.
           CALL "WRITE-LINE" USING WS-MESSAGE.
           PERFORM LIST-MY-SAVED-JOBS.
           IF WS-MY-SAVED-COUNT = 0
               MOVE "You have no saved jobs." TO WS-MESSAGE
               CALL "WRITE-LINE" USING WS-MESSAGE
           ELSE
               MOVE "Enter the ID of the job to unsave:" TO WS-MESSAGE
               CALL "WRITE-LINE" USING WS-MESSAGE
               CALL "READ-LINE" USING WS-LINE-TEXT
               IF NOT END-OF-INPUT
                   MOVE 0 TO WS-SAVED-POS
                   PERFORM PARSE-JOB-ID
                   IF WS-ID-OK = 'Y'
                       PERFORM FIND-SAVED
                   END-IF
                   IF WS-SAVED-POS = 0
                       MOVE "That job is not in your saved jobs."
                           TO WS-MESSAGE
                   ELSE
                       PERFORM VARYING WS-SAVED-IDX FROM WS-SAVED-POS BY 1
                               UNTIL WS-SAVED-IDX >= WS-SAVED-COUNT
                           MOVE WS-SAVED-ENTRY(WS-SAVED-IDX + 1)
                               TO WS-SAVED-ENTRY(WS-SAVED-IDX)
                       END-PERFORM
                       MOVE SPACES TO WS-SAVED-ENTRY(WS-SAVED-COUNT)
                       SUBTRACT 1 FROM WS-SAVED-COUNT
                       PERFORM SAVED-SAVE
                       IF WS-SAVED-WRITE-OK = 'Y'
                           MOVE "Job removed from saved jobs." TO WS-MESSAGE
                       ELSE
                           MOVE "Unable to update saved jobs." TO WS-MESSAGE
                       END-IF
                   END-IF
                   CALL "WRITE-LINE" USING WS-MESSAGE
               END-IF
           END-IF.

       SAVED-HELPERS SECTION.
      *>   Write the current user's saved jobs, one line each, and
      *>   count them in WS-MY-SAVED-COUNT. Saved IDs whose job is no
      *>   longer on file are skipped.
       LIST-MY-SAVED-JOBS.
           MOVE 0 TO WS-MY-SAVED-COUNT
           PERFORM VARYING WS-SAVED-IDX FROM 1 BY 1
                   UNTIL WS-SAVED-IDX > WS-SAVED-COUNT
               IF WS-SAVED-USERNAME(WS-SAVED-IDX) = WS-CURRENT-USERNAME
                   MOVE WS-SAVED-JOB-ID(WS-SAVED-IDX) TO WS-TARGET-ID
                   PERFORM FIND-JOB
                   IF WS-JOB-POS > 0
                       ADD 1 TO WS-MY-SAVED-COUNT
                       PERFORM WRITE-JOB-LINE
                   END-IF
               END-IF
           END-PERFORM.

      *>   "ID. Title - Employer (Location)" for job WS-JOB-POS.
       WRITE-JOB-LINE.
           MOVE WS-JOB-ID(WS-JOB-POS) TO WS-ID-DISPLAY
           MOVE SPACES TO WS-LONG-MESSAGE
           STRING FUNCTION TRIM(WS-ID-DISPLAY) DELIMITED BY SIZE
                  ". " DELIMITED BY SIZE
                  FUNCTION TRIM(WS-JOB-TITLE(WS-JOB-POS)) DELIMITED BY SIZE
                  " - " DELIMITED BY SIZE
                  FUNCTION TRIM(WS-JOB-EMPLOYER(WS-JOB-POS)) DELIMITED BY SIZE
                  " (" DELIMITED BY SIZE
                  FUNCTION TRIM(WS-JOB-LOCATION(WS-JOB-POS)) DELIMITED BY SIZE
                  ")" DELIMITED BY SIZE
                  INTO WS-LONG-MESSAGE
           CALL "WRITE-LONG-LINE" USING WS-LONG-MESSAGE.

      *>   WS-LINE-TEXT must be a 1-3 digit job ID; sets WS-ID-OK and
      *>   WS-TARGET-ID.
       PARSE-JOB-ID.
           MOVE 'N' TO WS-ID-OK
           MOVE 0 TO WS-TARGET-ID
           IF WS-LINE-TEXT NOT = SPACES
               MOVE FUNCTION LENGTH(FUNCTION TRIM(WS-LINE-TEXT))
                   TO WS-ID-LEN
               IF WS-ID-LEN <= 3
                   MOVE FUNCTION TRIM(WS-LINE-TEXT) TO WS-ID-TEXT
                   IF WS-ID-TEXT(1:WS-ID-LEN) IS NUMERIC
                       MOVE FUNCTION NUMVAL(WS-ID-TEXT(1:WS-ID-LEN))
                           TO WS-TARGET-ID
                       MOVE 'Y' TO WS-ID-OK
                   END-IF
               END-IF
           END-IF.

      *>   Position of job WS-TARGET-ID in the job table, or 0.
       FIND-JOB.
           MOVE 0 TO WS-JOB-POS
           PERFORM VARYING WS-JOB-IDX FROM 1 BY 1
                   UNTIL WS-JOB-IDX > WS-JOB-COUNT OR WS-JOB-POS > 0
               IF WS-JOB-ID(WS-JOB-IDX) = WS-TARGET-ID
                   MOVE WS-JOB-IDX TO WS-JOB-POS
               END-IF
           END-PERFORM.

      *>   Position of (current user, WS-TARGET-ID) in the saved
      *>   table, or 0.
       FIND-SAVED.
           MOVE 0 TO WS-SAVED-POS
           PERFORM VARYING WS-SAVED-IDX FROM 1 BY 1
                   UNTIL WS-SAVED-IDX > WS-SAVED-COUNT OR WS-SAVED-POS > 0
               IF WS-SAVED-USERNAME(WS-SAVED-IDX) = WS-CURRENT-USERNAME
                  AND WS-SAVED-JOB-ID(WS-SAVED-IDX) = WS-TARGET-ID
                   MOVE WS-SAVED-IDX TO WS-SAVED-POS
               END-IF
           END-PERFORM.

       SAVED-FILE-IO SECTION.
       SAVED-LOAD.
           INITIALIZE WS-SAVED-TABLE
           MOVE 0 TO WS-SAVED-COUNT
           MOVE 'N' TO WS-SAVED-EOF
           OPEN INPUT SAVED-JOB-FILE
           IF WS-SAVED-FILE-STATUS = "00"
               PERFORM UNTIL WS-SAVED-EOF = 'Y'
                   READ SAVED-JOB-FILE
                       AT END
                           MOVE 'Y' TO WS-SAVED-EOF
                       NOT AT END
                           IF WS-SAVED-COUNT < 200
                              AND SAVED-REC-JOB-ID IS NUMERIC
                               ADD 1 TO WS-SAVED-COUNT
                               MOVE SAVED-JOB-RECORD
                                   TO WS-SAVED-ENTRY(WS-SAVED-COUNT)
                           END-IF
                   END-READ
               END-PERFORM
               CLOSE SAVED-JOB-FILE
           END-IF.

      *>   Rewrite the whole saved-jobs file from the table.
       SAVED-SAVE.
           MOVE 'Y' TO WS-SAVED-WRITE-OK
           OPEN OUTPUT SAVED-JOB-FILE
           IF WS-SAVED-FILE-STATUS = "00"
               PERFORM VARYING WS-SAVED-IDX FROM 1 BY 1
                       UNTIL WS-SAVED-IDX > WS-SAVED-COUNT
                   MOVE WS-SAVED-ENTRY(WS-SAVED-IDX) TO SAVED-JOB-RECORD
                   WRITE SAVED-JOB-RECORD
                   IF WS-SAVED-FILE-STATUS NOT = "00"
                       MOVE 'N' TO WS-SAVED-WRITE-OK
                   END-IF
               END-PERFORM
               CLOSE SAVED-JOB-FILE
           ELSE
               MOVE 'N' TO WS-SAVED-WRITE-OK
           END-IF.
       END PROGRAM SAVED-JOBS-MENU.
