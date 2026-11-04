      *>*******************************************************
      *> InCollege-ApplyJob.cob
      *> BROWSE-JOBS: lists every posted job, shows the full
      *> details of the one the user picks, and lets them apply.
      *> Applications are stored as (username, job ID) pairs in
      *> InCollege-Applications.txt so they persist across runs.
      *> Called from JOB-MENU, which has already loaded the shared
      *> job table (InCollege-JobTable.cpy).
      *>*******************************************************
       >>SOURCE FORMAT FREE
       IDENTIFICATION DIVISION.
       PROGRAM-ID. BROWSE-JOBS.

       ENVIRONMENT DIVISION.
       INPUT-OUTPUT SECTION.
       FILE-CONTROL.
           SELECT APPLICATION-FILE ASSIGN TO "InCollege-Applications.txt"
               ORGANIZATION IS LINE SEQUENTIAL
               FILE STATUS IS WS-APP-FILE-STATUS.

       DATA DIVISION.
       FILE SECTION.
       FD  APPLICATION-FILE.
       COPY "InCollege-ApplicationRecord.cpy".

       WORKING-STORAGE SECTION.
       COPY "InCollege-Common.cpy".
       COPY "InCollege-JobTable.cpy".

       01  WS-APP-FILE-STATUS          PIC XX.
       01  WS-APP-EOF                  PIC X.
       01  WS-ALREADY-APPLIED          PIC X.
       01  WS-APP-WRITE-OK             PIC X.

       01  WS-JOB-IDX                  PIC 99.
       01  WS-JOB-POS                  PIC 99.

       01  WS-ID-TEXT                  PIC X(3).
       01  WS-ID-LEN                   PIC 9(3).
       01  WS-ID-OK                    PIC X.
       01  WS-TARGET-ID                PIC 9(3).
       01  WS-ID-DISPLAY               PIC ZZ9.

       01  WS-FIELD-LABEL              PIC X(20).
       01  WS-FIELD-VALUE              PIC X(200).

       01  WS-MESSAGE                  PIC X(100).
       01  WS-LONG-MESSAGE             PIC X(250).
       01  WS-LINE-TEXT                PIC X(100).

       01  WS-BROWSE-DONE              PIC X.
       01  WS-DETAIL-DONE              PIC X.

       PROCEDURE DIVISION.
       BROWSE-JOBS-START.
           MOVE 'N' TO WS-BROWSE-DONE.
           PERFORM UNTIL WS-BROWSE-DONE = 'Y' OR END-OF-INPUT
               MOVE "--- Available Job Listings ---" TO WS-MESSAGE
               CALL "WRITE-LINE" USING WS-MESSAGE
               IF WS-JOB-COUNT = 0
                   MOVE "There are no jobs available." TO WS-MESSAGE
                   CALL "WRITE-LINE" USING WS-MESSAGE
                   MOVE 'Y' TO WS-BROWSE-DONE
               ELSE
                   PERFORM VARYING WS-JOB-POS FROM 1 BY 1
                           UNTIL WS-JOB-POS > WS-JOB-COUNT
                       PERFORM WRITE-JOB-SUMMARY
                   END-PERFORM
                   MOVE "-----------------------------" TO WS-MESSAGE
                   CALL "WRITE-LINE" USING WS-MESSAGE
                   MOVE "Enter job number to view details, or 0 to go back:"
                       TO WS-MESSAGE
                   CALL "WRITE-LINE" USING WS-MESSAGE
                   CALL "READ-LINE" USING WS-LINE-TEXT
                   IF NOT END-OF-INPUT
                       IF FUNCTION TRIM(WS-LINE-TEXT) = "0"
                           MOVE 'Y' TO WS-BROWSE-DONE
                       ELSE
                           PERFORM PARSE-JOB-ID
                           IF WS-ID-OK = 'Y'
                               PERFORM FIND-JOB
                           ELSE
                               MOVE 0 TO WS-JOB-POS
                           END-IF
                           IF WS-JOB-POS = 0
                               MOVE "Invalid job number." TO WS-MESSAGE
                               CALL "WRITE-LINE" USING WS-MESSAGE
                           ELSE
                               PERFORM VIEW-JOB-DETAILS
                           END-IF
                       END-IF
                   END-IF
               END-IF
           END-PERFORM.
           GOBACK.

       JOB-DETAILS SECTION.
      *>   Full details of job WS-JOB-POS, then the apply/back menu.
       VIEW-JOB-DETAILS.
           MOVE "--- Job Details ---" TO WS-MESSAGE
           CALL "WRITE-LINE" USING WS-MESSAGE
           MOVE "Title:" TO WS-FIELD-LABEL
           MOVE WS-JOB-TITLE(WS-JOB-POS) TO WS-FIELD-VALUE
           PERFORM WRITE-FIELD
           MOVE "Description:" TO WS-FIELD-LABEL
           MOVE WS-JOB-DESCRIPTION(WS-JOB-POS) TO WS-FIELD-VALUE
           PERFORM WRITE-FIELD
           MOVE "Employer:" TO WS-FIELD-LABEL
           MOVE WS-JOB-EMPLOYER(WS-JOB-POS) TO WS-FIELD-VALUE
           PERFORM WRITE-FIELD
           MOVE "Location:" TO WS-FIELD-LABEL
           MOVE WS-JOB-LOCATION(WS-JOB-POS) TO WS-FIELD-VALUE
           PERFORM WRITE-FIELD
           IF FUNCTION TRIM(WS-JOB-SALARY(WS-JOB-POS)) NOT = "NONE"
              AND WS-JOB-SALARY(WS-JOB-POS) NOT = SPACES
               MOVE "Salary:" TO WS-FIELD-LABEL
               MOVE WS-JOB-SALARY(WS-JOB-POS) TO WS-FIELD-VALUE
               PERFORM WRITE-FIELD
           END-IF
           MOVE "-------------------" TO WS-MESSAGE
           CALL "WRITE-LINE" USING WS-MESSAGE
           MOVE 'N' TO WS-DETAIL-DONE
           PERFORM UNTIL WS-DETAIL-DONE = 'Y' OR END-OF-INPUT
               MOVE "1. Apply for this Job" TO WS-MESSAGE
               CALL "WRITE-LINE" USING WS-MESSAGE
               MOVE "2. Back to Job List" TO WS-MESSAGE
               CALL "WRITE-LINE" USING WS-MESSAGE
               MOVE "Enter your choice:" TO WS-MESSAGE
               CALL "WRITE-LINE" USING WS-MESSAGE
               CALL "READ-LINE" USING WS-LINE-TEXT
               IF NOT END-OF-INPUT
                   EVALUATE FUNCTION TRIM(WS-LINE-TEXT)
                       WHEN "1"
                           PERFORM APPLY-FOR-JOB
                           MOVE 'Y' TO WS-DETAIL-DONE
                       WHEN "2"
                           MOVE 'Y' TO WS-DETAIL-DONE
                       WHEN OTHER
                           MOVE "Invalid choice, please try again."
                               TO WS-MESSAGE
                           CALL "WRITE-LINE" USING WS-MESSAGE
                   END-EVALUATE
               END-IF
           END-PERFORM.

      *>   Record an application by the current user for job
      *>   WS-JOB-POS, unless they have already applied for it.
       APPLY-FOR-JOB.
           PERFORM CHECK-ALREADY-APPLIED
           IF WS-ALREADY-APPLIED = 'Y'
               MOVE "You have already applied for this job." TO WS-MESSAGE
               CALL "WRITE-LINE" USING WS-MESSAGE
           ELSE
               PERFORM SAVE-APPLICATION
               IF WS-APP-WRITE-OK = 'Y'
                   MOVE SPACES TO WS-LONG-MESSAGE
                   STRING "Your application for " DELIMITED BY SIZE
                          FUNCTION TRIM(WS-JOB-TITLE(WS-JOB-POS))
                              DELIMITED BY SIZE
                          " at " DELIMITED BY SIZE
                          FUNCTION TRIM(WS-JOB-EMPLOYER(WS-JOB-POS))
                              DELIMITED BY SIZE
                          " has been submitted." DELIMITED BY SIZE
                          INTO WS-LONG-MESSAGE
                   CALL "WRITE-LONG-LINE" USING WS-LONG-MESSAGE
               ELSE
                   MOVE "Unable to save your application. Please try again."
                       TO WS-MESSAGE
                   CALL "WRITE-LINE" USING WS-MESSAGE
               END-IF
           END-IF.

       BROWSE-HELPERS SECTION.
      *>   "ID. Title at Employer (Location)" for job WS-JOB-POS.
       WRITE-JOB-SUMMARY.
           MOVE WS-JOB-ID(WS-JOB-POS) TO WS-ID-DISPLAY
           MOVE SPACES TO WS-LONG-MESSAGE
           STRING FUNCTION TRIM(WS-ID-DISPLAY) DELIMITED BY SIZE
                  ". " DELIMITED BY SIZE
                  FUNCTION TRIM(WS-JOB-TITLE(WS-JOB-POS)) DELIMITED BY SIZE
                  " at " DELIMITED BY SIZE
                  FUNCTION TRIM(WS-JOB-EMPLOYER(WS-JOB-POS)) DELIMITED BY SIZE
                  " (" DELIMITED BY SIZE
                  FUNCTION TRIM(WS-JOB-LOCATION(WS-JOB-POS)) DELIMITED BY SIZE
                  ")" DELIMITED BY SIZE
                  INTO WS-LONG-MESSAGE
           CALL "WRITE-LONG-LINE" USING WS-LONG-MESSAGE.

      *>   "Label Value" line from WS-FIELD-LABEL and WS-FIELD-VALUE.
       WRITE-FIELD.
           MOVE SPACES TO WS-LONG-MESSAGE
           STRING FUNCTION TRIM(WS-FIELD-LABEL) DELIMITED BY SIZE
                  " " DELIMITED BY SIZE
                  FUNCTION TRIM(WS-FIELD-VALUE) DELIMITED BY SIZE
                  INTO WS-LONG-MESSAGE
           CALL "WRITE-LONG-LINE" USING WS-LONG-MESSAGE.

      *>   WS-LINE-TEXT must be a 1-3 digit job number; sets WS-ID-OK
      *>   and WS-TARGET-ID.
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

       APPLICATION-FILE-IO SECTION.
      *>   Scan the applications file for (current user, job
      *>   WS-JOB-POS); a missing file means no applications yet.
       CHECK-ALREADY-APPLIED.
           MOVE 'N' TO WS-ALREADY-APPLIED
           MOVE 'N' TO WS-APP-EOF
           OPEN INPUT APPLICATION-FILE
           IF WS-APP-FILE-STATUS = "00"
               PERFORM UNTIL WS-APP-EOF = 'Y' OR WS-ALREADY-APPLIED = 'Y'
                   READ APPLICATION-FILE
                       AT END
                           MOVE 'Y' TO WS-APP-EOF
                       NOT AT END
                           IF APP-REC-USERNAME = WS-CURRENT-USERNAME
                              AND APP-REC-JOB-ID IS NUMERIC
                              AND APP-REC-JOB-ID = WS-JOB-ID(WS-JOB-POS)
                               MOVE 'Y' TO WS-ALREADY-APPLIED
                           END-IF
                   END-READ
               END-PERFORM
               CLOSE APPLICATION-FILE
           END-IF.

      *>   Append (current user, job WS-JOB-POS); sets WS-APP-WRITE-OK.
       SAVE-APPLICATION.
           MOVE 'N' TO WS-APP-WRITE-OK
           OPEN EXTEND APPLICATION-FILE
           IF WS-APP-FILE-STATUS = "35"
               OPEN OUTPUT APPLICATION-FILE
           END-IF
           IF WS-APP-FILE-STATUS = "00"
               MOVE WS-CURRENT-USERNAME TO APP-REC-USERNAME
               MOVE WS-JOB-ID(WS-JOB-POS) TO APP-REC-JOB-ID
               WRITE APPLICATION-RECORD
               IF WS-APP-FILE-STATUS = "00"
                   MOVE 'Y' TO WS-APP-WRITE-OK
               END-IF
               CLOSE APPLICATION-FILE
           END-IF.
       END PROGRAM BROWSE-JOBS.
