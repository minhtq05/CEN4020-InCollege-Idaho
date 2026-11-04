      *>*******************************************************
      *> InCollege-ViewApplications.cob
      *> VIEW-APPLICATIONS: the Job Application Summary Report.
      *> Reads InCollege-Applications.txt, keeps the logged-in
      *> user's applications, and lists each job's ID, title,
      *> employer, and location, followed by the total count.
      *> Applications for a job no longer on file are skipped.
      *> Called from JOB-MENU, which has already loaded the shared
      *> job table (InCollege-JobTable.cpy).
      *>*******************************************************
       >>SOURCE FORMAT FREE
       IDENTIFICATION DIVISION.
       PROGRAM-ID. VIEW-APPLICATIONS.

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
       01  WS-MY-APP-COUNT             PIC 9(3).

       01  WS-JOB-IDX                  PIC 99.
       01  WS-JOB-POS                  PIC 99.
       01  WS-COUNT-DISPLAY            PIC ZZ9.

       01  WS-FIELD-LABEL              PIC X(30).
       01  WS-FIELD-VALUE              PIC X(200).

       01  WS-MESSAGE                  PIC X(100).
       01  WS-LONG-MESSAGE             PIC X(250).

       PROCEDURE DIVISION.
       VIEW-APPLICATIONS-START.
           MOVE 0 TO WS-MY-APP-COUNT.
           MOVE "--- Your Job Applications ---" TO WS-MESSAGE.
           CALL "WRITE-LINE" USING WS-MESSAGE.
           MOVE "Application Summary for" TO WS-FIELD-LABEL.
           MOVE WS-CURRENT-USERNAME TO WS-FIELD-VALUE.
           PERFORM WRITE-FIELD.
           MOVE "------------------------------" TO WS-MESSAGE.
           CALL "WRITE-LINE" USING WS-MESSAGE.

           MOVE 'N' TO WS-APP-EOF.
           OPEN INPUT APPLICATION-FILE.
           IF WS-APP-FILE-STATUS = "00"
               PERFORM UNTIL WS-APP-EOF = 'Y'
                   READ APPLICATION-FILE
                       AT END
                           MOVE 'Y' TO WS-APP-EOF
                       NOT AT END
                           IF APP-REC-USERNAME = WS-CURRENT-USERNAME
                              AND APP-REC-JOB-ID IS NUMERIC
                               PERFORM FIND-JOB
                               IF WS-JOB-POS > 0
                                   PERFORM WRITE-APPLICATION
                               END-IF
                           END-IF
                   END-READ
               END-PERFORM
               CLOSE APPLICATION-FILE
           END-IF.

           IF WS-MY-APP-COUNT = 0
               MOVE "You have not applied for any jobs yet." TO WS-MESSAGE
               CALL "WRITE-LINE" USING WS-MESSAGE
           END-IF.
           MOVE "------------------------------" TO WS-MESSAGE.
           CALL "WRITE-LINE" USING WS-MESSAGE.
           MOVE WS-MY-APP-COUNT TO WS-COUNT-DISPLAY.
           MOVE "Total Applications:" TO WS-FIELD-LABEL.
           MOVE WS-COUNT-DISPLAY TO WS-FIELD-VALUE.
           PERFORM WRITE-FIELD.
           MOVE "------------------------------" TO WS-MESSAGE.
           CALL "WRITE-LINE" USING WS-MESSAGE.
           GOBACK.

       REPORT-HELPERS SECTION.
      *>   One report entry for job WS-JOB-POS, with a "---"
      *>   separator before every entry but the first.
       WRITE-APPLICATION.
           IF WS-MY-APP-COUNT > 0
               MOVE "---" TO WS-MESSAGE
               CALL "WRITE-LINE" USING WS-MESSAGE
           END-IF
           ADD 1 TO WS-MY-APP-COUNT
           MOVE WS-JOB-ID(WS-JOB-POS) TO WS-COUNT-DISPLAY
           MOVE "Job ID:" TO WS-FIELD-LABEL
           MOVE WS-COUNT-DISPLAY TO WS-FIELD-VALUE
           PERFORM WRITE-FIELD
           MOVE "Job Title:" TO WS-FIELD-LABEL
           MOVE WS-JOB-TITLE(WS-JOB-POS) TO WS-FIELD-VALUE
           PERFORM WRITE-FIELD
           MOVE "Employer:" TO WS-FIELD-LABEL
           MOVE WS-JOB-EMPLOYER(WS-JOB-POS) TO WS-FIELD-VALUE
           PERFORM WRITE-FIELD
           MOVE "Location:" TO WS-FIELD-LABEL
           MOVE WS-JOB-LOCATION(WS-JOB-POS) TO WS-FIELD-VALUE
           PERFORM WRITE-FIELD.

      *>   "Label Value" line from WS-FIELD-LABEL and WS-FIELD-VALUE.
       WRITE-FIELD.
           MOVE SPACES TO WS-LONG-MESSAGE
           STRING FUNCTION TRIM(WS-FIELD-LABEL) DELIMITED BY SIZE
                  " " DELIMITED BY SIZE
                  FUNCTION TRIM(WS-FIELD-VALUE) DELIMITED BY SIZE
                  INTO WS-LONG-MESSAGE
           CALL "WRITE-LONG-LINE" USING WS-LONG-MESSAGE.

      *>   Position of job APP-REC-JOB-ID in the job table, or 0.
       FIND-JOB.
           MOVE 0 TO WS-JOB-POS
           PERFORM VARYING WS-JOB-IDX FROM 1 BY 1
                   UNTIL WS-JOB-IDX > WS-JOB-COUNT OR WS-JOB-POS > 0
               IF WS-JOB-ID(WS-JOB-IDX) = APP-REC-JOB-ID
                   MOVE WS-JOB-IDX TO WS-JOB-POS
               END-IF
           END-PERFORM.
       END PROGRAM VIEW-APPLICATIONS.
