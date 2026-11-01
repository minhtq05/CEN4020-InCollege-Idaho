      *>*******************************************************
      *> InCollege-Jobs.cob
      *> Job and internship menu: post a job (saved to
      *> InCollege-JobList.txt with a sequential ID and the
      *> poster's username), browse (under construction), and the
      *> saved-jobs submenu (SAVED-JOBS-MENU).
      *>*******************************************************
       >>SOURCE FORMAT FREE
       IDENTIFICATION DIVISION.
       PROGRAM-ID. JOB-MENU.

       ENVIRONMENT DIVISION.
       INPUT-OUTPUT SECTION.
       FILE-CONTROL.
           SELECT JOB-LIST-FILE ASSIGN TO "InCollege-JobList.txt"
               ORGANIZATION IS LINE SEQUENTIAL
               FILE STATUS IS WS-JOB-LIST-STATUS.
           SELECT APPLICATION-FILE ASSIGN TO "InCollege-Applications.txt"
               ORGANIZATION IS LINE SEQUENTIAL
               FILE STATUS IS WS-APP-FILE-STATUS.

       DATA DIVISION.
       FILE SECTION.
       FD  JOB-LIST-FILE.
       01  JOB-LIST-RECORD             PIC X(393).
       FD  APPLICATION-FILE.
       COPY "InCollege-ApplicationRecord.cpy".

       WORKING-STORAGE SECTION.
       COPY "InCollege-Common.cpy".
       COPY "InCollege-JobTable.cpy".

       01  WS-JOB-LIST-STATUS          PIC XX.
       01  WS-JOB-EOF                  PIC X.
       01  WS-JOB-LOAD-OK              PIC X.
       01  WS-JOB-POST-OK              PIC X.
       01  WS-JOB-NEXT-ID              PIC 9(3).
       01  WS-JOB-IDX                  PIC 99.
       01  WS-JOB-INPUT-LINE           PIC X(250).
       01  WS-JOB-INPUT-LENGTH         PIC 9(3).
       01  WS-JOB-ERROR                PIC X(100).
       01  WS-FIELD-NAME               PIC X(20).
       01  WS-FIELD-MAX                PIC 9(3).
       01  WS-FIELD-MAX-DISPLAY        PIC ZZ9.

       01  WS-CANDIDATE-JOB.
           05  WS-CANDIDATE-ID         PIC 9(3).
           05  WS-CANDIDATE-POSTER     PIC X(20).
           05  WS-CANDIDATE-TITLE      PIC X(50).
           05  WS-CANDIDATE-DESC       PIC X(200).
           05  WS-CANDIDATE-EMPLOYER   PIC X(50).
           05  WS-CANDIDATE-LOCATION   PIC X(50).
           05  WS-CANDIDATE-SALARY     PIC X(20).

       01  WS-MESSAGE                  PIC X(100).
       01  WS-LINE-TEXT                PIC X(100).
       01  WS-JOB-CHOICE               PIC X(20).
       01  WS-APP-FILE-STATUS          PIC XX.
       01  WS-APP-EOF                  PIC X.
       01  WS-APP-FILE-EXISTS          PIC X.
       01  WS-APPLICATION-IDX          PIC 9(3).
       01  WS-BROWSE-POS               PIC 99.
       01  WS-BROWSE-ID                PIC 9(3).
       01  WS-BROWSE-ID-TEXT           PIC X(3).
       01  WS-BROWSE-ID-LEN            PIC 9(3).
       01  WS-BROWSE-DONE              PIC X.
       01  WS-DETAIL-DONE              PIC X.
       01  WS-ALREADY-APPLIED          PIC X.
       01  WS-ID-DISPLAY               PIC ZZ9.
       01  WS-LONG-MESSAGE             PIC X(250).

       01  WS-JOB-MENU-DONE            PIC X VALUE 'N'.
           88  JOB-MENU-DONE                     VALUE 'Y'.

       PROCEDURE DIVISION.
       JOB-MENU-START.
           PERFORM JOB-LOAD.
           PERFORM APPLICATION-LOAD.
           MOVE 'N' TO WS-JOB-MENU-DONE.
           MOVE "--- Job Search/Internship Menu ---" TO WS-MESSAGE.
           CALL "WRITE-LINE" USING WS-MESSAGE.
           PERFORM UNTIL JOB-MENU-DONE OR END-OF-INPUT
               MOVE "1. Post a Job/Internship" TO WS-MESSAGE
               CALL "WRITE-LINE" USING WS-MESSAGE
               MOVE "2. Browse Jobs/Internships" TO WS-MESSAGE
               CALL "WRITE-LINE" USING WS-MESSAGE
               MOVE "3. Back to Main Menu" TO WS-MESSAGE
               CALL "WRITE-LINE" USING WS-MESSAGE
               MOVE "4. Saved Jobs" TO WS-MESSAGE
               CALL "WRITE-LINE" USING WS-MESSAGE
               MOVE "Enter your choice:" TO WS-MESSAGE
               CALL "WRITE-LINE" USING WS-MESSAGE
               CALL "READ-LINE" USING WS-LINE-TEXT
               IF NOT END-OF-INPUT
                   MOVE WS-LINE-TEXT TO WS-JOB-CHOICE
                   EVALUATE FUNCTION TRIM(WS-JOB-CHOICE)
                       WHEN "1"
                           PERFORM POST-JOB
                       WHEN "2"
                           PERFORM BROWSE-JOBS
                       WHEN "3"
                           MOVE 'Y' TO WS-JOB-MENU-DONE
                       WHEN "4"
                           CALL "SAVED-JOBS-MENU"
                       WHEN OTHER
                           MOVE "Invalid choice, please try again."
                               TO WS-MESSAGE
                           CALL "WRITE-LINE" USING WS-MESSAGE
                   END-EVALUATE
               END-IF
           END-PERFORM.
           GOBACK.

       BROWSE-JOBS SECTION.
       BROWSE-JOBS-START.
           MOVE 'N' TO WS-BROWSE-DONE.
           PERFORM UNTIL WS-BROWSE-DONE = 'Y' OR END-OF-INPUT
               MOVE "--- Available Jobs and Internships ---" TO WS-MESSAGE
               CALL "WRITE-LINE" USING WS-MESSAGE
               IF WS-JOB-COUNT = 0
                   MOVE "There are no jobs available." TO WS-MESSAGE
                   CALL "WRITE-LINE" USING WS-MESSAGE
                   MOVE 'Y' TO WS-BROWSE-DONE
               ELSE
                   PERFORM VARYING WS-BROWSE-POS FROM 1 BY 1
                           UNTIL WS-BROWSE-POS > WS-JOB-COUNT
                       MOVE WS-JOB-ID(WS-BROWSE-POS) TO WS-ID-DISPLAY
                       MOVE SPACES TO WS-LONG-MESSAGE
                       STRING FUNCTION TRIM(WS-ID-DISPLAY) DELIMITED BY SIZE
                              ". " DELIMITED BY SIZE
                              FUNCTION TRIM(WS-JOB-TITLE(WS-BROWSE-POS)) DELIMITED BY SIZE
                              " | " DELIMITED BY SIZE
                              FUNCTION TRIM(WS-JOB-EMPLOYER(WS-BROWSE-POS)) DELIMITED BY SIZE
                              " | " DELIMITED BY SIZE
                              FUNCTION TRIM(WS-JOB-LOCATION(WS-BROWSE-POS)) DELIMITED BY SIZE
                              INTO WS-LONG-MESSAGE
                       END-STRING
                       CALL "WRITE-LONG-LINE" USING WS-LONG-MESSAGE
                   END-PERFORM
                   MOVE "Enter a job ID to view, or 0 to go back:" TO WS-MESSAGE
                   CALL "WRITE-LINE" USING WS-MESSAGE
                   CALL "READ-LINE" USING WS-LINE-TEXT
                   IF NOT END-OF-INPUT
                       IF FUNCTION TRIM(WS-LINE-TEXT) = "0"
                           MOVE 'Y' TO WS-BROWSE-DONE
                       ELSE
                           PERFORM FIND-BROWSE-JOB
                           IF WS-BROWSE-POS = 0
                               MOVE "Invalid job ID." TO WS-MESSAGE
                               CALL "WRITE-LINE" USING WS-MESSAGE
                           ELSE
                               PERFORM VIEW-JOB-DETAILS
                           END-IF
                       END-IF
                   END-IF
               END-IF
           END-PERFORM
           GO TO BROWSE-JOBS-EXIT.

       FIND-BROWSE-JOB.
           MOVE 0 TO WS-BROWSE-POS WS-BROWSE-ID
           MOVE FUNCTION LENGTH(FUNCTION TRIM(WS-LINE-TEXT)) TO WS-BROWSE-ID-LEN
           IF WS-BROWSE-ID-LEN > 0 AND WS-BROWSE-ID-LEN <= 3
               MOVE FUNCTION TRIM(WS-LINE-TEXT) TO WS-BROWSE-ID-TEXT
               IF WS-BROWSE-ID-TEXT(1:WS-BROWSE-ID-LEN) IS NUMERIC
                   MOVE FUNCTION NUMVAL(WS-BROWSE-ID-TEXT(1:WS-BROWSE-ID-LEN))
                       TO WS-BROWSE-ID
                   PERFORM VARYING WS-JOB-IDX FROM 1 BY 1
                           UNTIL WS-JOB-IDX > WS-JOB-COUNT OR WS-BROWSE-POS > 0
                       IF WS-JOB-ID(WS-JOB-IDX) = WS-BROWSE-ID
                           MOVE WS-JOB-IDX TO WS-BROWSE-POS
                       END-IF
                   END-PERFORM
               END-IF
           END-IF.

       VIEW-JOB-DETAILS.
           MOVE "--- Job Details ---" TO WS-MESSAGE
           CALL "WRITE-LINE" USING WS-MESSAGE
           MOVE SPACES TO WS-LONG-MESSAGE
           STRING "Title: " DELIMITED BY SIZE
                  FUNCTION TRIM(WS-JOB-TITLE(WS-BROWSE-POS)) DELIMITED BY SIZE
                  INTO WS-LONG-MESSAGE
           CALL "WRITE-LONG-LINE" USING WS-LONG-MESSAGE
           MOVE SPACES TO WS-LONG-MESSAGE
           STRING "Employer: " DELIMITED BY SIZE
                  FUNCTION TRIM(WS-JOB-EMPLOYER(WS-BROWSE-POS)) DELIMITED BY SIZE
                  INTO WS-LONG-MESSAGE
           CALL "WRITE-LONG-LINE" USING WS-LONG-MESSAGE
           MOVE SPACES TO WS-LONG-MESSAGE
           STRING "Location: " DELIMITED BY SIZE
                  FUNCTION TRIM(WS-JOB-LOCATION(WS-BROWSE-POS)) DELIMITED BY SIZE
                  INTO WS-LONG-MESSAGE
           CALL "WRITE-LONG-LINE" USING WS-LONG-MESSAGE
           MOVE SPACES TO WS-LONG-MESSAGE
           STRING "Description: " DELIMITED BY SIZE
                  FUNCTION TRIM(WS-JOB-DESCRIPTION(WS-BROWSE-POS)) DELIMITED BY SIZE
                  INTO WS-LONG-MESSAGE
           CALL "WRITE-LONG-LINE" USING WS-LONG-MESSAGE
           IF FUNCTION TRIM(WS-JOB-SALARY(WS-BROWSE-POS)) NOT = "NONE"
              AND WS-JOB-SALARY(WS-BROWSE-POS) NOT = SPACES
               MOVE SPACES TO WS-LONG-MESSAGE
               STRING "Salary: " DELIMITED BY SIZE
                      FUNCTION TRIM(WS-JOB-SALARY(WS-BROWSE-POS)) DELIMITED BY SIZE
                      INTO WS-LONG-MESSAGE
               CALL "WRITE-LONG-LINE" USING WS-LONG-MESSAGE
           END-IF
           MOVE 'N' TO WS-DETAIL-DONE
           PERFORM UNTIL WS-DETAIL-DONE = 'Y' OR END-OF-INPUT
               MOVE "1. Apply for this job" TO WS-MESSAGE
               CALL "WRITE-LINE" USING WS-MESSAGE
               MOVE "2. Back to listings" TO WS-MESSAGE
               CALL "WRITE-LINE" USING WS-MESSAGE
               MOVE "Enter your choice:" TO WS-MESSAGE
               CALL "WRITE-LINE" USING WS-MESSAGE
               CALL "READ-LINE" USING WS-LINE-TEXT
               IF NOT END-OF-INPUT
                   EVALUATE FUNCTION TRIM(WS-LINE-TEXT)
                       WHEN "1"
                           PERFORM APPLY-FOR-JOB
                           MOVE 'Y' TO WS-DETAIL-DONE
                       WHEN "2" MOVE 'Y' TO WS-DETAIL-DONE
                       WHEN OTHER
                           MOVE "Invalid choice, please try again." TO WS-MESSAGE
                           CALL "WRITE-LINE" USING WS-MESSAGE
                   END-EVALUATE
               END-IF
           END-PERFORM.

       APPLY-FOR-JOB.
           MOVE 'N' TO WS-ALREADY-APPLIED
           PERFORM VARYING WS-APPLICATION-IDX FROM 1 BY 1
                   UNTIL WS-APPLICATION-IDX > WS-APPLICATION-COUNT
               IF WS-APPLICATION-USERNAME(WS-APPLICATION-IDX) = WS-CURRENT-USERNAME
                  AND WS-APPLICATION-JOB-ID(WS-APPLICATION-IDX) =
                      WS-JOB-ID(WS-BROWSE-POS)
                   MOVE 'Y' TO WS-ALREADY-APPLIED
               END-IF
           END-PERFORM
           IF WS-ALREADY-APPLIED = 'Y'
               MOVE "You have already applied for this job." TO WS-MESSAGE
               CALL "WRITE-LINE" USING WS-MESSAGE
           ELSE
               ADD 1 TO WS-APPLICATION-COUNT
               MOVE WS-CURRENT-USERNAME TO
                   WS-APPLICATION-USERNAME(WS-APPLICATION-COUNT)
               MOVE WS-JOB-ID(WS-BROWSE-POS) TO
                   WS-APPLICATION-JOB-ID(WS-APPLICATION-COUNT)
               IF WS-APP-FILE-EXISTS = 'Y'
                   OPEN EXTEND APPLICATION-FILE
               ELSE
                   OPEN OUTPUT APPLICATION-FILE
                   MOVE 'Y' TO WS-APP-FILE-EXISTS
               END-IF
               MOVE WS-CURRENT-USERNAME TO APP-REC-USERNAME
               MOVE WS-JOB-ID(WS-BROWSE-POS) TO APP-REC-JOB-ID
               WRITE APPLICATION-RECORD
               CLOSE APPLICATION-FILE
               MOVE SPACES TO WS-LONG-MESSAGE
               STRING "Your application for " DELIMITED BY SIZE
                      FUNCTION TRIM(WS-JOB-TITLE(WS-BROWSE-POS)) DELIMITED BY SIZE
                      " at " DELIMITED BY SIZE
                      FUNCTION TRIM(WS-JOB-EMPLOYER(WS-BROWSE-POS)) DELIMITED BY SIZE
                      " has been submitted." DELIMITED BY SIZE
                      INTO WS-LONG-MESSAGE
               CALL "WRITE-LONG-LINE" USING WS-LONG-MESSAGE
           END-IF.

       APPLICATION-LOAD.
           INITIALIZE WS-APPLICATION-TABLE
           MOVE 0 TO WS-APPLICATION-COUNT
           MOVE 'N' TO WS-APP-EOF
           MOVE 'N' TO WS-APP-FILE-EXISTS
           OPEN INPUT APPLICATION-FILE
           IF WS-APP-FILE-STATUS = "00"
               MOVE 'Y' TO WS-APP-FILE-EXISTS
               PERFORM UNTIL WS-APP-EOF = 'Y'
                   READ APPLICATION-FILE
                       AT END MOVE 'Y' TO WS-APP-EOF
                       NOT AT END
                           IF WS-APPLICATION-COUNT < 100
                               ADD 1 TO WS-APPLICATION-COUNT
                               MOVE APPLICATION-RECORD TO
                                   WS-APPLICATION-ENTRY(WS-APPLICATION-COUNT)
                           END-IF
                   END-READ
               END-PERFORM
               CLOSE APPLICATION-FILE
           END-IF.

       BROWSE-JOBS-EXIT.
           EXIT SECTION.

       JOB-LOAD SECTION.
       JOB-LOAD-START.
           INITIALIZE WS-JOB-TABLE.
           MOVE 0 TO WS-JOB-COUNT.
           MOVE 1 TO WS-JOB-NEXT-ID.
           MOVE 'Y' TO WS-JOB-LOAD-OK.
           MOVE 'N' TO WS-JOB-EOF.
           OPEN INPUT JOB-LIST-FILE.
           EVALUATE WS-JOB-LIST-STATUS
               WHEN "35"
                   CONTINUE
               WHEN "00"
                   PERFORM UNTIL WS-JOB-EOF = 'Y'
                       READ JOB-LIST-FILE
                           AT END
                               MOVE 'Y' TO WS-JOB-EOF
                           NOT AT END
                               IF WS-JOB-LIST-STATUS = "00"
                                   IF WS-JOB-COUNT < 20
                                       ADD 1 TO WS-JOB-COUNT
                                       MOVE JOB-LIST-RECORD
                                           TO WS-JOB-ENTRY(WS-JOB-COUNT)
                                   ELSE
                                       MOVE 'N' TO WS-JOB-LOAD-OK
                                       MOVE 'Y' TO WS-JOB-EOF
                                   END-IF
                               ELSE
                                   MOVE 'N' TO WS-JOB-LOAD-OK
                                   MOVE 'Y' TO WS-JOB-EOF
                               END-IF
                       END-READ
                   END-PERFORM
                   CLOSE JOB-LIST-FILE
                   IF WS-JOB-LIST-STATUS NOT = "00"
                       MOVE 'N' TO WS-JOB-LOAD-OK
                   END-IF
               WHEN OTHER
                   MOVE 'N' TO WS-JOB-LOAD-OK
           END-EVALUATE.

      *>   Next ID is one past the highest ID on file; a record
      *>   without a numeric ID means the file is not in this format.
           PERFORM VARYING WS-JOB-IDX FROM 1 BY 1
                   UNTIL WS-JOB-IDX > WS-JOB-COUNT
               IF WS-JOB-ID(WS-JOB-IDX) IS NUMERIC
                   IF WS-JOB-ID(WS-JOB-IDX) >= WS-JOB-NEXT-ID
                       COMPUTE WS-JOB-NEXT-ID = WS-JOB-ID(WS-JOB-IDX) + 1
                   END-IF
               ELSE
                   MOVE 'N' TO WS-JOB-LOAD-OK
               END-IF
           END-PERFORM.

       POST-JOB SECTION.
       POST-JOB-START.
           MOVE 'Y' TO WS-JOB-POST-OK.
           MOVE SPACES TO WS-JOB-ERROR.
           INITIALIZE WS-CANDIDATE-JOB.
           MOVE "--- Post a New Job/Internship ---" TO WS-MESSAGE.
           CALL "WRITE-LINE" USING WS-MESSAGE.

           EVALUATE TRUE
               WHEN WS-JOB-LOAD-OK NOT = 'Y'
                   MOVE 'N' TO WS-JOB-POST-OK
                   MOVE "Unable to load job listings. Job not posted."
                       TO WS-JOB-ERROR
               WHEN WS-JOB-COUNT >= 20
                   MOVE 'N' TO WS-JOB-POST-OK
                   MOVE "The job list is full (20 jobs). Job not posted."
                       TO WS-JOB-ERROR
           END-EVALUATE.

           IF WS-JOB-POST-OK = 'Y'
               MOVE "Enter Job Title:" TO WS-MESSAGE
               MOVE "Job Title" TO WS-FIELD-NAME
               MOVE 50 TO WS-FIELD-MAX
               PERFORM READ-REQUIRED-FIELD
               IF WS-JOB-POST-OK = 'Y'
                   MOVE FUNCTION TRIM(WS-JOB-INPUT-LINE)
                       TO WS-CANDIDATE-TITLE
               END-IF
           END-IF.

           IF WS-JOB-POST-OK = 'Y'
               MOVE "Enter Description (max 200 chars):" TO WS-MESSAGE
               MOVE "Description" TO WS-FIELD-NAME
               MOVE 200 TO WS-FIELD-MAX
               PERFORM READ-REQUIRED-FIELD
               IF WS-JOB-POST-OK = 'Y'
                   MOVE FUNCTION TRIM(WS-JOB-INPUT-LINE)
                       TO WS-CANDIDATE-DESC
               END-IF
           END-IF.

           IF WS-JOB-POST-OK = 'Y'
               MOVE "Enter Employer Name:" TO WS-MESSAGE
               MOVE "Employer Name" TO WS-FIELD-NAME
               MOVE 50 TO WS-FIELD-MAX
               PERFORM READ-REQUIRED-FIELD
               IF WS-JOB-POST-OK = 'Y'
                   MOVE FUNCTION TRIM(WS-JOB-INPUT-LINE)
                       TO WS-CANDIDATE-EMPLOYER
               END-IF
           END-IF.

           IF WS-JOB-POST-OK = 'Y'
               MOVE "Enter Location:" TO WS-MESSAGE
               MOVE "Location" TO WS-FIELD-NAME
               MOVE 50 TO WS-FIELD-MAX
               PERFORM READ-REQUIRED-FIELD
               IF WS-JOB-POST-OK = 'Y'
                   MOVE FUNCTION TRIM(WS-JOB-INPUT-LINE)
                       TO WS-CANDIDATE-LOCATION
               END-IF
           END-IF.

      *>   Salary is free text (e.g. "$50,000/year"); blank or NONE
      *>   means no salary was given.
           IF WS-JOB-POST-OK = 'Y'
               MOVE "Enter Salary (optional, enter 'NONE' to skip):"
                   TO WS-MESSAGE
               PERFORM READ-JOB-FIELD
               IF WS-JOB-POST-OK = 'Y'
                   EVALUATE TRUE
                       WHEN WS-JOB-INPUT-LINE = SPACES
                           MOVE "NONE" TO WS-CANDIDATE-SALARY
                       WHEN FUNCTION LENGTH(
                           FUNCTION TRIM(WS-JOB-INPUT-LINE)) > 20
                           MOVE 'N' TO WS-JOB-POST-OK
                           MOVE "Salary must be 20 characters or fewer. Job not posted."
                               TO WS-JOB-ERROR
                       WHEN FUNCTION UPPER-CASE(
                           FUNCTION TRIM(WS-JOB-INPUT-LINE)) = "NONE"
                           MOVE "NONE" TO WS-CANDIDATE-SALARY
                       WHEN OTHER
                           MOVE FUNCTION TRIM(WS-JOB-INPUT-LINE)
                               TO WS-CANDIDATE-SALARY
                   END-EVALUATE
               END-IF
           END-IF.

           IF WS-JOB-POST-OK = 'Y'
               PERFORM SAVE-JOB
           END-IF.
           IF WS-JOB-POST-OK = 'Y'
               MOVE "Job posted successfully!" TO WS-MESSAGE
               CALL "WRITE-LINE" USING WS-MESSAGE
               MOVE "----------------------------------" TO WS-MESSAGE
               CALL "WRITE-LINE" USING WS-MESSAGE
           ELSE
               CALL "WRITE-LINE" USING WS-JOB-ERROR
           END-IF.

       READ-JOB-INPUT SECTION.
       READ-JOB-FIELD.
           CALL "WRITE-LINE" USING WS-MESSAGE
           MOVE SPACES TO WS-JOB-INPUT-LINE
           CALL "READ-LONG-LINE" USING WS-JOB-INPUT-LINE
           IF END-OF-INPUT
               MOVE 'N' TO WS-JOB-POST-OK
               MOVE "Job not posted." TO WS-JOB-ERROR
           END-IF.

      *>   Prompt with WS-MESSAGE and reject a blank answer or one
      *>   longer than WS-FIELD-MAX, naming WS-FIELD-NAME in the error.
       READ-REQUIRED-FIELD.
           PERFORM READ-JOB-FIELD
           IF WS-JOB-POST-OK = 'Y'
               IF WS-JOB-INPUT-LINE = SPACES
                   MOVE 'N' TO WS-JOB-POST-OK
                   STRING FUNCTION TRIM(WS-FIELD-NAME) DELIMITED BY SIZE
                          " is required. Job not posted." DELIMITED BY SIZE
                          INTO WS-JOB-ERROR
               ELSE
                   MOVE FUNCTION LENGTH(FUNCTION TRIM(WS-JOB-INPUT-LINE))
                       TO WS-JOB-INPUT-LENGTH
                   IF WS-JOB-INPUT-LENGTH > WS-FIELD-MAX
                       MOVE 'N' TO WS-JOB-POST-OK
                       MOVE WS-FIELD-MAX TO WS-FIELD-MAX-DISPLAY
                       STRING FUNCTION TRIM(WS-FIELD-NAME) DELIMITED BY SIZE
                              " must be " DELIMITED BY SIZE
                              FUNCTION TRIM(WS-FIELD-MAX-DISPLAY)
                                  DELIMITED BY SIZE
                              " characters or fewer. Job not posted."
                                  DELIMITED BY SIZE
                              INTO WS-JOB-ERROR
                   END-IF
               END-IF
           END-IF.

       JOB-SAVE SECTION.
       SAVE-JOB.
           MOVE WS-JOB-NEXT-ID TO WS-CANDIDATE-ID
           MOVE WS-CURRENT-USERNAME TO WS-CANDIDATE-POSTER
           OPEN EXTEND JOB-LIST-FILE
           IF WS-JOB-LIST-STATUS = "35"
               OPEN OUTPUT JOB-LIST-FILE
           END-IF
           IF WS-JOB-LIST-STATUS = "00"
               MOVE WS-CANDIDATE-JOB TO JOB-LIST-RECORD
               WRITE JOB-LIST-RECORD
               IF WS-JOB-LIST-STATUS = "00"
                   CLOSE JOB-LIST-FILE
                   IF WS-JOB-LIST-STATUS = "00"
                       ADD 1 TO WS-JOB-COUNT
                       MOVE WS-CANDIDATE-JOB
                           TO WS-JOB-ENTRY(WS-JOB-COUNT)
                       ADD 1 TO WS-JOB-NEXT-ID
                   ELSE
                       MOVE 'N' TO WS-JOB-POST-OK
                   END-IF
               ELSE
                   MOVE 'N' TO WS-JOB-POST-OK
                   CLOSE JOB-LIST-FILE
               END-IF
           ELSE
               MOVE 'N' TO WS-JOB-POST-OK
           END-IF
           IF WS-JOB-POST-OK = 'N'
               MOVE "Unable to save job. Job not posted." TO WS-JOB-ERROR
           END-IF.
       END PROGRAM JOB-MENU.
