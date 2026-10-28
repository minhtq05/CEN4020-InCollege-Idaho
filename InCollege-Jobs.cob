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

       DATA DIVISION.
       FILE SECTION.
       FD  JOB-LIST-FILE.
       01  JOB-LIST-RECORD             PIC X(393).

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

       01  WS-JOB-MENU-DONE            PIC X VALUE 'N'.
           88  JOB-MENU-DONE                     VALUE 'Y'.

       PROCEDURE DIVISION.
       JOB-MENU-START.
           PERFORM JOB-LOAD.
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
                           MOVE "Browse Jobs/Internships is under construction."
                               TO WS-MESSAGE
                           CALL "WRITE-LINE" USING WS-MESSAGE
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
