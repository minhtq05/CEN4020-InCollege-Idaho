       >>SOURCE FORMAT FREE
       IDENTIFICATION DIVISION.
       PROGRAM-ID. JOB-LOAD.
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
       COPY "InCollege-JobTable.cpy".
       01  WS-APP-FILE-STATUS         PIC X(2).
       01  WS-APP-EOF                 PIC X VALUE 'N'.
       PROCEDURE DIVISION.
       JOB-LOAD-START.
           INITIALIZE WS-JOB-TABLE WS-APPLICATION-TABLE.
           MOVE 3 TO WS-JOB-COUNT.
           MOVE "JOB-001" TO WS-JOB-ID(1).
           MOVE "Software Engineer Intern" TO WS-JOB-TITLE(1).
           MOVE "Idaho Technology Group" TO WS-JOB-EMPLOYER(1).
           MOVE "Boise, Idaho" TO WS-JOB-LOCATION(1).
           MOVE "Build and test software with an experienced engineering team."
               TO WS-JOB-DESCRIPTION(1).
           MOVE "$22 per hour" TO WS-JOB-SALARY(1).
           MOVE "JOB-002" TO WS-JOB-ID(2).
           MOVE "Junior Data Analyst" TO WS-JOB-TITLE(2).
           MOVE "Gem State Analytics" TO WS-JOB-EMPLOYER(2).
           MOVE "Moscow, Idaho" TO WS-JOB-LOCATION(2).
           MOVE "Analyze business data and prepare dashboards and reports."
               TO WS-JOB-DESCRIPTION(2).
           MOVE "$55,000 per year" TO WS-JOB-SALARY(2).
           MOVE "JOB-003" TO WS-JOB-ID(3).
           MOVE "Marketing Internship" TO WS-JOB-TITLE(3).
           MOVE "Sawtooth Creative" TO WS-JOB-EMPLOYER(3).
           MOVE "Remote" TO WS-JOB-LOCATION(3).
           MOVE "Support campaigns, social media, and market research."
               TO WS-JOB-DESCRIPTION(3).
           MOVE SPACES TO WS-JOB-SALARY(3).
           MOVE 0 TO WS-APPLICATION-COUNT.
           MOVE 'N' TO WS-APP-EOF.
           OPEN INPUT APPLICATION-FILE.
           IF WS-APP-FILE-STATUS = "00"
               PERFORM UNTIL WS-APP-EOF = 'Y'
                   READ APPLICATION-FILE
                       AT END MOVE 'Y' TO WS-APP-EOF
                       NOT AT END
                           IF WS-APPLICATION-COUNT < 100
                               ADD 1 TO WS-APPLICATION-COUNT
                               MOVE APP-REC-USERNAME TO
                                   WS-APPLICATION-USERNAME(WS-APPLICATION-COUNT)
                               MOVE APP-REC-JOB-ID TO
                                   WS-APPLICATION-JOB-ID(WS-APPLICATION-COUNT)
                           END-IF
                   END-READ
               END-PERFORM
               CLOSE APPLICATION-FILE
           END-IF.
           GOBACK.
       END PROGRAM JOB-LOAD.

       IDENTIFICATION DIVISION.
       PROGRAM-ID. JOB-MENU.
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
       01  WS-APP-FILE-STATUS         PIC X(2).
       01  WS-MESSAGE                 PIC X(100).
       01  WS-LINE-TEXT               PIC X(100).
       01  WS-CHOICE                  PIC X(20).
       01  WS-IDX                     PIC 9(3).
       01  WS-SELECTED                PIC 9(2).
       01  WS-MENU-DONE               PIC X VALUE 'N'.
       01  WS-DETAIL-DONE             PIC X VALUE 'N'.
       01  WS-ALREADY-APPLIED         PIC X VALUE 'N'.
       PROCEDURE DIVISION.
       JOB-MENU-START.
           MOVE 'N' TO WS-MENU-DONE.
           PERFORM UNTIL WS-MENU-DONE = 'Y' OR END-OF-INPUT
               PERFORM DISPLAY-LISTINGS
               MOVE "Select a listing number, or enter 0 to go back:"
                   TO WS-MESSAGE
               CALL "WRITE-LINE" USING WS-MESSAGE
               CALL "READ-LINE" USING WS-LINE-TEXT
               IF NOT END-OF-INPUT
                   MOVE FUNCTION TRIM(WS-LINE-TEXT) TO WS-CHOICE
                   IF FUNCTION TRIM(WS-CHOICE) = "0"
                       MOVE 'Y' TO WS-MENU-DONE
                   ELSE
                       MOVE 0 TO WS-SELECTED
                       IF FUNCTION TEST-NUMVAL(WS-CHOICE) = 0
                           PERFORM VARYING WS-IDX FROM 1 BY 1
                                   UNTIL WS-IDX > WS-JOB-COUNT
                               IF FUNCTION NUMVAL(WS-CHOICE) = WS-IDX
                                   MOVE WS-IDX TO WS-SELECTED
                               END-IF
                           END-PERFORM
                       END-IF
                       IF WS-SELECTED = 0
                           MOVE "Invalid choice, please try again."
                               TO WS-MESSAGE
                           CALL "WRITE-LINE" USING WS-MESSAGE
                       ELSE
                           PERFORM VIEW-DETAILS
                       END-IF
                   END-IF
               END-IF
           END-PERFORM.
           GOBACK.
       DISPLAY-LISTINGS SECTION.
           MOVE "--- Available Jobs and Internships ---" TO WS-MESSAGE.
           CALL "WRITE-LINE" USING WS-MESSAGE.
           PERFORM VARYING WS-IDX FROM 1 BY 1 UNTIL WS-IDX > WS-JOB-COUNT
               MOVE SPACES TO WS-MESSAGE
               STRING WS-IDX DELIMITED BY SIZE ". " DELIMITED BY SIZE
                      FUNCTION TRIM(WS-JOB-TITLE(WS-IDX)) DELIMITED BY SIZE
                      " | " DELIMITED BY SIZE
                      FUNCTION TRIM(WS-JOB-EMPLOYER(WS-IDX)) DELIMITED BY SIZE
                      " | " DELIMITED BY SIZE
                      FUNCTION TRIM(WS-JOB-LOCATION(WS-IDX)) DELIMITED BY SIZE
                      INTO WS-MESSAGE
               END-STRING
               CALL "WRITE-LINE" USING WS-MESSAGE
           END-PERFORM.
       VIEW-DETAILS SECTION.
           MOVE "--- Job Details ---" TO WS-MESSAGE.
           CALL "WRITE-LINE" USING WS-MESSAGE.
           MOVE SPACES TO WS-MESSAGE.
           STRING "Title: " DELIMITED BY SIZE
                  FUNCTION TRIM(WS-JOB-TITLE(WS-SELECTED)) DELIMITED BY SIZE
                  INTO WS-MESSAGE.
           CALL "WRITE-LINE" USING WS-MESSAGE.
           MOVE SPACES TO WS-MESSAGE.
           STRING "Employer: " DELIMITED BY SIZE
                  FUNCTION TRIM(WS-JOB-EMPLOYER(WS-SELECTED)) DELIMITED BY SIZE
                  INTO WS-MESSAGE.
           CALL "WRITE-LINE" USING WS-MESSAGE.
           MOVE SPACES TO WS-MESSAGE.
           STRING "Location: " DELIMITED BY SIZE
                  FUNCTION TRIM(WS-JOB-LOCATION(WS-SELECTED)) DELIMITED BY SIZE
                  INTO WS-MESSAGE.
           CALL "WRITE-LINE" USING WS-MESSAGE.
           MOVE SPACES TO WS-MESSAGE.
           STRING "Description: " DELIMITED BY SIZE
                  FUNCTION TRIM(WS-JOB-DESCRIPTION(WS-SELECTED)) DELIMITED BY SIZE
                  INTO WS-MESSAGE.
           CALL "WRITE-LINE" USING WS-MESSAGE.
           IF FUNCTION TRIM(WS-JOB-SALARY(WS-SELECTED)) NOT = SPACES
               MOVE SPACES TO WS-MESSAGE
               STRING "Salary: " DELIMITED BY SIZE
                      FUNCTION TRIM(WS-JOB-SALARY(WS-SELECTED)) DELIMITED BY SIZE
                      INTO WS-MESSAGE
               END-STRING
               CALL "WRITE-LINE" USING WS-MESSAGE
           END-IF.
           MOVE 'N' TO WS-DETAIL-DONE.
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
       APPLY-FOR-JOB SECTION.
           MOVE 'N' TO WS-ALREADY-APPLIED.
           PERFORM VARYING WS-IDX FROM 1 BY 1
                   UNTIL WS-IDX > WS-APPLICATION-COUNT
               IF FUNCTION TRIM(WS-APPLICATION-USERNAME(WS-IDX)) =
                  FUNCTION TRIM(WS-CURRENT-USERNAME)
                  AND FUNCTION TRIM(WS-APPLICATION-JOB-ID(WS-IDX)) =
                      FUNCTION TRIM(WS-JOB-ID(WS-SELECTED))
                   MOVE 'Y' TO WS-ALREADY-APPLIED
               END-IF
           END-PERFORM.
           IF WS-ALREADY-APPLIED = 'Y'
               MOVE "You have already applied for this job." TO WS-MESSAGE
               CALL "WRITE-LINE" USING WS-MESSAGE
           ELSE
               ADD 1 TO WS-APPLICATION-COUNT
               MOVE WS-CURRENT-USERNAME TO WS-APPLICATION-USERNAME(WS-APPLICATION-COUNT)
               MOVE WS-JOB-ID(WS-SELECTED) TO WS-APPLICATION-JOB-ID(WS-APPLICATION-COUNT)
               OPEN EXTEND APPLICATION-FILE
               IF WS-APP-FILE-STATUS = "35" OPEN OUTPUT APPLICATION-FILE END-IF
               MOVE WS-CURRENT-USERNAME TO APP-REC-USERNAME
               MOVE WS-JOB-ID(WS-SELECTED) TO APP-REC-JOB-ID
               WRITE APPLICATION-RECORD
               CLOSE APPLICATION-FILE
               MOVE SPACES TO WS-MESSAGE
               STRING "Your application for " DELIMITED BY SIZE
                      FUNCTION TRIM(WS-JOB-TITLE(WS-SELECTED)) DELIMITED BY SIZE
                      " at " DELIMITED BY SIZE
                      FUNCTION TRIM(WS-JOB-EMPLOYER(WS-SELECTED)) DELIMITED BY SIZE
                      " has been submitted." DELIMITED BY SIZE
                      INTO WS-MESSAGE
               END-STRING
               CALL "WRITE-LINE" USING WS-MESSAGE
           END-IF.
       END PROGRAM JOB-MENU.
