      *>*******************************************************
      *> InCollege-Jobs.cob
      *> Job and internship menu.
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
       01  JOB-LIST-RECORD             PIC X(370).

       WORKING-STORAGE SECTION.
       COPY "InCollege-Common.cpy".
       COPY "InCollege-JobTable.cpy".

       01  WS-JOB-LIST-STATUS          PIC XX.
       01  WS-JOB-EOF                  PIC X.
       01  WS-JOB-LOAD-OK              PIC X.
       01  WS-JOB-POST-OK              PIC X.
       01  WS-JOB-INPUT-LINE           PIC X(100).
       01  WS-JOB-INPUT-LENGTH         PIC 9(3).

       01  WS-CANDIDATE-JOB.
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
           PERFORM UNTIL JOB-MENU-DONE OR END-OF-INPUT
               MOVE "1. Post a Job/Internship" TO WS-MESSAGE
               CALL "WRITE-LINE" USING WS-MESSAGE
               MOVE "2. Browse Jobs/Internships" TO WS-MESSAGE
               CALL "WRITE-LINE" USING WS-MESSAGE
               MOVE "3. Back to Main Menu" TO WS-MESSAGE
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
                           MOVE "This option is under construction."
                               TO WS-MESSAGE
                           CALL "WRITE-LINE" USING WS-MESSAGE
                       WHEN "3"
                           MOVE 'Y' TO WS-JOB-MENU-DONE
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

       POST-JOB SECTION.
       POST-JOB-START.
           MOVE 'Y' TO WS-JOB-POST-OK.
           INITIALIZE WS-CANDIDATE-JOB.

           IF WS-JOB-LOAD-OK NOT = 'Y' OR WS-JOB-COUNT >= 20
               MOVE 'N' TO WS-JOB-POST-OK
           ELSE
               MOVE "Enter Job Title:" TO WS-MESSAGE
               PERFORM READ-JOB-FIELD
               IF WS-JOB-POST-OK = 'Y'
                   MOVE FUNCTION LENGTH(
                       FUNCTION TRIM(WS-JOB-INPUT-LINE))
                       TO WS-JOB-INPUT-LENGTH
                   IF WS-JOB-INPUT-LENGTH = 0 OR
                       WS-JOB-INPUT-LENGTH > 50
                       MOVE 'N' TO WS-JOB-POST-OK
                   ELSE
                       MOVE FUNCTION TRIM(WS-JOB-INPUT-LINE)
                           TO WS-CANDIDATE-TITLE
                   END-IF
               END-IF

               IF WS-JOB-POST-OK = 'Y'
                   MOVE "Enter Description (max 200 chars):"
                       TO WS-MESSAGE
                   PERFORM READ-JOB-FIELD
                   IF WS-JOB-POST-OK = 'Y'
                       MOVE FUNCTION LENGTH(
                           FUNCTION TRIM(WS-JOB-INPUT-LINE))
                           TO WS-JOB-INPUT-LENGTH
                       IF WS-JOB-INPUT-LENGTH > 200
                           MOVE 'N' TO WS-JOB-POST-OK
                       ELSE
                           MOVE FUNCTION TRIM(WS-JOB-INPUT-LINE)
                               TO WS-CANDIDATE-DESC
                       END-IF
                   END-IF
               END-IF

               IF WS-JOB-POST-OK = 'Y'
                   MOVE "Enter Employer Name:" TO WS-MESSAGE
                   PERFORM READ-JOB-FIELD
                   IF WS-JOB-POST-OK = 'Y'
                       MOVE FUNCTION LENGTH(
                           FUNCTION TRIM(WS-JOB-INPUT-LINE))
                           TO WS-JOB-INPUT-LENGTH
                       IF WS-JOB-INPUT-LENGTH = 0 OR
                           WS-JOB-INPUT-LENGTH > 50
                           MOVE 'N' TO WS-JOB-POST-OK
                       ELSE
                           MOVE FUNCTION TRIM(WS-JOB-INPUT-LINE)
                               TO WS-CANDIDATE-EMPLOYER
                       END-IF
                   END-IF
               END-IF

               IF WS-JOB-POST-OK = 'Y'
                   MOVE "Enter Location:" TO WS-MESSAGE
                   PERFORM READ-JOB-FIELD
                   IF WS-JOB-POST-OK = 'Y'
                       MOVE FUNCTION LENGTH(
                           FUNCTION TRIM(WS-JOB-INPUT-LINE))
                           TO WS-JOB-INPUT-LENGTH
                       IF WS-JOB-INPUT-LENGTH = 0 OR
                           WS-JOB-INPUT-LENGTH > 50
                           MOVE 'N' TO WS-JOB-POST-OK
                       ELSE
                           MOVE FUNCTION TRIM(WS-JOB-INPUT-LINE)
                               TO WS-CANDIDATE-LOCATION
                       END-IF
                   END-IF
               END-IF

               IF WS-JOB-POST-OK = 'Y'
                   MOVE "Enter Salary (optional, enter 'NONE' to skip):"
                       TO WS-MESSAGE
                   PERFORM READ-JOB-FIELD
                   IF WS-JOB-POST-OK = 'Y'
                       MOVE FUNCTION LENGTH(
                           FUNCTION TRIM(WS-JOB-INPUT-LINE))
                           TO WS-JOB-INPUT-LENGTH
                       EVALUATE TRUE
                           WHEN WS-JOB-INPUT-LENGTH = 0
                               MOVE "NONE" TO WS-CANDIDATE-SALARY
                           WHEN WS-JOB-INPUT-LENGTH > 20
                               MOVE 'N' TO WS-JOB-POST-OK
                           WHEN FUNCTION UPPER-CASE(
                               FUNCTION TRIM(WS-JOB-INPUT-LINE))
                               = "NONE"
                               MOVE "NONE" TO WS-CANDIDATE-SALARY
                           WHEN FUNCTION TEST-NUMVAL(
                               FUNCTION TRIM(WS-JOB-INPUT-LINE)) = 0
                               MOVE FUNCTION TRIM(WS-JOB-INPUT-LINE)
                                   TO WS-CANDIDATE-SALARY
                           WHEN OTHER
                               MOVE 'N' TO WS-JOB-POST-OK
                       END-EVALUATE
                   END-IF
               END-IF
           END-IF

           IF WS-JOB-POST-OK = 'Y'
               PERFORM SAVE-JOB
           END-IF
           IF WS-JOB-POST-OK = 'Y'
               MOVE "Job posted successfully!" TO WS-MESSAGE
           ELSE
               MOVE "Failed to post job." TO WS-MESSAGE
           END-IF
           CALL "WRITE-LINE" USING WS-MESSAGE.

       READ-JOB-INPUT SECTION.
       READ-JOB-FIELD.
           CALL "WRITE-LINE" USING WS-MESSAGE
           CALL "READ-LINE" USING WS-JOB-INPUT-LINE
           IF END-OF-INPUT
               MOVE 'N' TO WS-JOB-POST-OK
           END-IF.

       JOB-SAVE SECTION.
       SAVE-JOB.
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
                   ELSE
                       MOVE 'N' TO WS-JOB-POST-OK
                   END-IF
               ELSE
                   MOVE 'N' TO WS-JOB-POST-OK
                   CLOSE JOB-LIST-FILE
               END-IF
           ELSE
               MOVE 'N' TO WS-JOB-POST-OK
           END-IF.
       END PROGRAM JOB-MENU.
