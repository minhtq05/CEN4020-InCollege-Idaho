      *>*******************************************************
      *> InCollege - main program.
      *> Top-level and post-login menu orchestration only; every
      *> feature is delegated to a subprogram (InCollege-IO,
      *> InCollege-Account, InCollege-Profile,
      *> InCollege-ProfileEntries, InCollege-ProfileView,
      *> InCollege-Skills), sharing session state via the EXTERNAL
      *> items in InCollege-Common.cpy. See README.md for the
      *> module layout and build instructions (make/make run).
      *>*******************************************************
       >>SOURCE FORMAT FREE
       IDENTIFICATION DIVISION.
       PROGRAM-ID. INCOLLEGE.

       DATA DIVISION.
       WORKING-STORAGE SECTION.
       COPY "InCollege-Common.cpy".

       01  WS-MESSAGE                  PIC X(100).
       01  WS-LINE-TEXT                PIC X(100).
       01  WS-TOP-CHOICE               PIC X(20).
       01  WS-POST-CHOICE              PIC X(20).

       01  WS-LOGIN-OK                 PIC X VALUE 'N'.

       01  WS-LOGOUT-FLAG              PIC X VALUE 'N'.
           88  LOGOUT-REQUESTED                  VALUE 'Y'.

       PROCEDURE DIVISION.
       MAIN-START.
           MOVE 'N' TO WS-EOF-INPUT.
           MOVE 'N' TO WS-PROGRAM-DONE.
           MOVE SPACES TO WS-CURRENT-USERNAME.
           CALL "ACCOUNT-LOAD".
           CALL "PROFILE-LOAD".
           PERFORM UNTIL PROGRAM-DONE OR END-OF-INPUT
               PERFORM TOP-LEVEL-MENU
           END-PERFORM.
           CALL "READ-LINE-CLOSE".
           CALL "WRITE-LINE-CLOSE".
           STOP RUN.

       TOP-LEVEL-MENU SECTION.
       TOP-LEVEL-MENU-START.
           MOVE "Welcome to InCollege!" TO WS-MESSAGE
           CALL "WRITE-LINE" USING WS-MESSAGE.
           MOVE "Log In" TO WS-MESSAGE
           CALL "WRITE-LINE" USING WS-MESSAGE.
           MOVE "Create New Account" TO WS-MESSAGE
           CALL "WRITE-LINE" USING WS-MESSAGE.
           MOVE "Enter your choice:" TO WS-MESSAGE
           CALL "WRITE-LINE" USING WS-MESSAGE.
           CALL "READ-LINE" USING WS-LINE-TEXT.
           IF NOT END-OF-INPUT
               MOVE WS-LINE-TEXT TO WS-TOP-CHOICE
               EVALUATE FUNCTION TRIM(WS-TOP-CHOICE)
                   WHEN "1"
                       MOVE 'N' TO WS-LOGIN-OK
                       CALL "ACCOUNT-LOGIN" USING WS-LOGIN-OK
                       IF WS-LOGIN-OK = 'Y'
                           PERFORM POST-LOGIN-MENU
                       END-IF
                   WHEN "2"
                       CALL "ACCOUNT-CREATE"
                   WHEN OTHER
                       MOVE "Invalid choice, please try again."
                           TO WS-MESSAGE
                       CALL "WRITE-LINE" USING WS-MESSAGE
               END-EVALUATE
           END-IF.

       POST-LOGIN-MENU SECTION.
       POST-LOGIN-MENU-START.
           MOVE 'N' TO WS-LOGOUT-FLAG.
           PERFORM UNTIL LOGOUT-REQUESTED OR END-OF-INPUT
               MOVE "1. Create/Edit My Profile" TO WS-MESSAGE
               CALL "WRITE-LINE" USING WS-MESSAGE
               MOVE "2. View My Profile" TO WS-MESSAGE
               CALL "WRITE-LINE" USING WS-MESSAGE
               MOVE "3. Search for a job" TO WS-MESSAGE
               CALL "WRITE-LINE" USING WS-MESSAGE
               MOVE "4. Find someone you know" TO WS-MESSAGE
               CALL "WRITE-LINE" USING WS-MESSAGE
               MOVE "5. Learn a New Skill" TO WS-MESSAGE
               CALL "WRITE-LINE" USING WS-MESSAGE
               MOVE "6. Logout" TO WS-MESSAGE
               CALL "WRITE-LINE" USING WS-MESSAGE
               MOVE "Enter your choice:" TO WS-MESSAGE
               CALL "WRITE-LINE" USING WS-MESSAGE
               CALL "READ-LINE" USING WS-LINE-TEXT
               IF NOT END-OF-INPUT
                   MOVE WS-LINE-TEXT TO WS-POST-CHOICE
                   EVALUATE FUNCTION TRIM(WS-POST-CHOICE)
                       WHEN "1"
                           CALL "PROFILE-EDIT"
                       WHEN "2"
                           CALL "PROFILE-VIEW"
                       WHEN "3"
                           MOVE "Job search/internship is under construction."
                               TO WS-MESSAGE
                           CALL "WRITE-LINE" USING WS-MESSAGE
                       WHEN "4"
                           MOVE "Find someone you know is under construction."
                               TO WS-MESSAGE
                           CALL "WRITE-LINE" USING WS-MESSAGE
                       WHEN "5"
                           CALL "SKILL-MENU"
                       WHEN "6"
                           MOVE 'Y' TO WS-LOGOUT-FLAG
                           MOVE 'Y' TO WS-PROGRAM-DONE
                       WHEN OTHER
                           MOVE "Invalid choice, please try again."
                               TO WS-MESSAGE
                           CALL "WRITE-LINE" USING WS-MESSAGE
                   END-EVALUATE
               END-IF
           END-PERFORM.
       END PROGRAM INCOLLEGE.
