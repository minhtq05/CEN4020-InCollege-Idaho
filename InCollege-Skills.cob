      *>*******************************************************
      *> InCollege-Skills.cob
      *> "Learn a New Skill" browsing menu.
      *>*******************************************************
       >>SOURCE FORMAT FREE
       IDENTIFICATION DIVISION.
       PROGRAM-ID. SKILL-MENU.

       DATA DIVISION.
       WORKING-STORAGE SECTION.
       COPY "InCollege-Common.cpy".

       01  WS-MESSAGE                  PIC X(100).
       01  WS-LINE-TEXT                PIC X(100).
       01  WS-SKILL-CHOICE             PIC X(20).
       01  WS-IDX                      PIC 9(2).

       01  WS-SKILLS-TABLE.
           05  WS-SKILL OCCURS 5 TIMES PIC X(30) VALUE SPACES.

       01  WS-SKILL-MENU-DONE          PIC X VALUE 'N'.
           88  SKILL-MENU-DONE                   VALUE 'Y'.

       PROCEDURE DIVISION.
       SKILL-MENU-START.
           MOVE "Python Programming" TO WS-SKILL(1).
           MOVE "Public Speaking"    TO WS-SKILL(2).
           MOVE "Resume Writing"     TO WS-SKILL(3).
           MOVE "Data Analysis"      TO WS-SKILL(4).
           MOVE "Networking"         TO WS-SKILL(5).

           MOVE 'N' TO WS-SKILL-MENU-DONE.
           PERFORM UNTIL SKILL-MENU-DONE OR END-OF-INPUT
               MOVE "Learn a New Skill:" TO WS-MESSAGE
               CALL "WRITE-LINE" USING WS-MESSAGE
               PERFORM VARYING WS-IDX FROM 1 BY 1 UNTIL WS-IDX > 5
                   MOVE WS-SKILL(WS-IDX) TO WS-MESSAGE
                   CALL "WRITE-LINE" USING WS-MESSAGE
               END-PERFORM
               MOVE "Go Back" TO WS-MESSAGE
               CALL "WRITE-LINE" USING WS-MESSAGE
               MOVE "Enter your choice:" TO WS-MESSAGE
               CALL "WRITE-LINE" USING WS-MESSAGE
               CALL "READ-LINE" USING WS-LINE-TEXT
               IF NOT END-OF-INPUT
                   MOVE WS-LINE-TEXT TO WS-SKILL-CHOICE
                   EVALUATE FUNCTION TRIM(WS-SKILL-CHOICE)
                       WHEN "1" THRU "5"
                           MOVE "This skill is under construction."
                               TO WS-MESSAGE
                           CALL "WRITE-LINE" USING WS-MESSAGE
                       WHEN "6"
                           MOVE 'Y' TO WS-SKILL-MENU-DONE
                       WHEN OTHER
                           MOVE "Invalid choice, please try again."
                               TO WS-MESSAGE
                           CALL "WRITE-LINE" USING WS-MESSAGE
                   END-EVALUATE
               END-IF
           END-PERFORM.
           GOBACK.
       END PROGRAM SKILL-MENU.
