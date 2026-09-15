      *>*******************************************************
      *> InCollege-ProfileView.cob
      *> Displays the current user's saved profile, or a "not
      *> created yet" message.
      *>*******************************************************
       >>SOURCE FORMAT FREE
       IDENTIFICATION DIVISION.
       PROGRAM-ID. PROFILE-VIEW.

       DATA DIVISION.
       WORKING-STORAGE SECTION.
       COPY "InCollege-Common.cpy".
       COPY "InCollege-ProfileTable.cpy".

       01  WS-MESSAGE                  PIC X(100).
       01  WS-PROFILE-IDX              PIC 9(2).
       01  WS-FINDROW-APPEND           PIC X.
       01  WS-IDX                      PIC 9(2).
       01  WS-EXP-IDX                  PIC 9.
       01  WS-EDU-IDX                  PIC 9.

       PROCEDURE DIVISION.
       PROFILE-VIEW-START.
           MOVE 'N' TO WS-FINDROW-APPEND.
           PERFORM PROFILE-FIND-ROW.
           IF WS-PROFILE-IDX = 0
               MOVE "You have not created a profile yet." TO WS-MESSAGE
               CALL "WRITE-LINE" USING WS-MESSAGE
           ELSE
               MOVE "--- Your Profile ---" TO WS-MESSAGE
               CALL "WRITE-LINE" USING WS-MESSAGE
               MOVE SPACES TO WS-MESSAGE
               STRING "Name: " DELIMITED BY SIZE
                      FUNCTION TRIM(WS-PROF-FIRST-NAME(WS-PROFILE-IDX))
                          DELIMITED BY SIZE
                      " " DELIMITED BY SIZE
                      FUNCTION TRIM(WS-PROF-LAST-NAME(WS-PROFILE-IDX))
                          DELIMITED BY SIZE
                      INTO WS-MESSAGE
               CALL "WRITE-LINE" USING WS-MESSAGE
               MOVE SPACES TO WS-MESSAGE
               STRING "University: " DELIMITED BY SIZE
                      FUNCTION TRIM(WS-PROF-UNIVERSITY(WS-PROFILE-IDX))
                          DELIMITED BY SIZE
                      INTO WS-MESSAGE
               CALL "WRITE-LINE" USING WS-MESSAGE
               MOVE SPACES TO WS-MESSAGE
               STRING "Major: " DELIMITED BY SIZE
                      FUNCTION TRIM(WS-PROF-MAJOR(WS-PROFILE-IDX))
                          DELIMITED BY SIZE
                      INTO WS-MESSAGE
               CALL "WRITE-LINE" USING WS-MESSAGE
               MOVE SPACES TO WS-MESSAGE
               STRING "Graduation Year: " DELIMITED BY SIZE
                      WS-PROF-GRAD-YEAR(WS-PROFILE-IDX) DELIMITED BY SIZE
                      INTO WS-MESSAGE
               CALL "WRITE-LINE" USING WS-MESSAGE
               IF FUNCTION TRIM(WS-PROF-ABOUT-ME(WS-PROFILE-IDX)) NOT = SPACES
                   MOVE SPACES TO WS-MESSAGE
                   STRING "About Me: " DELIMITED BY SIZE
                          FUNCTION TRIM(WS-PROF-ABOUT-ME(WS-PROFILE-IDX))
                              DELIMITED BY SIZE
                          INTO WS-MESSAGE
                   CALL "WRITE-LINE" USING WS-MESSAGE
               END-IF
               IF WS-PROF-EXP-COUNT(WS-PROFILE-IDX) > 0
                   MOVE "Experience:" TO WS-MESSAGE
                   CALL "WRITE-LINE" USING WS-MESSAGE
                   PERFORM VARYING WS-EXP-IDX FROM 1 BY 1
                           UNTIL WS-EXP-IDX > WS-PROF-EXP-COUNT(WS-PROFILE-IDX)
                       MOVE SPACES TO WS-MESSAGE
                       STRING "  Title: " DELIMITED BY SIZE
                              FUNCTION TRIM(WS-PROF-EXP-TITLE
                                  (WS-PROFILE-IDX, WS-EXP-IDX))
                                  DELIMITED BY SIZE
                              INTO WS-MESSAGE
                       CALL "WRITE-LINE" USING WS-MESSAGE
                       MOVE SPACES TO WS-MESSAGE
                       STRING "  Company: " DELIMITED BY SIZE
                              FUNCTION TRIM(WS-PROF-EXP-COMPANY
                                  (WS-PROFILE-IDX, WS-EXP-IDX))
                                  DELIMITED BY SIZE
                              INTO WS-MESSAGE
                       CALL "WRITE-LINE" USING WS-MESSAGE
                       MOVE SPACES TO WS-MESSAGE
                       STRING "  Dates: " DELIMITED BY SIZE
                              FUNCTION TRIM(WS-PROF-EXP-DATES
                                  (WS-PROFILE-IDX, WS-EXP-IDX))
                                  DELIMITED BY SIZE
                              INTO WS-MESSAGE
                       CALL "WRITE-LINE" USING WS-MESSAGE
                       IF FUNCTION TRIM(WS-PROF-EXP-DESC
                              (WS-PROFILE-IDX, WS-EXP-IDX)) NOT = SPACES
                           MOVE SPACES TO WS-MESSAGE
                           STRING "  Description: " DELIMITED BY SIZE
                                  FUNCTION TRIM(WS-PROF-EXP-DESC
                                      (WS-PROFILE-IDX, WS-EXP-IDX))
                                      DELIMITED BY SIZE
                                  INTO WS-MESSAGE
                           CALL "WRITE-LINE" USING WS-MESSAGE
                       END-IF
                   END-PERFORM
               END-IF
               IF WS-PROF-EDU-COUNT(WS-PROFILE-IDX) > 0
                   MOVE "Education:" TO WS-MESSAGE
                   CALL "WRITE-LINE" USING WS-MESSAGE
                   PERFORM VARYING WS-EDU-IDX FROM 1 BY 1
                           UNTIL WS-EDU-IDX > WS-PROF-EDU-COUNT(WS-PROFILE-IDX)
                       MOVE SPACES TO WS-MESSAGE
                       STRING "  Degree: " DELIMITED BY SIZE
                              FUNCTION TRIM(WS-PROF-EDU-DEGREE
                                  (WS-PROFILE-IDX, WS-EDU-IDX))
                                  DELIMITED BY SIZE
                              INTO WS-MESSAGE
                       CALL "WRITE-LINE" USING WS-MESSAGE
                       MOVE SPACES TO WS-MESSAGE
                       STRING "  University: " DELIMITED BY SIZE
                              FUNCTION TRIM(WS-PROF-EDU-UNIVERSITY
                                  (WS-PROFILE-IDX, WS-EDU-IDX))
                                  DELIMITED BY SIZE
                              INTO WS-MESSAGE
                       CALL "WRITE-LINE" USING WS-MESSAGE
                       MOVE SPACES TO WS-MESSAGE
                       STRING "  Years: " DELIMITED BY SIZE
                              FUNCTION TRIM(WS-PROF-EDU-YEARS
                                  (WS-PROFILE-IDX, WS-EDU-IDX))
                                  DELIMITED BY SIZE
                              INTO WS-MESSAGE
                       CALL "WRITE-LINE" USING WS-MESSAGE
                   END-PERFORM
               END-IF
               MOVE "--------------------" TO WS-MESSAGE
               CALL "WRITE-LINE" USING WS-MESSAGE
           END-IF.
           GOBACK.

       COPY "InCollege-ProfileFindRow.cpy".
       END PROGRAM PROFILE-VIEW.
