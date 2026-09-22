      *>*******************************************************
      *> InCollege-UserSearch.cob
      *> Exact full-name lookup for saved profiles belonging to
      *> users other than the currently logged-in user.
      *>*******************************************************
       >>SOURCE FORMAT FREE
       IDENTIFICATION DIVISION.
       PROGRAM-ID. USER-SEARCH.

       DATA DIVISION.
       WORKING-STORAGE SECTION.
       COPY "InCollege-Common.cpy".
       COPY "InCollege-ProfileTable.cpy".

       01  WS-MESSAGE                  PIC X(100).
       01  WS-TARGET-NAME              PIC X(100).
       01  WS-CANDIDATE-NAME           PIC X(61).
       01  WS-IDX                      PIC 9(2).
       01  WS-MATCH-FOUND              PIC X VALUE 'N'.
           88  MATCH-FOUND                      VALUE 'Y'.

       01 WS-FOUND-IDX                 PIC 9(2).
       01  WS-EXP-IDX                  PIC 9.
       01  WS-EDU-IDX                  PIC 9.

       PROCEDURE DIVISION.
       USER-SEARCH-START.
           MOVE "Enter the full name of the person you are looking for:"
               TO WS-MESSAGE.
           CALL "WRITE-LINE" USING WS-MESSAGE.
           CALL "READ-LINE" USING WS-TARGET-NAME.

           IF NOT END-OF-INPUT
               MOVE 'N' TO WS-MATCH-FOUND
               PERFORM VARYING WS-IDX FROM 1 BY 1
                       UNTIL WS-IDX > WS-PROFILE-COUNT OR MATCH-FOUND
                   IF WS-PROF-HAS-DATA(WS-IDX) = 'Y'
                      AND FUNCTION TRIM(WS-PROF-USERNAME(WS-IDX)) NOT =
                          FUNCTION TRIM(WS-CURRENT-USERNAME)
                       MOVE SPACES TO WS-CANDIDATE-NAME
                       STRING
                           FUNCTION TRIM(WS-PROF-FIRST-NAME(WS-IDX))
                               DELIMITED BY SIZE
                           " " DELIMITED BY SIZE
                           FUNCTION TRIM(WS-PROF-LAST-NAME(WS-IDX))
                               DELIMITED BY SIZE
                           INTO WS-CANDIDATE-NAME
                       IF FUNCTION TRIM(WS-TARGET-NAME) =
                          FUNCTION TRIM(WS-CANDIDATE-NAME)
                           MOVE 'Y' TO WS-MATCH-FOUND
                           MOVE WS-IDX TO WS-FOUND-IDX
                       END-IF
                   END-IF
               END-PERFORM

               IF MATCH-FOUND
               MOVE SPACES TO WS-MESSAGE
               STRING "==== Profile for " DELIMITED BY SIZE
                      FUNCTION TRIM(WS-PROF-FIRST-NAME(WS-FOUND-IDX))
                          DELIMITED BY SIZE
                      " " DELIMITED BY SIZE
                      FUNCTION TRIM(WS-PROF-LAST-NAME(WS-FOUND-IDX))
                          DELIMITED BY SIZE
                      INTO WS-MESSAGE
               CALL "WRITE-LINE" USING WS-MESSAGE
               MOVE SPACES TO WS-MESSAGE
               CALL "WRITE-LINE" USING WS-MESSAGE
               MOVE SPACES TO WS-MESSAGE
               STRING "Name:             " DELIMITED BY SIZE
                      FUNCTION TRIM(WS-PROF-FIRST-NAME(WS-FOUND-IDX))
                          DELIMITED BY SIZE
                      " " DELIMITED BY SIZE
                      FUNCTION TRIM(WS-PROF-LAST-NAME(WS-FOUND-IDX))
                          DELIMITED BY SIZE
                      INTO WS-MESSAGE
               CALL "WRITE-LINE" USING WS-MESSAGE
               MOVE SPACES TO WS-MESSAGE
               STRING "University:       " DELIMITED BY SIZE
                      FUNCTION TRIM(WS-PROF-UNIVERSITY(WS-FOUND-IDX))
                          DELIMITED BY SIZE
                      INTO WS-MESSAGE
               CALL "WRITE-LINE" USING WS-MESSAGE
               MOVE SPACES TO WS-MESSAGE
               STRING "Major:            " DELIMITED BY SIZE
                      FUNCTION TRIM(WS-PROF-MAJOR(WS-FOUND-IDX))
                          DELIMITED BY SIZE
                      INTO WS-MESSAGE
               CALL "WRITE-LINE" USING WS-MESSAGE
               MOVE SPACES TO WS-MESSAGE
               STRING "Graduation Year:  " DELIMITED BY SIZE
                      WS-PROF-GRAD-YEAR(WS-FOUND-IDX) DELIMITED BY SIZE
                      INTO WS-MESSAGE
               CALL "WRITE-LINE" USING WS-MESSAGE
               IF FUNCTION TRIM(WS-PROF-ABOUT-ME(WS-FOUND-IDX)) NOT = SPACES
                   MOVE SPACES TO WS-MESSAGE
                   STRING "About Me:         " DELIMITED BY SIZE
                          FUNCTION TRIM(WS-PROF-ABOUT-ME(WS-FOUND-IDX))
                              DELIMITED BY SIZE
                          INTO WS-MESSAGE
                   CALL "WRITE-LINE" USING WS-MESSAGE
               END-IF
               IF WS-PROF-EXP-COUNT(WS-FOUND-IDX) > 0
                   MOVE SPACES TO WS-MESSAGE
                   CALL "WRITE-LINE" USING WS-MESSAGE
                   MOVE "Experience:" TO WS-MESSAGE
                   CALL "WRITE-LINE" USING WS-MESSAGE
                   PERFORM VARYING WS-EXP-IDX FROM 1 BY 1
                           UNTIL WS-EXP-IDX > WS-PROF-EXP-COUNT(WS-FOUND-IDX)
                       IF WS-EXP-IDX > 1
                           MOVE SPACES TO WS-MESSAGE
                           CALL "WRITE-LINE" USING WS-MESSAGE
                       END-IF
                       MOVE SPACES TO WS-MESSAGE
                       STRING "  Experience #" DELIMITED BY SIZE
                              WS-EXP-IDX DELIMITED BY SIZE
                              ":" DELIMITED BY SIZE
                              INTO WS-MESSAGE
                       CALL "WRITE-LINE" USING WS-MESSAGE
                       MOVE SPACES TO WS-MESSAGE
                       STRING "    Title:        " DELIMITED BY SIZE
                              FUNCTION TRIM(WS-PROF-EXP-TITLE
                                  (WS-FOUND-IDX, WS-EXP-IDX))
                                  DELIMITED BY SIZE
                              INTO WS-MESSAGE
                       CALL "WRITE-LINE" USING WS-MESSAGE
                       MOVE SPACES TO WS-MESSAGE
                       STRING "    Company:      " DELIMITED BY SIZE
                              FUNCTION TRIM(WS-PROF-EXP-COMPANY
                                  (WS-FOUND-IDX, WS-EXP-IDX))
                                  DELIMITED BY SIZE
                              INTO WS-MESSAGE
                       CALL "WRITE-LINE" USING WS-MESSAGE
                       MOVE SPACES TO WS-MESSAGE
                       STRING "    Dates:        " DELIMITED BY SIZE
                              FUNCTION TRIM(WS-PROF-EXP-DATES
                                  (WS-FOUND-IDX, WS-EXP-IDX))
                                  DELIMITED BY SIZE
                              INTO WS-MESSAGE
                       CALL "WRITE-LINE" USING WS-MESSAGE
                       IF FUNCTION TRIM(WS-PROF-EXP-DESC
                              (WS-FOUND-IDX, WS-EXP-IDX)) NOT = SPACES
                           MOVE SPACES TO WS-MESSAGE
                           STRING "    Description:  " DELIMITED BY SIZE
                                  FUNCTION TRIM(WS-PROF-EXP-DESC
                                      (WS-FOUND-IDX, WS-EXP-IDX))
                                      DELIMITED BY SIZE
                                  INTO WS-MESSAGE
                           CALL "WRITE-LINE" USING WS-MESSAGE
                       END-IF
                   END-PERFORM
               END-IF
               IF WS-PROF-EDU-COUNT(WS-FOUND-IDX) > 0
                   MOVE SPACES TO WS-MESSAGE
                   CALL "WRITE-LINE" USING WS-MESSAGE
                   MOVE "Education:" TO WS-MESSAGE
                   CALL "WRITE-LINE" USING WS-MESSAGE
                   PERFORM VARYING WS-EDU-IDX FROM 1 BY 1
                           UNTIL WS-EDU-IDX > WS-PROF-EDU-COUNT(WS-FOUND-IDX)
                       IF WS-EDU-IDX > 1
                           MOVE SPACES TO WS-MESSAGE
                           CALL "WRITE-LINE" USING WS-MESSAGE
                       END-IF
                       MOVE SPACES TO WS-MESSAGE
                       STRING "  Education #" DELIMITED BY SIZE
                              WS-EDU-IDX DELIMITED BY SIZE
                              ":" DELIMITED BY SIZE
                              INTO WS-MESSAGE
                       CALL "WRITE-LINE" USING WS-MESSAGE
                       MOVE SPACES TO WS-MESSAGE
                       STRING "    Degree:       " DELIMITED BY SIZE
                              FUNCTION TRIM(WS-PROF-EDU-DEGREE
                                  (WS-FOUND-IDX, WS-EDU-IDX))
                                  DELIMITED BY SIZE
                              INTO WS-MESSAGE
                       CALL "WRITE-LINE" USING WS-MESSAGE
                       MOVE SPACES TO WS-MESSAGE
                       STRING "    University:   " DELIMITED BY SIZE
                              FUNCTION TRIM(WS-PROF-EDU-UNIVERSITY
                                  (WS-FOUND-IDX, WS-EDU-IDX))
                                  DELIMITED BY SIZE
                              INTO WS-MESSAGE
                       CALL "WRITE-LINE" USING WS-MESSAGE
                       MOVE SPACES TO WS-MESSAGE
                       STRING "    Years:        " DELIMITED BY SIZE
                              FUNCTION TRIM(WS-PROF-EDU-YEARS
                                  (WS-FOUND-IDX, WS-EDU-IDX))
                                  DELIMITED BY SIZE
                              INTO WS-MESSAGE
                       CALL "WRITE-LINE" USING WS-MESSAGE
                   END-PERFORM
               END-IF
               MOVE SPACES TO WS-MESSAGE
               CALL "WRITE-LINE" USING WS-MESSAGE
               MOVE "--------------------" TO WS-MESSAGE
               CALL "WRITE-LINE" USING WS-MESSAGE
               ELSE
                   MOVE "No one by that name could be found."
                       TO WS-MESSAGE
                   CALL "WRITE-LINE" USING WS-MESSAGE
               END-IF               
           END-IF.
           GOBACK.
       END PROGRAM USER-SEARCH.
