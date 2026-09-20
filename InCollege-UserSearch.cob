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
                       END-IF
                   END-IF
               END-PERFORM

               IF MATCH-FOUND
                   MOVE "They are a part of the InCollege system."
                       TO WS-MESSAGE
               ELSE
                   MOVE "They are not yet a part of the InCollege system."
                       TO WS-MESSAGE
               END-IF
               CALL "WRITE-LINE" USING WS-MESSAGE
           END-IF.
           GOBACK.
       END PROGRAM USER-SEARCH.
