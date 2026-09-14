      *>*******************************************************
      *> InCollege-ProfileFindRow.cpy
      *> Resolves WS-CURRENT-USERNAME to a row index in
      *> WS-PROFILE-TABLE (InCollege-ProfileTable.cpy).
      *>
      *> Caller must declare, in WORKING-STORAGE:
      *>   01  WS-PROFILE-IDX     PIC 9(2).
      *>   01  WS-FINDROW-APPEND  PIC X.
      *>   01  WS-IDX             PIC 9(2).
      *>
      *> Set WS-FINDROW-APPEND to 'Y' before PERFORM PROFILE-FIND-ROW
      *> to append a new row (up to the 5-row cap) when the username
      *> isn't found yet; 'N' to only look up an existing row.
      *> Result: WS-PROFILE-IDX = 0 if not found (and not appended).
      *>*******************************************************
       PROFILE-FIND-ROW SECTION.
       PROFILE-FIND-ROW-START.
           MOVE 0 TO WS-PROFILE-IDX.
           IF WS-PROFILE-COUNT > 0
               PERFORM VARYING WS-IDX FROM 1 BY 1
                       UNTIL WS-IDX > WS-PROFILE-COUNT
                   IF FUNCTION TRIM(WS-PROF-USERNAME(WS-IDX)) =
                      FUNCTION TRIM(WS-CURRENT-USERNAME)
                       MOVE WS-IDX TO WS-PROFILE-IDX
                   END-IF
               END-PERFORM
           END-IF.
           IF WS-PROFILE-IDX = 0 AND WS-FINDROW-APPEND = 'Y'
                   AND WS-PROFILE-COUNT < 5
               ADD 1 TO WS-PROFILE-COUNT
               MOVE FUNCTION TRIM(WS-CURRENT-USERNAME)
                   TO WS-PROF-USERNAME(WS-PROFILE-COUNT)
               MOVE WS-PROFILE-COUNT TO WS-PROFILE-IDX
           END-IF.
