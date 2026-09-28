       >>SOURCE FORMAT FREE
       IDENTIFICATION DIVISION.
       PROGRAM-ID. FREQ-LOAD.

       ENVIRONMENT DIVISION.
       INPUT-OUTPUT SECTION.
       FILE-CONTROL.
           SELECT FREQ-FILE ASSIGN TO "InCollege-Requests.txt"
               ORGANIZATION IS LINE SEQUENTIAL
               FILE STATUS IS WS-FREQ-FILE-STATUS.

       DATA DIVISION.
       FILE SECTION.
       FD  FREQ-FILE.
       COPY "InCollege-PendingRequest.cpy".

       WORKING-STORAGE SECTION.
       COPY "InCollege-PendingRequestTable.cpy".

       01  WS-FREQ-FILE-STATUS         PIC XX.
       01  WS-FREQ-EOF                 PIC X VALUE 'N'.
           88  END-OF-FREQ                      VALUE 'Y'.

       PROCEDURE DIVISION.
       FREQ-LOAD-START.
           MOVE 0 TO WS-FREQ-COUNT.
           MOVE 'N' TO WS-FREQ-EOF.
           OPEN INPUT FREQ-FILE.
           IF WS-FREQ-FILE-STATUS NOT = "35"
               PERFORM UNTIL END-OF-FREQ
                   READ FREQ-FILE
                       AT END
                           MOVE 'Y' TO WS-FREQ-EOF
                       NOT AT END
                           ADD 1 TO WS-FREQ-COUNT
                           MOVE PEND-REC-SENDER
                               TO WS-FREQ-SENDER(WS-FREQ-COUNT)
                           MOVE PEND-REC-RECEIVER
                               TO WS-FREQ-RECEIVER(WS-FREQ-COUNT)
                           MOVE PEND-REC-STATUS
                               TO WS-FREQ-STATUS(WS-FREQ-COUNT)
                   END-READ
               END-PERFORM
               CLOSE FREQ-FILE
           END-IF.
           GOBACK.
       END PROGRAM FREQ-LOAD.

       IDENTIFICATION DIVISION.
       PROGRAM-ID. FREQ-VIEW-PENDING.

       DATA DIVISION.
       WORKING-STORAGE SECTION.
       COPY "InCollege-Common.cpy".
       COPY "InCollege-PendingRequestTable.cpy".

       01  WS-MESSAGE                  PIC X(100).
       01  WS-IDX                      PIC 9(2).
       01  WS-PENDING-FOUND            PIC X VALUE 'N'.

       PROCEDURE DIVISION.
       FREQ-VIEW-PENDING-START.
           MOVE "==== Pending Connection Requests ====" TO WS-MESSAGE
           CALL "WRITE-LINE" USING WS-MESSAGE
           MOVE 'N' TO WS-PENDING-FOUND.

           IF WS-FREQ-COUNT > 0
               PERFORM VARYING WS-IDX FROM 1 BY 1
                       UNTIL WS-IDX > WS-FREQ-COUNT
                   IF FUNCTION TRIM(WS-FREQ-RECEIVER(WS-IDX)) =
                      FUNCTION TRIM(WS-CURRENT-USERNAME) AND
                      FUNCTION TRIM(WS-FREQ-STATUS(WS-IDX)) = "PENDING"
                       MOVE 'Y' TO WS-PENDING-FOUND
                       MOVE SPACES TO WS-MESSAGE
                       STRING "From: " DELIMITED BY SIZE
                              FUNCTION TRIM(WS-FREQ-SENDER(WS-IDX)) DELIMITED BY SIZE
                              INTO WS-MESSAGE
                       CALL "WRITE-LINE" USING WS-MESSAGE
                   END-IF
               END-PERFORM
           END-IF.

           IF WS-PENDING-FOUND = 'N'
               MOVE "You have no pending connection requests." TO WS-MESSAGE
               CALL "WRITE-LINE" USING WS-MESSAGE
           END-IF.
           GOBACK.
       END PROGRAM FREQ-VIEW-PENDING.

IDENTIFICATION DIVISION.
       PROGRAM-ID. FREQ-SEND-REQUEST.

       ENVIRONMENT DIVISION.
       INPUT-OUTPUT SECTION.
       FILE-CONTROL.
           SELECT FREQ-FILE ASSIGN TO "InCollege-PendingRequests.txt"
               ORGANIZATION IS LINE SEQUENTIAL
               FILE STATUS IS WS-FREQ-FILE-STATUS.

       DATA DIVISION.
       FILE SECTION.
       FD  FREQ-FILE.
       COPY "InCollege-PendingRequest.cpy".

       WORKING-STORAGE SECTION.
       COPY "InCollege-Common.cpy".
       COPY "InCollege-AccountTable.cpy".
       COPY "InCollege-PendingRequestTable.cpy".

       01  WS-FREQ-FILE-STATUS         PIC XX.
       01  WS-MESSAGE                  PIC X(100).
       01  WS-IDX                      PIC 9(2).
       01  WS-TARGET-EXISTS            PIC X VALUE 'N'.

       LINKAGE SECTION.
       01  LS-TARGET-USERNAME          PIC X(20).

       PROCEDURE DIVISION USING LS-TARGET-USERNAME.
       FREQ-SEND-START.
           *> 1. Verify target account exists
           MOVE 'N' TO WS-TARGET-EXISTS
           IF WS-ACCOUNT-COUNT > 0
               PERFORM VARYING WS-IDX FROM 1 BY 1
                       UNTIL WS-IDX > WS-ACCOUNT-COUNT
                   IF FUNCTION TRIM(WS-ACCT-USERNAME(WS-IDX)) =
                      FUNCTION TRIM(LS-TARGET-USERNAME)
                       MOVE 'Y' TO WS-TARGET-EXISTS
                   END-IF
               END-PERFORM
           END-IF

           IF WS-TARGET-EXISTS = 'N'
               MOVE "The specified user does not exist." TO WS-MESSAGE
               CALL "WRITE-LINE" USING WS-MESSAGE
           ELSE IF WS-FREQ-COUNT >= 50
               MOVE "System storage for connection requests is full." TO WS-MESSAGE
               CALL "WRITE-LINE" USING WS-MESSAGE
           ELSE
               *> 2. Add and save request if valid
               ADD 1 TO WS-FREQ-COUNT
               MOVE WS-CURRENT-USERNAME TO WS-FREQ-SENDER(WS-FREQ-COUNT)
               MOVE LS-TARGET-USERNAME TO WS-FREQ-RECEIVER(WS-FREQ-COUNT)
               MOVE "PENDING" TO WS-FREQ-STATUS(WS-FREQ-COUNT)

               OPEN EXTEND FREQ-FILE
               IF WS-FREQ-FILE-STATUS = "35"
                   OPEN OUTPUT FREQ-FILE
               END-IF

               MOVE WS-FREQ-SENDER(WS-FREQ-COUNT) TO PEND-REC-SENDER
               MOVE WS-FREQ-RECEIVER(WS-FREQ-COUNT) TO PEND-REC-RECEIVER
               MOVE WS-FREQ-STATUS(WS-FREQ-COUNT) TO PEND-REC-STATUS
               WRITE PENDING-REQUEST-RECORD
               CLOSE FREQ-FILE

               MOVE "Connection request sent successfully." TO WS-MESSAGE
               CALL "WRITE-LINE" USING WS-MESSAGE
           END-IF.
           GOBACK.
       END PROGRAM FREQ-SEND-REQUEST.