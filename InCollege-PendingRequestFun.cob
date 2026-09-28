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
       01  WS-FREQ-FILE-STATUS         PIC X(2).
       01  WS-FREQ-EOF                 PIC X VALUE 'N'.

       PROCEDURE DIVISION.
       FREQ-LOAD-START.
           MOVE 0 TO WS-FREQ-COUNT
           MOVE 'N' TO WS-FREQ-EOF
           OPEN INPUT FREQ-FILE
           IF WS-FREQ-FILE-STATUS = "00"
               PERFORM UNTIL WS-FREQ-EOF = 'Y'
                   READ FREQ-FILE
                       AT END
                           MOVE 'Y' TO WS-FREQ-EOF
                       NOT AT END
                           IF WS-FREQ-COUNT < 50
                               ADD 1 TO WS-FREQ-COUNT
                               MOVE PEND-REC-SENDER
                                   TO WS-FREQ-SENDER(WS-FREQ-COUNT)
                               MOVE PEND-REC-RECEIVER
                                   TO WS-FREQ-RECEIVER(WS-FREQ-COUNT)
                               MOVE PEND-REC-STATUS
                                   TO WS-FREQ-STATUS(WS-FREQ-COUNT)
                           END-IF
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
       01  WS-MESSAGE                  PIC X(100).
       01  WS-IDX                      PIC 9(2).
       01  WS-FOUND                    PIC X VALUE 'N'.

       PROCEDURE DIVISION.
       FREQ-VIEW-START.
           MOVE 'N' TO WS-FOUND
           IF WS-FREQ-COUNT > 0
               PERFORM VARYING WS-IDX FROM 1 BY 1
                       UNTIL WS-IDX > WS-FREQ-COUNT
                   IF FUNCTION TRIM(WS-FREQ-RECEIVER(WS-IDX)) =
                      FUNCTION TRIM(WS-CURRENT-USERNAME)
                      AND FUNCTION TRIM(WS-FREQ-STATUS(WS-IDX)) = "PENDING"
                       IF WS-FOUND = 'N'
                           MOVE "Pending connection requests:" TO WS-MESSAGE
                           CALL "WRITE-LINE" USING WS-MESSAGE
                           MOVE 'Y' TO WS-FOUND
                       END-IF
                       STRING "  From: "
                              FUNCTION TRIM(WS-FREQ-SENDER(WS-IDX))
                              DELIMITED BY SIZE
                              INTO WS-MESSAGE
                       END-STRING
                       CALL "WRITE-LINE" USING WS-MESSAGE
                   END-IF
               END-PERFORM
           END-IF.

           IF WS-FOUND = 'N'
               MOVE "No pending connection requests." TO WS-MESSAGE
               CALL "WRITE-LINE" USING WS-MESSAGE
           END-IF.
           GOBACK.
       END PROGRAM FREQ-VIEW-PENDING.

       IDENTIFICATION DIVISION.
       PROGRAM-ID. FREQ-SEND-REQUEST.
       *>*******************************************************
       *> FREQ-SEND-REQUEST
       *> Called as:  CALL "FREQ-SEND-REQUEST" USING <PIC X(20)>
       *> where the argument is the USERNAME of the person the
       *> logged-in user wants to connect with.
       *>
       *> It decides whether the request is allowed, and either
       *>   - adds it (status PENDING) to the in-memory table AND
       *>     appends it to InCollege-Requests.txt, or
       *>   - tells the user why it was not sent.
       *> Every outcome prints exactly one message to the user.
       *>*******************************************************

       ENVIRONMENT DIVISION.
       INPUT-OUTPUT SECTION.
       FILE-CONTROL.
       *> Must be the SAME file name FREQ-LOAD reads, otherwise
       *> requests are saved to one file and loaded from another.
           SELECT FREQ-FILE ASSIGN TO "InCollege-Requests.txt"
               ORGANIZATION IS LINE SEQUENTIAL
               FILE STATUS IS WS-FREQ-FILE-STATUS.

       DATA DIVISION.
       FILE SECTION.
       FD  FREQ-FILE.
       COPY "InCollege-PendingRequest.cpy".

       WORKING-STORAGE SECTION.
       01  WS-FREQ-FILE-STATUS         PIC X(2).
       01  WS-MESSAGE                  PIC X(100).
       01  WS-IDX                      PIC 9(2).
       01  WS-TARGET-EXISTS            PIC X VALUE 'N'.
       01  WS-ALREADY-CONNECTED        PIC X VALUE 'N'.
       01  WS-ALREADY-SENT             PIC X VALUE 'N'.
       01  WS-ALREADY-RECEIVED         PIC X VALUE 'N'.
       01  WS-SAVE-OK                  PIC X VALUE 'N'.

       LINKAGE SECTION.
       01  LS-TARGET-USERNAME          PIC X(20).

       PROCEDURE DIVISION USING LS-TARGET-USERNAME.
       FREQ-SEND-START.
           PERFORM CHECK-TARGET-EXISTS.
           PERFORM CHECK-EXISTING-REQUESTS.

       *> The first WHEN that is true wins, so the order below is
       *> the order of priority for the error messages.
           EVALUATE TRUE
               WHEN FUNCTION TRIM(LS-TARGET-USERNAME) =
                    FUNCTION TRIM(WS-CURRENT-USERNAME)
                   MOVE "You cannot send a connection request to yourself."
                       TO WS-MESSAGE
                   CALL "WRITE-LINE" USING WS-MESSAGE
               WHEN WS-TARGET-EXISTS = 'N'
                   MOVE "The specified user does not exist."
                       TO WS-MESSAGE
                   CALL "WRITE-LINE" USING WS-MESSAGE
               WHEN WS-ALREADY-CONNECTED = 'Y'
                   MOVE "You are already connected with this user."
                       TO WS-MESSAGE
                   CALL "WRITE-LINE" USING WS-MESSAGE
               WHEN WS-ALREADY-SENT = 'Y'
                   MOVE "You have already sent a connection request to this user."
                       TO WS-MESSAGE
                   CALL "WRITE-LINE" USING WS-MESSAGE
               WHEN WS-ALREADY-RECEIVED = 'Y'
                   MOVE "This user already sent you a request. See option 7."
                       TO WS-MESSAGE
                   CALL "WRITE-LINE" USING WS-MESSAGE
               WHEN WS-FREQ-COUNT >= 50
                   MOVE "System storage for connection requests is full."
                       TO WS-MESSAGE
                   CALL "WRITE-LINE" USING WS-MESSAGE
               WHEN OTHER
                   PERFORM ADD-AND-SAVE-REQUEST
           END-EVALUATE.
           GOBACK.

       *> Look through the account table for the target username.
       CHECK-TARGET-EXISTS.
           MOVE 'N' TO WS-TARGET-EXISTS.
           IF WS-ACCOUNT-COUNT > 0
               PERFORM VARYING WS-IDX FROM 1 BY 1
                       UNTIL WS-IDX > WS-ACCOUNT-COUNT
                   IF FUNCTION TRIM(WS-ACCT-USERNAME(WS-IDX)) =
                      FUNCTION TRIM(LS-TARGET-USERNAME)
                       MOVE 'Y' TO WS-TARGET-EXISTS
                   END-IF
               END-PERFORM
           END-IF.

       *> Look through every saved request that involves BOTH the
       *> logged-in user and the target (in either direction) and
       *> set flags describing what already exists between them.
       CHECK-EXISTING-REQUESTS.
           MOVE 'N' TO WS-ALREADY-CONNECTED.
           MOVE 'N' TO WS-ALREADY-SENT.
           MOVE 'N' TO WS-ALREADY-RECEIVED.
           IF WS-FREQ-COUNT > 0
               PERFORM VARYING WS-IDX FROM 1 BY 1
                       UNTIL WS-IDX > WS-FREQ-COUNT
       *> A request I sent to the target
                   IF FUNCTION TRIM(WS-FREQ-SENDER(WS-IDX)) =
                      FUNCTION TRIM(WS-CURRENT-USERNAME)
                      AND FUNCTION TRIM(WS-FREQ-RECEIVER(WS-IDX)) =
                          FUNCTION TRIM(LS-TARGET-USERNAME)
                       IF FUNCTION TRIM(WS-FREQ-STATUS(WS-IDX)) =
                          "ACCEPTED"
                           MOVE 'Y' TO WS-ALREADY-CONNECTED
                       END-IF
                       IF FUNCTION TRIM(WS-FREQ-STATUS(WS-IDX)) =
                          "PENDING"
                           MOVE 'Y' TO WS-ALREADY-SENT
                       END-IF
                   END-IF
       *> A request the target sent to me
                   IF FUNCTION TRIM(WS-FREQ-SENDER(WS-IDX)) =
                      FUNCTION TRIM(LS-TARGET-USERNAME)
                      AND FUNCTION TRIM(WS-FREQ-RECEIVER(WS-IDX)) =
                          FUNCTION TRIM(WS-CURRENT-USERNAME)
                       IF FUNCTION TRIM(WS-FREQ-STATUS(WS-IDX)) =
                          "ACCEPTED"
                           MOVE 'Y' TO WS-ALREADY-CONNECTED
                       END-IF
                       IF FUNCTION TRIM(WS-FREQ-STATUS(WS-IDX)) =
                          "PENDING"
                           MOVE 'Y' TO WS-ALREADY-RECEIVED
                       END-IF
                   END-IF
               END-PERFORM
           END-IF.

       *> Add the request to the table, append it to the file, and
       *> tell the user whether that worked.
       ADD-AND-SAVE-REQUEST.
           ADD 1 TO WS-FREQ-COUNT.
           MOVE WS-CURRENT-USERNAME TO WS-FREQ-SENDER(WS-FREQ-COUNT).
           MOVE LS-TARGET-USERNAME TO WS-FREQ-RECEIVER(WS-FREQ-COUNT).
           MOVE "PENDING" TO WS-FREQ-STATUS(WS-FREQ-COUNT).

           MOVE 'N' TO WS-SAVE-OK.
           OPEN EXTEND FREQ-FILE.
           IF WS-FREQ-FILE-STATUS = "35"
               OPEN OUTPUT FREQ-FILE
           END-IF.
           IF WS-FREQ-FILE-STATUS = "00"
               MOVE WS-FREQ-SENDER(WS-FREQ-COUNT) TO PEND-REC-SENDER
               MOVE WS-FREQ-RECEIVER(WS-FREQ-COUNT) TO PEND-REC-RECEIVER
               MOVE WS-FREQ-STATUS(WS-FREQ-COUNT) TO PEND-REC-STATUS
               WRITE PENDING-REQUEST-RECORD
               IF WS-FREQ-FILE-STATUS = "00"
                   MOVE 'Y' TO WS-SAVE-OK
               END-IF
               CLOSE FREQ-FILE
           END-IF.

           IF WS-SAVE-OK = 'Y'
               MOVE "Connection request sent successfully."
                   TO WS-MESSAGE
               CALL "WRITE-LINE" USING WS-MESSAGE
           ELSE
       *> Saving failed, so undo the in-memory add to keep the
       *> table and the file consistent.
               SUBTRACT 1 FROM WS-FREQ-COUNT
               MOVE "Connection request could not be sent. Please try again."
                   TO WS-MESSAGE
               CALL "WRITE-LINE" USING WS-MESSAGE
           END-IF.
           GOBACK.
       END PROGRAM FREQ-SEND-REQUEST.