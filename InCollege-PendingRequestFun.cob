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

       01  WS-FREQ-FILE-STATUS         PIC X(2).
       01  WS-FREQ-EOF                 PIC X VALUE 'N'.

       PROCEDURE DIVISION.
       FREQ-LOAD-START.
           INITIALIZE WS-FREQ-TABLE.
           MOVE 0 TO WS-FREQ-COUNT.
           MOVE 'N' TO WS-FREQ-EOF.
           OPEN INPUT FREQ-FILE.
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

      *>*******************************************************
      *> FREQ-SAVE
      *> Rewrites the entire WS-FREQ-TABLE back to
      *> InCollege-Requests.txt so accepted/rejected status
      *> updates persist across program restarts.
      *>*******************************************************
       IDENTIFICATION DIVISION.
       PROGRAM-ID. FREQ-SAVE.

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

       01  WS-FREQ-FILE-STATUS         PIC X(2).
       01  WS-IDX                      PIC 9(2).

       PROCEDURE DIVISION.
       FREQ-SAVE-START.
           OPEN OUTPUT FREQ-FILE.
           IF WS-FREQ-COUNT > 0
               PERFORM VARYING WS-IDX FROM 1 BY 1
                       UNTIL WS-IDX > WS-FREQ-COUNT
                   MOVE WS-FREQ-SENDER(WS-IDX)   TO PEND-REC-SENDER
                   MOVE WS-FREQ-RECEIVER(WS-IDX) TO PEND-REC-RECEIVER
                   MOVE WS-FREQ-STATUS(WS-IDX)   TO PEND-REC-STATUS
                   WRITE PENDING-REQUEST-RECORD
               END-PERFORM
           END-IF.
           CLOSE FREQ-FILE.
           GOBACK.
       END PROGRAM FREQ-SAVE.

      *>*******************************************************
      *> FREQ-VIEW-PENDING
      *> Implements the 4 Managing Requests Stories (Epic 5):
      *>   1. Accept a pending request -> status = "ACCEPTED"
      *>   2. Reject a pending request -> status = "REJECTED"
      *>   3. Notify user when request is accepted or rejected
      *>   4. Remove accepted/rejected requests from pending list
      *>*******************************************************
       IDENTIFICATION DIVISION.
       PROGRAM-ID. FREQ-VIEW-PENDING.

       DATA DIVISION.
       WORKING-STORAGE SECTION.
       COPY "InCollege-Common.cpy".
       COPY "InCollege-PendingRequestTable.cpy".

       01  WS-MESSAGE                  PIC X(100).
       01  WS-LINE-TEXT                PIC X(100).
       01  WS-DECISION-CHOICE          PIC X(20).
       01  WS-IDX                      PIC 9(2).
       01  WS-FOUND                    PIC X VALUE 'N'.
       01  WS-DECISION-DONE            PIC X VALUE 'N'.
           88  DECISION-MADE                 VALUE 'Y'.

       PROCEDURE DIVISION.
       FREQ-VIEW-START.
           MOVE "--- Pending Connection Requests ---" TO WS-MESSAGE.
           CALL "WRITE-LINE" USING WS-MESSAGE.

           MOVE 'N' TO WS-FOUND.
           IF WS-FREQ-COUNT > 0
               PERFORM VARYING WS-IDX FROM 1 BY 1
                       UNTIL WS-IDX > WS-FREQ-COUNT OR END-OF-INPUT
                   IF FUNCTION TRIM(WS-FREQ-RECEIVER(WS-IDX)) =
                      FUNCTION TRIM(WS-CURRENT-USERNAME)
                      AND FUNCTION TRIM(WS-FREQ-STATUS(WS-IDX)) = "PENDING"
                       MOVE 'Y' TO WS-FOUND
                       PERFORM PROMPT-ACCEPT-OR-REJECT
                   END-IF
               END-PERFORM
           END-IF.

           IF WS-FOUND = 'N'
               MOVE "You have no pending connection requests." TO WS-MESSAGE
               CALL "WRITE-LINE" USING WS-MESSAGE
           ELSE
      *>       Story 4: Save updated statuses so accepted/rejected
      *>       requests are permanently removed from the pending list.
               CALL "FREQ-SAVE"
           END-IF.
           GOBACK.

       PROMPT-ACCEPT-OR-REJECT SECTION.
       PROMPT-ACCEPT-OR-REJECT-START.
           MOVE 'N' TO WS-DECISION-DONE.
           PERFORM UNTIL DECISION-MADE OR END-OF-INPUT
               MOVE SPACES TO WS-MESSAGE
               STRING "Request from: " DELIMITED BY SIZE
                      FUNCTION TRIM(WS-FREQ-SENDER(WS-IDX))
                          DELIMITED BY SIZE
                      INTO WS-MESSAGE
               END-STRING
               CALL "WRITE-LINE" USING WS-MESSAGE

               MOVE "1. Accept" TO WS-MESSAGE
               CALL "WRITE-LINE" USING WS-MESSAGE
               MOVE "2. Reject" TO WS-MESSAGE
               CALL "WRITE-LINE" USING WS-MESSAGE

               MOVE SPACES TO WS-MESSAGE
               STRING "Enter your choice for " DELIMITED BY SIZE
                      FUNCTION TRIM(WS-FREQ-SENDER(WS-IDX))
                          DELIMITED BY SIZE
                      ":" DELIMITED BY SIZE
                      INTO WS-MESSAGE
               END-STRING
               CALL "WRITE-LINE" USING WS-MESSAGE

               CALL "READ-LINE" USING WS-LINE-TEXT
               IF NOT END-OF-INPUT
                   MOVE WS-LINE-TEXT TO WS-DECISION-CHOICE
                   EVALUATE FUNCTION TRIM(WS-DECISION-CHOICE)
                       WHEN "1"
      *>                   Story 1 & 4: Accept & remove from PENDING
                           MOVE "ACCEPTED" TO WS-FREQ-STATUS(WS-IDX)
                           MOVE 'Y' TO WS-DECISION-DONE

      *>                   Story 3: Notify user of acceptance
                           MOVE SPACES TO WS-MESSAGE
                           STRING "Connection request from "
                                      DELIMITED BY SIZE
                                  FUNCTION TRIM(WS-FREQ-SENDER(WS-IDX))
                                      DELIMITED BY SIZE
                                  " accepted!" DELIMITED BY SIZE
                                  INTO WS-MESSAGE
                           END-STRING
                           CALL "WRITE-LINE" USING WS-MESSAGE

                       WHEN "2"
      *>                   Story 2 & 4: Reject & remove from PENDING
                           MOVE "REJECTED" TO WS-FREQ-STATUS(WS-IDX)
                           MOVE 'Y' TO WS-DECISION-DONE

      *>                   Story 3: Notify user of rejection
                           MOVE SPACES TO WS-MESSAGE
                           STRING "Connection request from "
                                      DELIMITED BY SIZE
                                  FUNCTION TRIM(WS-FREQ-SENDER(WS-IDX))
                                      DELIMITED BY SIZE
                                  " rejected!" DELIMITED BY SIZE
                                  INTO WS-MESSAGE
                           END-STRING
                           CALL "WRITE-LINE" USING WS-MESSAGE

                       WHEN OTHER
                           MOVE "Invalid choice, please try again."
                               TO WS-MESSAGE
                           CALL "WRITE-LINE" USING WS-MESSAGE
                   END-EVALUATE
               END-IF
           END-PERFORM.
       END PROGRAM FREQ-VIEW-PENDING.

       IDENTIFICATION DIVISION.
       PROGRAM-ID. FREQ-SEND-REQUEST.
       *>*******************************************************
       *> FREQ-SEND-REQUEST
       *> Called as:  CALL "FREQ-SEND-REQUEST" USING <PIC X(20)>
       *>*******************************************************

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
       COPY "InCollege-Common.cpy".
       COPY "InCollege-AccountTable.cpy".
       COPY "InCollege-PendingRequestTable.cpy".

       01  WS-FREQ-FILE-STATUS         PIC X(2).
       01  WS-MESSAGE                  PIC X(100).
       01  WS-IDX                      PIC 9(2).
       01  WS-TARGET-EXISTS            PIC X VALUE 'N'.
       01  WS-ALREADY-CONNECTED        PIC X VALUE 'N'.
       01  WS-ALREADY-SENT             PIC X VALUE 'N'.
       01  WS-ALREADY-RECEIVED         PIC X VALUE 'N'.
       01  WS-WAS-REJECTED             PIC X VALUE 'N'.
       01  WS-SAVE-OK                  PIC X VALUE 'N'.

       LINKAGE SECTION.
       01  LS-TARGET-USERNAME          PIC X(20).

       PROCEDURE DIVISION USING LS-TARGET-USERNAME.
       FREQ-SEND-START.
           PERFORM CHECK-TARGET-EXISTS.
           PERFORM CHECK-EXISTING-REQUESTS.

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
               WHEN WS-WAS-REJECTED = 'Y'
                   MOVE "You cannot send a connection request to this user."
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

       CHECK-EXISTING-REQUESTS.
           MOVE 'N' TO WS-ALREADY-CONNECTED.
           MOVE 'N' TO WS-ALREADY-SENT.
           MOVE 'N' TO WS-ALREADY-RECEIVED.
           MOVE 'N' TO WS-WAS-REJECTED.
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
                       IF FUNCTION TRIM(WS-FREQ-STATUS(WS-IDX)) =
                          "REJECTED"
                           MOVE 'Y' TO WS-WAS-REJECTED
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
               SUBTRACT 1 FROM WS-FREQ-COUNT
               MOVE "Connection request could not be sent. Please try again."
                   TO WS-MESSAGE
               CALL "WRITE-LINE" USING WS-MESSAGE
           END-IF.
           GOBACK.
       END PROGRAM FREQ-SEND-REQUEST.