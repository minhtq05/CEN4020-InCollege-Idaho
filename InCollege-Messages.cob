*>*******************************************************
      *> Epic #8 & #9 private messaging menu, sender, and viewer.
      *> Messages are limited to accepted connections, persisted in
      *> InCollege-Messages.txt, and filtered by recipient on view.
      *>*******************************************************
       >>SOURCE FORMAT FREE
       IDENTIFICATION DIVISION.
       PROGRAM-ID. MESSAGE-MENU.

       DATA DIVISION.
       WORKING-STORAGE SECTION.
       COPY "InCollege-Common.cpy".

       01  WS-MESSAGE                  PIC X(100).
       01  WS-LINE-TEXT                PIC X(100).
       01  WS-CHOICE                   PIC X(20).
       01  WS-BACK-FLAG                PIC X VALUE 'N'.
           88  BACK-REQUESTED                    VALUE 'Y'.

       PROCEDURE DIVISION.
       MESSAGE-MENU-START.
           MOVE 'N' TO WS-BACK-FLAG
           PERFORM UNTIL BACK-REQUESTED OR END-OF-INPUT
               MOVE "--- Messages Menu ---" TO WS-MESSAGE
               CALL "WRITE-LINE" USING WS-MESSAGE
               MOVE "1. Send a New Message" TO WS-MESSAGE
               CALL "WRITE-LINE" USING WS-MESSAGE
               MOVE "2. View My Messages" TO WS-MESSAGE
               CALL "WRITE-LINE" USING WS-MESSAGE
               MOVE "3. Back to Main Menu" TO WS-MESSAGE
               CALL "WRITE-LINE" USING WS-MESSAGE
               MOVE "Enter your choice:" TO WS-MESSAGE
               CALL "WRITE-LINE" USING WS-MESSAGE
               CALL "READ-LINE" USING WS-LINE-TEXT
               IF NOT END-OF-INPUT
                   MOVE WS-LINE-TEXT TO WS-CHOICE
                   EVALUATE FUNCTION TRIM(WS-CHOICE)
                       WHEN "1"
                           CALL "SEND-MESSAGE"
                       WHEN "2"
                           CALL "VIEW-MESSAGES"
                       WHEN "3"
                           MOVE 'Y' TO WS-BACK-FLAG
                       WHEN OTHER
                           MOVE "Invalid choice, please try again."
                               TO WS-MESSAGE
                           CALL "WRITE-LINE" USING WS-MESSAGE
                   END-EVALUATE
               END-IF
           END-PERFORM.
           GOBACK.
       END PROGRAM MESSAGE-MENU.

       IDENTIFICATION DIVISION.
       PROGRAM-ID. SEND-MESSAGE.

       ENVIRONMENT DIVISION.
       INPUT-OUTPUT SECTION.
       FILE-CONTROL.
           SELECT MESSAGE-FILE ASSIGN TO "InCollege-Messages.txt"
               ORGANIZATION IS LINE SEQUENTIAL
               FILE STATUS IS WS-MESSAGE-FILE-STATUS.

       DATA DIVISION.
       FILE SECTION.
       FD  MESSAGE-FILE.
       COPY "InCollege-MessageRecord.cpy".

       WORKING-STORAGE SECTION.
       COPY "InCollege-Common.cpy".
       COPY "InCollege-PendingRequestTable.cpy".

       01  WS-MESSAGE-FILE-STATUS      PIC X(2).
       01  WS-MESSAGE                  PIC X(100).
       01  WS-LINE-TEXT                PIC X(100).
       01  WS-RECIPIENT                PIC X(20).
       01  WS-CONTENT                  PIC X(250).
       01  WS-REQUEST-IDX              PIC 9(2).
       01  WS-CONNECTED                PIC X VALUE 'N'.
       01  WS-SAVE-OK                  PIC X VALUE 'N'.

       PROCEDURE DIVISION.
       SEND-MESSAGE-START.
           MOVE "Enter recipient's username (must be a connection):"
               TO WS-MESSAGE
           CALL "WRITE-LINE" USING WS-MESSAGE
           CALL "READ-LINE" USING WS-LINE-TEXT
           IF END-OF-INPUT
               GOBACK
           END-IF
           MOVE FUNCTION TRIM(WS-LINE-TEXT) TO WS-RECIPIENT

           PERFORM VALIDATE-CONNECTION
           IF WS-CONNECTED = 'N'
               MOVE "You can only message users you are connected with."
                   TO WS-MESSAGE
               CALL "WRITE-LINE" USING WS-MESSAGE
               GOBACK
           END-IF

           MOVE "Enter your message (max 200 chars):" TO WS-MESSAGE
           CALL "WRITE-LINE" USING WS-MESSAGE
           CALL "READ-LONG-LINE" USING WS-CONTENT
           IF END-OF-INPUT
               GOBACK
           END-IF
           IF WS-CONTENT = SPACES
               MOVE "Message cannot be empty." TO WS-MESSAGE
               CALL "WRITE-LINE" USING WS-MESSAGE
               GOBACK
           END-IF
           IF WS-CONTENT(201:50) NOT = SPACES
               MOVE "Message must be 200 characters or fewer."
                   TO WS-MESSAGE
               CALL "WRITE-LINE" USING WS-MESSAGE
               GOBACK
           END-IF

           PERFORM SAVE-MESSAGE
           IF WS-SAVE-OK = 'Y'
               MOVE SPACES TO WS-MESSAGE
               STRING "Message sent to " DELIMITED BY SIZE
                      FUNCTION TRIM(WS-RECIPIENT) DELIMITED BY SIZE
                      " successfully!" DELIMITED BY SIZE
                      INTO WS-MESSAGE
               END-STRING
           ELSE
               MOVE "Message could not be sent. Please try again."
                   TO WS-MESSAGE
           END-IF
           CALL "WRITE-LINE" USING WS-MESSAGE.
           GOBACK.

       VALIDATE-CONNECTION.
           MOVE 'N' TO WS-CONNECTED
           IF WS-FREQ-COUNT > 0
               PERFORM VARYING WS-REQUEST-IDX FROM 1 BY 1
                       UNTIL WS-REQUEST-IDX > WS-FREQ-COUNT
                   IF FUNCTION TRIM(WS-FREQ-STATUS(WS-REQUEST-IDX)) =
                      "ACCEPTED"
                       IF (FUNCTION TRIM(
                              WS-FREQ-SENDER(WS-REQUEST-IDX)) =
                           FUNCTION TRIM(WS-CURRENT-USERNAME)
                           AND FUNCTION TRIM(
                              WS-FREQ-RECEIVER(WS-REQUEST-IDX)) =
                           FUNCTION TRIM(WS-RECIPIENT))
                          OR (FUNCTION TRIM(
                              WS-FREQ-RECEIVER(WS-REQUEST-IDX)) =
                           FUNCTION TRIM(WS-CURRENT-USERNAME)
                           AND FUNCTION TRIM(
                              WS-FREQ-SENDER(WS-REQUEST-IDX)) =
                           FUNCTION TRIM(WS-RECIPIENT))
                           MOVE 'Y' TO WS-CONNECTED
                       END-IF
                   END-IF
               END-PERFORM
           END-IF.

       SAVE-MESSAGE.
           MOVE 'N' TO WS-SAVE-OK
           OPEN EXTEND MESSAGE-FILE
           IF WS-MESSAGE-FILE-STATUS = "35"
               OPEN OUTPUT MESSAGE-FILE
           END-IF
           IF WS-MESSAGE-FILE-STATUS = "00"
               MOVE WS-CURRENT-USERNAME TO MSG-REC-SENDER
               MOVE WS-RECIPIENT TO MSG-REC-RECIPIENT
               MOVE FUNCTION CURRENT-DATE TO MSG-REC-TIMESTAMP
               MOVE WS-CONTENT(1:200) TO MSG-REC-CONTENT
               WRITE MESSAGE-RECORD
               IF WS-MESSAGE-FILE-STATUS = "00"
                   MOVE 'Y' TO WS-SAVE-OK
               END-IF
               CLOSE MESSAGE-FILE
           END-IF.
       END PROGRAM SEND-MESSAGE.

       IDENTIFICATION DIVISION.
       PROGRAM-ID. VIEW-MESSAGES.

       ENVIRONMENT DIVISION.
       INPUT-OUTPUT SECTION.
       FILE-CONTROL.
           SELECT MESSAGE-FILE ASSIGN TO "InCollege-Messages.txt"
               ORGANIZATION IS LINE SEQUENTIAL
               FILE STATUS IS WS-MESSAGE-FILE-STATUS.

       DATA DIVISION.
       FILE SECTION.
       FD  MESSAGE-FILE.
       COPY "InCollege-MessageRecord.cpy".

       WORKING-STORAGE SECTION.
       COPY "InCollege-Common.cpy".

       01  WS-MESSAGE-FILE-STATUS      PIC X(2).
       01  WS-MESSAGE-EOF              PIC X VALUE 'N'.
           88  END-OF-MESSAGES                 VALUE 'Y'.
       01  WS-MSG-FOUND-COUNT          PIC 9(4) VALUE 0.
       01  WS-MESSAGE                  PIC X(100).
       01  WS-LONG-MESSAGE             PIC X(250).
       01  WS-FORMATTED-TIME           PIC X(25).

       PROCEDURE DIVISION.
       VIEW-MESSAGES-START.
           MOVE "--- Your Messages ---" TO WS-MESSAGE
           CALL "WRITE-LINE" USING WS-MESSAGE

           MOVE 'N' TO WS-MESSAGE-EOF
           MOVE 0 TO WS-MSG-FOUND-COUNT

           OPEN INPUT MESSAGE-FILE
           IF WS-MESSAGE-FILE-STATUS = "00"
               PERFORM UNTIL END-OF-MESSAGES
                   READ MESSAGE-FILE
                       AT END
                           MOVE 'Y' TO WS-MESSAGE-EOF
                       NOT AT END
                           IF FUNCTION TRIM(MSG-REC-RECIPIENT) =
                              FUNCTION TRIM(WS-CURRENT-USERNAME)
                               IF WS-MSG-FOUND-COUNT > 0
                                   MOVE "---" TO WS-MESSAGE
                                   CALL "WRITE-LINE" USING WS-MESSAGE
                               END-IF
                               ADD 1 TO WS-MSG-FOUND-COUNT
                               PERFORM DISPLAY-SINGLE-MESSAGE
                           END-IF
                   END-READ
               END-PERFORM
               CLOSE MESSAGE-FILE
           END-IF

           IF WS-MSG-FOUND-COUNT = 0
               MOVE "You have no messages at this time." TO WS-MESSAGE
               CALL "WRITE-LINE" USING WS-MESSAGE
           END-IF

           MOVE "---------------------" TO WS-MESSAGE
           CALL "WRITE-LINE" USING WS-MESSAGE
           GOBACK.

       DISPLAY-SINGLE-MESSAGE.
           MOVE SPACES TO WS-MESSAGE
           STRING "From: " DELIMITED BY SIZE
                  FUNCTION TRIM(MSG-REC-SENDER) DELIMITED BY SIZE
                  INTO WS-MESSAGE
           END-STRING
           CALL "WRITE-LINE" USING WS-MESSAGE

           MOVE SPACES TO WS-LONG-MESSAGE
           STRING "Message: " DELIMITED BY SIZE
                  FUNCTION TRIM(MSG-REC-CONTENT) DELIMITED BY SIZE
                  INTO WS-LONG-MESSAGE
           END-STRING
           CALL "WRITE-LONG-LINE" USING WS-LONG-MESSAGE

           IF FUNCTION TRIM(MSG-REC-TIMESTAMP) NOT = SPACES
               PERFORM FORMAT-TIMESTAMP
               MOVE SPACES TO WS-MESSAGE
               STRING "Sent: " DELIMITED BY SIZE
                      FUNCTION TRIM(WS-FORMATTED-TIME) DELIMITED BY SIZE
                      INTO WS-MESSAGE
               END-STRING
               CALL "WRITE-LINE" USING WS-MESSAGE
           END-IF.

       FORMAT-TIMESTAMP.
           MOVE SPACES TO WS-FORMATTED-TIME
           IF MSG-REC-TIMESTAMP(1:12) IS NUMERIC
               STRING MSG-REC-TIMESTAMP(1:4) DELIMITED BY SIZE
                      "-" DELIMITED BY SIZE
                      MSG-REC-TIMESTAMP(5:2) DELIMITED BY SIZE
                      "-" DELIMITED BY SIZE
                      MSG-REC-TIMESTAMP(7:2) DELIMITED BY SIZE
                      " " DELIMITED BY SIZE
                      MSG-REC-TIMESTAMP(9:2) DELIMITED BY SIZE
                      ":" DELIMITED BY SIZE
                      MSG-REC-TIMESTAMP(11:2) DELIMITED BY SIZE
                      INTO WS-FORMATTED-TIME
               END-STRING
           ELSE
               MOVE FUNCTION TRIM(MSG-REC-TIMESTAMP) TO WS-FORMATTED-TIME
           END-IF.
       END PROGRAM VIEW-MESSAGES.