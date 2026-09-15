      *>*******************************************************
      *> InCollege-IO.cob
      *> Shared screen/file echo I/O, used by every other program.
      *> WRITE-LINE displays one line and writes it to OUTPUT-FILE.
      *> READ-LINE reads one line from INPUT-FILE, echoes it via
      *> WRITE-LINE, and sets the shared END-OF-INPUT flag.
      *>*******************************************************
       >>SOURCE FORMAT FREE
       IDENTIFICATION DIVISION.
       PROGRAM-ID. WRITE-LINE.

       ENVIRONMENT DIVISION.
       INPUT-OUTPUT SECTION.
       FILE-CONTROL.
           SELECT OUTPUT-FILE ASSIGN TO "InCollege-Output.txt"
               ORGANIZATION IS LINE SEQUENTIAL.

       DATA DIVISION.
       FILE SECTION.
       FD  OUTPUT-FILE.
       01  OUTPUT-RECORD               PIC X(100).

       WORKING-STORAGE SECTION.
       01  WS-FIRST-CALL               PIC X VALUE 'Y'.

       LINKAGE SECTION.
       01  LS-MESSAGE                  PIC X(100).

       PROCEDURE DIVISION USING LS-MESSAGE.
       WRITE-LINE-START.
           IF WS-FIRST-CALL = 'Y'
               OPEN OUTPUT OUTPUT-FILE
               MOVE 'N' TO WS-FIRST-CALL
           END-IF.
           DISPLAY FUNCTION TRIM(LS-MESSAGE).
           MOVE FUNCTION TRIM(LS-MESSAGE) TO OUTPUT-RECORD.
           WRITE OUTPUT-RECORD.
           GOBACK.

           ENTRY "WRITE-LINE-CLOSE".
           IF WS-FIRST-CALL = 'N'
               CLOSE OUTPUT-FILE
           END-IF.
           GOBACK.
       END PROGRAM WRITE-LINE.

       IDENTIFICATION DIVISION.
       PROGRAM-ID. READ-LINE.

       ENVIRONMENT DIVISION.
       INPUT-OUTPUT SECTION.
       FILE-CONTROL.
           SELECT INPUT-FILE ASSIGN TO "InCollege-Input.txt"
               ORGANIZATION IS LINE SEQUENTIAL.

       DATA DIVISION.
       FILE SECTION.
       FD  INPUT-FILE.
       01  INPUT-RECORD                PIC X(100).

       WORKING-STORAGE SECTION.
       COPY "InCollege-Common.cpy".

       01  WS-FIRST-CALL               PIC X VALUE 'Y'.
       01  WS-INPUT-LINE               PIC X(100).
       01  WS-ECHO-MSG                 PIC X(100).

       LINKAGE SECTION.
       01  LS-LINE-TEXT                PIC X(100).

       PROCEDURE DIVISION USING LS-LINE-TEXT.
       READ-LINE-START.
           IF WS-FIRST-CALL = 'Y'
               OPEN INPUT INPUT-FILE
               MOVE 'N' TO WS-FIRST-CALL
           END-IF.
           READ INPUT-FILE INTO WS-INPUT-LINE
               AT END
                   MOVE 'Y' TO WS-EOF-INPUT
                   MOVE SPACES TO WS-INPUT-LINE
               NOT AT END
                   MOVE FUNCTION TRIM(WS-INPUT-LINE) TO WS-ECHO-MSG
                   CALL "WRITE-LINE" USING WS-ECHO-MSG
           END-READ.
           MOVE WS-INPUT-LINE TO LS-LINE-TEXT.
           GOBACK.

           ENTRY "READ-LINE-CLOSE".
           IF WS-FIRST-CALL = 'N'
               CLOSE INPUT-FILE
           END-IF.
           GOBACK.
       END PROGRAM READ-LINE.
