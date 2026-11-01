      *>*******************************************************
      *> InCollege-IO.cob
      *> Shared screen/file echo I/O, used by every other program.
      *> WRITE-LINE displays one line and writes it to OUTPUT-FILE.
      *> READ-LINE reads one line from INPUT-FILE, echoes it via
      *> WRITE-LINE, and sets the shared END-OF-INPUT flag.
      *> WRITE-LONG-LINE / READ-LONG-LINE do the same with a
      *> 250-character buffer, for fields longer than 100 (e.g. a
      *> 200-character job description).
      *> Each program's entries share ONE 250-char LINKAGE item:
      *> GnuCOBOL 3.1 nulls an entry's parameter if it is not the
      *> first LINKAGE item. The 100-char entries only touch
      *> (1:100), since their callers pass a 100-char field.
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
       01  OUTPUT-RECORD               PIC X(250).

       WORKING-STORAGE SECTION.
       01  WS-FIRST-CALL               PIC X VALUE 'Y'.
       01  WS-OUT-BUFFER               PIC X(250).

       LINKAGE SECTION.
       01  LS-MESSAGE                  PIC X(250).

       PROCEDURE DIVISION USING LS-MESSAGE.
       WRITE-LINE-START.
           MOVE LS-MESSAGE(1:100) TO WS-OUT-BUFFER.
           PERFORM EMIT-LINE.
           GOBACK.

           ENTRY "WRITE-LONG-LINE" USING LS-MESSAGE.
           MOVE LS-MESSAGE TO WS-OUT-BUFFER.
           PERFORM EMIT-LINE.
           GOBACK.

           ENTRY "WRITE-LINE-CLOSE".
           IF WS-FIRST-CALL = 'N'
               CLOSE OUTPUT-FILE
           END-IF.
           GOBACK.

       EMIT-LINE.
           IF WS-FIRST-CALL = 'Y'
               OPEN OUTPUT OUTPUT-FILE
               MOVE 'N' TO WS-FIRST-CALL
           END-IF.
           DISPLAY FUNCTION TRIM(WS-OUT-BUFFER TRAILING).
           MOVE FUNCTION TRIM(WS-OUT-BUFFER TRAILING) TO OUTPUT-RECORD.
           WRITE OUTPUT-RECORD.
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
       01  INPUT-RECORD                PIC X(250).

       WORKING-STORAGE SECTION.
       COPY "InCollege-Common.cpy".

       01  WS-FIRST-CALL               PIC X VALUE 'Y'.
       01  WS-INPUT-LINE               PIC X(250).
       01  WS-ECHO-MSG                 PIC X(100).
       01  WS-LONG-ECHO-MSG            PIC X(250).

       LINKAGE SECTION.
       01  LS-LINE-TEXT                PIC X(250).

       PROCEDURE DIVISION USING LS-LINE-TEXT.
       READ-LINE-START.
           PERFORM FETCH-LINE.
           IF NOT END-OF-INPUT
               MOVE FUNCTION TRIM(WS-INPUT-LINE) TO WS-ECHO-MSG
               CALL "WRITE-LINE" USING WS-ECHO-MSG
           END-IF.
           MOVE WS-INPUT-LINE(1:100) TO LS-LINE-TEXT(1:100).
           GOBACK.

           ENTRY "READ-LONG-LINE" USING LS-LINE-TEXT.
           PERFORM FETCH-LINE.
           IF NOT END-OF-INPUT
               MOVE FUNCTION TRIM(WS-INPUT-LINE) TO WS-LONG-ECHO-MSG
               CALL "WRITE-LONG-LINE" USING WS-LONG-ECHO-MSG
           END-IF.
           MOVE WS-INPUT-LINE TO LS-LINE-TEXT.
           GOBACK.

           ENTRY "READ-LINE-CLOSE".
           IF WS-FIRST-CALL = 'N'
               CLOSE INPUT-FILE
           END-IF.
           GOBACK.

       FETCH-LINE.
           IF WS-FIRST-CALL = 'Y'
               OPEN INPUT INPUT-FILE
               MOVE 'N' TO WS-FIRST-CALL
           END-IF.
           READ INPUT-FILE INTO WS-INPUT-LINE
               AT END
                   MOVE 'Y' TO WS-EOF-INPUT
                   MOVE SPACES TO WS-INPUT-LINE
           END-READ.
       END PROGRAM READ-LINE.
