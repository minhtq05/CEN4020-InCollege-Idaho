      *> Persistent private-message record.
       01  MESSAGE-RECORD.
           05  MSG-REC-SENDER          PIC X(20).
           05  MSG-REC-RECIPIENT       PIC X(20).
           05  MSG-REC-TIMESTAMP       PIC X(21).
           05  MSG-REC-CONTENT         PIC X(200).
