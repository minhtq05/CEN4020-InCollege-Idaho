*> Shared in-memory friend request table
       01  WS-FREQ-COUNT               PIC 9(2) EXTERNAL.

       01  WS-FREQ-TABLE               EXTERNAL.
           05  WS-FREQ-ENTRY OCCURS 50 TIMES.
               10  WS-FREQ-SENDER      PIC X(20).
               10  WS-FREQ-RECEIVER    PIC X(20).
               10  WS-FREQ-STATUS      PIC X(10).