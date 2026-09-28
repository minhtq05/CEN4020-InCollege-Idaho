   *> Shared working-storage structure for in-memory request table
        01  WS-FREQ-TABLE.
            05  WS-FREQ-COUNT           PIC 9(2) VALUE 0.
            05  WS-FREQ-ENTRY OCCURS 50 TIMES.
                10  WS-FREQ-SENDER      PIC X(20).
                10  WS-FREQ-RECEIVER    PIC X(20).
                10  WS-FREQ-STATUS      PIC X(10).