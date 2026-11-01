       01  WS-JOB-COUNT                PIC 9(2) EXTERNAL.
       01  WS-JOB-TABLE                EXTERNAL.
           05  WS-JOB-ENTRY OCCURS 20 TIMES.
               10  WS-JOB-ID           PIC X(10).
               10  WS-JOB-TITLE        PIC X(50).
               10  WS-JOB-EMPLOYER     PIC X(50).
               10  WS-JOB-LOCATION     PIC X(50).
               10  WS-JOB-DESCRIPTION  PIC X(200).
               10  WS-JOB-SALARY       PIC X(30).
       01  WS-APPLICATION-COUNT        PIC 9(3) EXTERNAL.
       01  WS-APPLICATION-TABLE        EXTERNAL.
           05  WS-APPLICATION-ENTRY OCCURS 100 TIMES.
               10  WS-APPLICATION-USERNAME PIC X(20).
               10  WS-APPLICATION-JOB-ID   PIC X(10).
