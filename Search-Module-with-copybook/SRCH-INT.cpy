       01  INTF.
           05 OP-CODE                      PIC X(1).
               88 OP-SEQUENTIAL            VALUE "S".
               88 OP-BINARY                VALUE "B".
           05 LIN  OCCURS 100.
               10  SURNAME                 PIC X(30).
               10  NAME                    PIC X(30).
               10  CNP                     PIC 9(13).
           05  N                           PIC 9(2).
           05  INP-CNP                     PIC X(13).
           05  OUT-SURNAME-NAME            PIC X(60).
		   05  QUIT-CODE                   PIC X(02).
		   05  QUIT-MESSAGE                PIC X(70).
		   
		   
		   