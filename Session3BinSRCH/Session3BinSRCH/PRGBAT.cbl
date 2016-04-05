      ******************************************************************
      * Author:
      * Date:
      * Purpose:
      * Tectonics: cobc
      ******************************************************************
      * READ FROM FILE
      * FILL THE VECTOR
      * SET CNP VALUE
      * CALL MODULE
      * DISPLAY NAME RECEIVED FROM MODULE
       IDENTIFICATION DIVISION.
      *-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-
       PROGRAM-ID. YOUR-PROGRAM-NAME.
       ENVIRONMENT DIVISION.
      *-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-
       CONFIGURATION SECTION.
      *-----------------------
       INPUT-OUTPUT SECTION.
      *-----------------------
       FILE-CONTROL.
           SELECT ST-INPUT ASSIGN TO 'StGradesCobol.txt'
           ORGANIZATION IS LINE SEQUENTIAL.

       DATA DIVISION.
      *-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-
       FILE SECTION.
      *-----------------------
           *> COPY CNP-INT.
           FD ST-INPUT.
           01 ST-INPUT-FD              PIC X(103).

       WORKING-STORAGE SECTION.
      *-----------------------
       01  INP-SWITCH                  PIC X(1) VALUE "N".
           88  INP1-EOF                VALUE "E".
           88  INP1-NEOF               VALUE "N".

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

       PROCEDURE DIVISION.
      *-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-
       S0  SECTION.
       S0A.
           PERFORM A0
           PERFORM B0 VARYING N FROM 1 BY 1 UNTIL INP1-EOF
           PERFORM F0
           PERFORM Z0
           STOP RUN.
       S0Z.
           EXIT.

       A0  SECTION.
       A0A.

           OPEN INPUT ST-INPUT.
       A0Z.

       B0  SECTION.
       B0A.
           READ ST-INPUT                  INTO LIN (N)
           AT END
               SET INP1-EOF               TO TRUE
           END-READ.
       B0Z.

       F0  SECTION.
       F0A.

           MOVE "1940917123999" TO INP-CNP
           *> MOVE SPACE TO INP-CNP
           *> SET OP-BINARY TO TRUE
           *> CALL "SRCHMOD" USING INTF

           SET OP-SEQUENTIAL TO TRUE
           *> DISPLAY N
           CALL "SRCHMOD" USING INTF
           *> DISPLAY N

           EVALUATE RETURN-CODE
           WHEN 0
               DISPLAY OUT-SURNAME-NAME
           WHEN 1
               DISPLAY "NOT FOUND"
           WHEN 2
               DISPLAY "MULTIPLE OCCURRENCES"
           WHEN 3
               DISPLAY "EMPTY INPUT-CNP"
           WHEN OTHER
               DISPLAY "INVALID RETURN-CODE!"
           END-EVALUATE
           .
       F0Z.
           EXIT.


       Z0 SECTION.
       Z0A.
           CLOSE ST-INPUT.
       Z0Z.
           EXIT.
       END PROGRAM YOUR-PROGRAM-NAME.
