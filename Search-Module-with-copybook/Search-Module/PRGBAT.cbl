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
           FD ST-INPUT.
           01 ST-INPUT-FD              PIC X(103).

       WORKING-STORAGE SECTION.
      *-----------------------
       01  INP-SWITCH                  PIC X(1) VALUE "N".
           88  INP1-EOF                VALUE "E".
           88  INP1-NEOF               VALUE "N".

       COPY SRCH-INT.

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

           MOVE "1940917123777" TO INP-CNP
      *      MOVE SPACE TO INP-CNP
           SET OP-BINARY TO TRUE
           CALL "SRCMOD" USING INTF

           DISPLAY OUT-SURNAME-NAME
           .
       F0Z.
           EXIT.


       Z0 SECTION.
       Z0A.
           CLOSE ST-INPUT.
       Z0Z.
           EXIT.
       END PROGRAM YOUR-PROGRAM-NAME.
