      ******************************************************************
      * Author:
      * Date:
      * Purpose:
      * Tectonics: cobc
      ******************************************************************
       IDENTIFICATION DIVISION.
      *-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-
       PROGRAM-ID. "SRCMOD".
       ENVIRONMENT DIVISION.
      *-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-
       CONFIGURATION SECTION.
      *-----------------------
       INPUT-OUTPUT SECTION.
      *-----------------------
       DATA DIVISION.
      *-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-
       FILE SECTION.
      *-----------------------
       WORKING-STORAGE SECTION.
       01 WS-FIELDS.
           05 LFT         PIC 9(2).
           05 RGT         PIC 9(2).
           05 MID         PIC 9(2).

       01 WORK-STATUS PIC X(1) VALUE "N".
           88 NOT-FOUND            VALUE "N".
           88 FOUND                VALUE "F".

       01 PRG-STATUS  PIC X(1).
       88 STATUS-OK         VALUE "O".
       88 ERR               VALUE "E".
      *-----------------------
       LINKAGE SECTION.

       COPY SRCH-INT.

       PROCEDURE DIVISION USING INTF.
      *-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-
       S0  SECTION.
       S0A.

           PERFORM A0
           IF STATUS-OK
               PERFORM F0
           END-IF
           MOVE RETURN-CODE TO QUIT-CODE
           MOVE "ALL OK" TO QUIT-MESSAGE

           GOBACK.
       S0Z.
           EXIT.

       A0 SECTION.
       A0A.
           MOVE 1 TO RETURN-CODE
           SET STATUS-OK TO TRUE
           IF INP-CNP <= SPACE
               MOVE 3          TO RETURN-CODE
               SET ERR         TO TRUE
           END-IF
           .
       A0Z.
           EXIT.

       F0 SECTION.
       F0A.

           SET NOT-FOUND TO TRUE

           EVALUATE TRUE
           WHEN OP-SEQUENTIAL
               PERFORM F-SEARCH-SEQ
           WHEN OP-BINARY
               PERFORM F-SEARCH-BIN
           END-EVALUATE


      * TEST FOR MULTIPLE OCCURRENCES
           IF FOUND
               IF INP-CNP = CNP(MID + 1)
               AND MID = 1
                   MOVE 2 TO RETURN-CODE
                   GO TO F0Z
               END-IF

               IF INP-CNP = CNP(MID + 1)
               OR INP-CNP = CNP(MID - 1)
                   MOVE 2 TO RETURN-CODE
               END-IF
           END-IF
           .
       F0Z.
           EXIT.




       F-SEARCH-BIN SECTION.
       F-SEARCH-BINA.
           MOVE 1 TO LFT
           MOVE N TO RGT

           IF CNP(1) = INP-CNP
               MOVE 1                      TO MID
               STRING SURNAME(MID)  NAME(MID)
                      DELIMITED BY SIZE INTO OUT-SURNAME-NAME
               MOVE 0                      TO RETURN-CODE
               SET FOUND                   TO TRUE
           END-IF

           PERFORM TEST BEFORE UNTIL LFT >= RGT
               OR FOUND
               COMPUTE MID = (RGT + LFT)/2
               IF CNP(MID) = INP-CNP
                   STRING SURNAME(MID)  NAME(MID)
                      DELIMITED BY SIZE INTO OUT-SURNAME-NAME
                   MOVE 0 TO RETURN-CODE
                   SET FOUND TO TRUE
               ELSE
                   IF INP-CNP > CNP(MID)
                       COMPUTE LFT = MID + 1
                   ELSE
                       COMPUTE RGT = MID - 1
                   END-IF
               END-IF
           END-PERFORM
           .
       F-SEARCH-BINZ.
           EXIT.


       F-SEARCH-SEQ SECTION.
       F-SEARCH-SEQA.
      * TO BE DONE
       F-SEARCH-SEQZ.
           EXIT.

       END PROGRAM "SRCMOD".
