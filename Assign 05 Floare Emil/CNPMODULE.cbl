      ******************************************************************
      * Author:
      * Date:
      * Purpose:
      * Tectonics: cobc
      ******************************************************************
       IDENTIFICATION DIVISION.
      *-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-
       PROGRAM-ID. "CNPMODULE".
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
           COPY CNP-JUD.

           01 JUD-COD-TAB REDEFINES JUD-COD.
               05 JD-LINIE OCCURS 52.
                   10 JD-COD           PIC 9(2).
                   10 JD-JUD-NUME      PIC X(30).
           01 WS-FILD.
               05 WS-COD-JUDET         PIC 9(2).
               05 WS-CTR-NMR           PIC 9(9).
               05 WS-CTR-NMR2          PIC 9(9).
               05 WS-CTR-NUMERIC       PIC 9(1).
           01 CURRENT-ACCTION.
               05 FLAGG                PIC 9(01).
               05 NUMBER-DIVIDER       PIC 9(01) VALUE 2.
               05 NUMBER-REST          PIC 9(02) VALUE 0.
               05 NUMBER-REZULT          PIC 9(02) VALUE 0.

           01 PRG-STATUS               PIC X.
               88 STATUS-OK            VALUE "O".
               88 ERR                  VALUE "E".


      *-----------------------
       LINKAGE SECTION.
           01 CNP-INTERFACE.
           COPY CNP-INT.

       PROCEDURE DIVISION USING CNP-INTERFACE.
      *-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-
       S0  SECTION.
       S0A.

           MOVE 0 TO CNP-QUIT

           PERFORM A0

           IF STATUS-OK
             PERFORM F0
           ELSE
             PERFORM Z0
           END-IF

           IF STATUS-OK
             PERFORM F-CNP-VAL-SEX
           ELSE
             PERFORM Z0
           END-IF

           IF STATUS-OK
             PERFORM F-CHECK-CNP-VAL-DATE-BIRTH
           ELSE
             PERFORM Z0
           END-IF

           IF STATUS-OK
             PERFORM F-CHECK-CNP-VAL-JUD
           ELSE
             PERFORM Z0
           END-IF

            IF STATUS-OK
             PERFORM F-COMPUTE-CIF-CTR
           ELSE
             PERFORM Z0
           END-IF
           PERFORM Z0
           .
       S0Z.
           GOBACK.

       A0  SECTION.
       A0A.
           *> VERIFY THAN THE FILD IS NOT EMPTY

           SET STATUS-OK TO TRUE
           IF CNP-VAL <= SPACE
               SET ERR         TO TRUE
               MOVE 5 TO CNP-QUIT
               DISPLAY "CNP INVALID"
               *> GO TO Z0A
           END-IF
           .

       A0Z.

           .



       F0  SECTION.
       F0A.
           *> VERIFY THAN THE FILD IS NUMERIC
           IF CNP-VAL NOT NUMERIC
               SET ERR TO TRUE
               MOVE 5 TO CNP-QUIT
               *> GO TO Z0A
           END-IF
           .

       F0Z.
           EXIT
           .


       F-CNP-VAL-SEX SECTION.
       F-CNP-VAL-SEX0A.
           *> VERIFY FIRST NUMBER- SEX
           *> SPECICATION IS ONLY CONCERNING ABOUT 1, 2 AND 5, 6
           *> IF THIS IS VALID, SETTING THE SEX M/F

           IF NOT (CNP-VAL-SEX = 1 OR CNP-VAL-SEX = 2 OR
             CNP-VAL-SEX = 5 OR CNP-VAL-SEX = 6)
             MOVE 1 TO CNP-QUIT
             SET ERR TO TRUE
           ELSE
             SET STATUS-OK TO TRUE
             DIVIDE CNP-VAL-SEX BY NUMBER-DIVIDER GIVING
             NUMBER-REZULT REMAINDER NUMBER-REST
             IF(NUMBER-REST = 0)
                   MOVE "F" TO CNP-SEX
                 ELSE
                   MOVE "M" TO CNP-SEX
                 END-IF
           END-IF
           .

       F-CHECK-CNP-VAL-SEX0Z.
           EXIT
           .


       F-CHECK-CNP-VAL-DATE-BIRTH SECTION.
       F-CHECK-CNP-VAL-DATE-BIRTHA.
           *> VERIFY DATE OF BIRTH
           *> FIRST YEAR, THEN MONTH AND AT LEAST THE DAY
           *> IF THIS IS VALID, SETTING THE DAY OF BIRT IN ZZ/LL/AAAA
           *> FORMAT

             IF NOT (CNP-VAL-AN-2 >= 0 AND CNP-VAL-AN-2 <= 99)
               MOVE 7 TO CNP-QUIT
               SET ERR TO TRUE
             ELSE
               EVALUATE CNP-VAL-SEX
                 WHEN 1
                 WHEN 2
                   MOVE 19 TO CNP-DAT-AN-FIRST
                 WHEN 5
                 WHEN 6
                   MOVE 20 TO CNP-DAT-AN-FIRST
               END-EVALUATE
               MOVE CNP-VAL-AN-2 TO CNP-DAT-AN-TWO
             END-IF
             .

       F-CHECK-CNP-VAL-DATE-BIRTH0A1.
           IF (CNP-VAL-LUNA >= 1 AND CNP-VAL-LUNA <= 12)
             MOVE CNP-VAL-LUNA TO CNP-DAT-LUNA

           ELSE
             MOVE 3 TO CNP-QUIT
             SET ERR TO TRUE
           END-IF
           .

       F-CHECK-CNP-VAL-DATE-BIRTH0A2.
             *> FIRST CHECK THAT YEAR IS BISECT
            *> if(( year % 4 == 0 && year % 100 != 0 ) || year % 400 = 0 )
           MOVE 400 TO NUMBER-DIVIDER
           MOVE 0 TO NUMBER-REST
           DIVIDE CNP-DAT-AN BY NUMBER-DIVIDER GIVING NUMBER-REZULT
           REMAINDER NUMBER-REST

           IF (CNP-VAL-LUNA = 2 AND CNP-VAL-ZI >= 28
                                               AND NUMBER-REST = 0)
             MOVE 8 TO CNP-QUIT
             SET ERR TO TRUE
           END-IF

           IF NOT (CNP-VAL-ZI >= 1 AND CNP-VAL-ZI <= 31)
             MOVE 8 TO CNP-QUIT
              SET ERR TO TRUE
           ELSE
             MOVE CNP-VAL-ZI TO CNP-DAT-ZI
           END-IF
             .

       F-CHECK-CNP-VAL-DATE-BIRTH0Z.
             EXIT
             .

       F-CHECK-CNP-VAL-JUD SECTION.
           *> VALIDATING AND SETTING THE COUNTRY NUMBER

       F-CHECK-CNP-VAL-JUD0A.
           IF (CNP-VAL-JUD = 51)
               COMPUTE CNP-VAL-JUD = 47
           END-IF
           IF  CNP-VAL-JUD = 52
             COMPUTE CNP-VAL-JUD = 48
           END-IF

           IF NOT(CNP-VAL-JUD >= 1 AND CNP-VAL-JUD <= 48)
             MOVE 6 TO CNP-QUIT
             SET ERR TO TRUE
            ELSE
             MOVE CNP-VAL-JUD TO JD-COD(CNP-VAL-JUD)
             MOVE JD-JUD-NUME(CNP-VAL-JUD) TO CNP-NUME-JUDET
           END-IF

             .
       F-CHECK-CNP-VAL-JUD0Z.
           EXIT
            .


       F-COMPUTE-CIF-CTR SECTION.
       F-COMPUTE-CIF-CTRA.
           *> COMPUTE THE CONTROL NUMBER AND CHECK THAT THE EXISTING
           *> ONES IS CORECT

           *> THERE IS WS-CTR-NMR2 THAT IS THE SUM OF MULTIPLY OF
           *> WS-CTR-NMR WITH EVRY NUMBER FROM THE LIST  "279146358279"
           *> AFTER THAT, THE SUM IS DIVIDED BY 11 AND THE REST IS CONSIDERING
           *> TO BE THE CNP VERIFICATION NUMBER
           *> THIS IS THE STANDARD PROCES TO VERIFY A CNP NUMBER

           MOVE 0 TO WS-CTR-NMR2
           MOVE 0 TO WS-CTR-NMR
           MOVE 0 TO WS-CTR-NUMERIC

           MULTIPLY CNP-VAL-SEX BY 2 GIVING  WS-CTR-NMR
           COMPUTE WS-CTR-NMR2 = WS-CTR-NMR2 +  WS-CTR-NMR

           MOVE CNP-VAL-AN-2(1:1) TO WS-CTR-NUMERIC
           MULTIPLY WS-CTR-NUMERIC BY 7 GIVING  WS-CTR-NMR
           COMPUTE WS-CTR-NMR2 = WS-CTR-NMR2 +  WS-CTR-NMR

           MOVE CNP-VAL-AN-2(2:1) TO WS-CTR-NUMERIC
           MULTIPLY WS-CTR-NUMERIC BY 9 GIVING  WS-CTR-NMR
           COMPUTE WS-CTR-NMR2 = WS-CTR-NMR2 +  WS-CTR-NMR

           MOVE CNP-VAL-LUNA(1:1) TO WS-CTR-NUMERIC
           MULTIPLY WS-CTR-NUMERIC BY 1 GIVING  WS-CTR-NMR
           COMPUTE WS-CTR-NMR2 = WS-CTR-NMR2 +  WS-CTR-NMR


           MOVE CNP-VAL-LUNA(2:1) TO WS-CTR-NUMERIC
           MULTIPLY WS-CTR-NUMERIC BY 4 GIVING  WS-CTR-NMR
           COMPUTE WS-CTR-NMR2 = WS-CTR-NMR2 +  WS-CTR-NMR


           MOVE CNP-VAL-ZI(1:1) TO WS-CTR-NUMERIC
           MULTIPLY WS-CTR-NUMERIC BY 6 GIVING  WS-CTR-NMR
           COMPUTE WS-CTR-NMR2 = WS-CTR-NMR2 +  WS-CTR-NMR


           MOVE CNP-VAL-ZI(2:1) TO WS-CTR-NUMERIC
           MULTIPLY WS-CTR-NUMERIC BY 3 GIVING  WS-CTR-NMR
           COMPUTE WS-CTR-NMR2 = WS-CTR-NMR2 +  WS-CTR-NMR


           MOVE CNP-VAL-JUD(1:1) TO WS-CTR-NUMERIC
           MULTIPLY WS-CTR-NUMERIC BY 5 GIVING  WS-CTR-NMR
           COMPUTE WS-CTR-NMR2 = WS-CTR-NMR2 +  WS-CTR-NMR


           MOVE CNP-VAL-JUD(2:1) TO WS-CTR-NUMERIC
           MULTIPLY WS-CTR-NUMERIC BY 8 GIVING  WS-CTR-NMR
           COMPUTE WS-CTR-NMR2 = WS-CTR-NMR2 +  WS-CTR-NMR

           MOVE CNP-VAL-3CIF(1:1) TO WS-CTR-NUMERIC
           MULTIPLY WS-CTR-NUMERIC BY 2 GIVING  WS-CTR-NMR
           COMPUTE WS-CTR-NMR2 = WS-CTR-NMR2 +  WS-CTR-NMR


           MOVE CNP-VAL-3CIF(2:1) TO WS-CTR-NUMERIC
           MULTIPLY WS-CTR-NUMERIC BY 7 GIVING  WS-CTR-NMR
           COMPUTE WS-CTR-NMR2 = WS-CTR-NMR2 +  WS-CTR-NMR


           MOVE CNP-VAL-3CIF(3:1) TO WS-CTR-NUMERIC
           MULTIPLY WS-CTR-NUMERIC BY 9 GIVING  WS-CTR-NMR
           COMPUTE WS-CTR-NMR2 = WS-CTR-NMR2 +  WS-CTR-NMR

           DIVIDE WS-CTR-NMR2 BY 11 GIVING
                 NUMBER-REZULT REMAINDER NUMBER-REST
           IF (NUMBER-REST = 10)
             MOVE 1 TO CNP-CIF-CTRL-CORECT
           END-IF
           MOVE NUMBER-REST TO CNP-CIF-CTRL-CORECT

           IF (CNP-VAL-CIF-CTRL NOT = CNP-CIF-CTRL-CORECT)
              MOVE 2 TO CNP-QUIT
              SET ERR TO TRUE
           END-IF

           .
           F-COMPUTE-CIF-CTRA0Z.
           EXIT
           .


       Z0 SECTION.
       Z0A.
           MOVE CNP-INTERFACE TO RETURN-CODE

           .

       Z0Z.
           EXIT.
       END PROGRAM "CNPMODULE".
