      ******************************************************************
      * Author:
      * Date:
      * Purpose:
      * Tectonics: cobc
      ******************************************************************
       IDENTIFICATION DIVISION.
      *-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-
       PROGRAM-ID. CNPBAT.
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
           *> I USED COPYBOOK FOR JUD AND CNP-INTERFACE

           01 CNP-INTERFACE.
           COPY CNP-INT.
           COPY CNP-JUD.

           01 CURRENT-ACCTION.
               05 FLAGG                PIC 9(01).

           01 PRG-STATUS               PIC X.
               88 STATUS-OK            VALUE "O".
               88 ERR                  VALUE "E".

       PROCEDURE DIVISION.
      *-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-
       S0  SECTION.
       S0A.
           MOVE "1910504060596" TO CNP-VAL
           MOVE 0 TO FLAGG


           PERFORM A0
           IF STATUS-OK
             CALL "CNPMODULE" USING CNP-INTERFACE
             PERFORM Z0
           END-IF

           STOP RUN
           EXIT.
       S0Z.
           EXIT.

       A0  SECTION.
       A0A.
           SET STATUS-OK TO TRUE
           IF CNP-VAL <= SPACE
               SET ERR         TO TRUE
               MOVE 5 TO CNP-QUIT
               DISPLAY "CNP INVALID"
               PERFORM Z0
           END-IF
           .

       A0Z.

       Z0 SECTION.
       Z0A.

           EVALUATE CNP-QUIT
           WHEN 0
               DISPLAY "ALL OK"
               DISPLAY "SEX: " CNP-SEX
               DISPLAY CNP-DAT-ZI"/" CNP-DAT-LUNA "/" CNP-DAT-AN
               "(ZZ/LL/AAAA)"
               DISPLAY "JUD: " CNP-NUME-JUDET
               DISPLAY "CNP CIF CONTROL: " CNP-CIF-CTRL-CORECT

           WHEN 1
               DISPLAY "CIFRA PENTRU SEX INVALIDA!"
           WHEN 2
               DISPLAY "CIFRA DE CONTROL INVALIDA!"
               DISPLAY "CNP CIF CTRL CORECTA: " CNP-CIF-CTRL-CORECT
           WHEN 3
               DISPLAY "LUNA INVALIDA!"
           WHEN 4
               DISPLAY "COMBINATIA ZI/LUNA INVALIDA!"
           WHEN 5
               DISPLAY "CNP NOT NUMERIC!"
           WHEN 6
               DISPLAY "JUDET INEXISTENT!"
           WHEN 7
               DISPLAY "AN INVALID!"
           WHEN 8
               DISPLAY "ZI INVALIDA!"
           WHEN OTHER
               DISPLAY "PROGRAM FAILED SOME TIMES :D"
           END-EVALUATE
           STOP RUN
           .

       Z0Z.

           EXIT.
       END PROGRAM CNPBAT.
