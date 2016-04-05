      ******************************************************************
      * Author:
      * Date:
      * Purpose: PROGRAM CONFORM SPECIFICATIILOR, MODUL CORECTAT PENTRU AN BISECT
      * Tectonics: cobc
      ******************************************************************
       IDENTIFICATION DIVISION.
      *-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-
       PROGRAM-ID. YOUR-PROGRAM-NAME.
       ENVIRONMENT DIVISION.
      *-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-
       CONFIGURATION SECTION.
      *-----------------------
       INPUT-OUTPUT SECTION.
       FILE-CONTROL.
           SELECT ST-INPUT ASSIGN TO 'StudentsSQL.csv'
           ORGANIZATION IS LINE SEQUENTIAL.

           SELECT ST-OUTPUT ASSIGN TO 'Results.txt'
           ORGANIZATION IS LINE SEQUENTIAL.

      *-----------------------
       DATA DIVISION.
      *-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-
       FILE SECTION.
           FD ST-INPUT.
           01 ST-INPUT-FD                  PIC X(200).

           FD ST-OUTPUT.
           01 ST-OUTPUT-FD                 PIC X(200).

      *-----------------------
       WORKING-STORAGE SECTION.
       01 CNP-INTERFACE.
           COPY CNP-INT.
       01 STUDENT-GRADES-LINE              PIC X(200).
       01 STUDENT-GRADES.
        05 ST-CLASA                        PIC 9(04).
        05 ST-NUME                         PIC X(30).
        05 ST-PRENUME                      PIC X(30).
        05 ST-SEX                          PIC X(01).
        05 ST-CNP-SEX                      PIC X(01).
        05 ST-AN-LAST2                     PIC 9(02).
        05 ST-AN-FIRST2                    PIC 9(02).
        05 ST-AN-NASTERE                   PIC 9(04).
        05 ST-ZI-AN                        PIC 9(03).
        05 ST-YEAR-DAY-JULIAN              PIC 9(07).
        05 ST-YEAR-DATE-GREGORIAN          PIC 9(10).
        05 ST-DATE-MM                      PIC 9(2).
        05 ST-DATE-DD                      PIC 9(2).
        05 ST-DATE-YYYY                    PIC 9(4).
        05 ST-RANDOM-JUD                   PIC 9(2).
        05 ST-RANDOM-3CIF                  PIC 9(3).
        05 ST-CIF-CONTROL                  PIC 9(1).

       01 WS-FILD.
        05 WS-CNP-FOR-MODULE               PIC 9(13).
        05 WS-NUMERIC-BUFF                 PIC 9(5).
        05 WS-NUMERIC-BUFF-OF-WORD         PIC 9(5).
        05 WS-NUMERIC-BUFF2                PIC 9(5).

       01 OUT-F-DATA.
           05 OUT-F-CLASA                  PIC X(4).
           05 OUT-F-NUME                   PIC X(30).
           05 OUT-F-PRENUME                PIC X(30).
           05 OUT-F-CNP                    PIC X(13).

       01 INP-SWITCH                       PIC X(1) VALUE "N".
           88 INP1-EOF                     VALUE "E".
           88 INP1-NEOF                    VALUE "N".

      *-----------------------
       PROCEDURE DIVISION.
      *-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-
       S0 SECTION.
       S0A.
           DISPLAY "BEGINNING PROGRAM".
           PERFORM A0
           PERFORM WITH TEST AFTER UNTIL INP1-EOF
               PERFORM B0
               PERFORM C0
               PERFORM E0
               PERFORM F-COMPUTE-CNP
               PERFORM F-CALL-MODULE
               PERFORM F-SET-OUTPUT
           END-PERFORM
           PERFORM Z0
           STOP RUN.
       S0Z.
           EXIT.

       A0 SECTION.
       A0A.
           OPEN INPUT ST-INPUT
           OPEN OUTPUT ST-OUTPUT
           .
       A0Z.
           EXIT.

      *    Read one line from the file and write it into the declared
      *    data
       B0 SECTION.
       B0A.
           MOVE SPACE                      TO STUDENT-GRADES-LINE
           READ ST-INPUT INTO STUDENT-GRADES-LINE

           AT END
               SET INP1-EOF                TO TRUE
           END-READ
           .
       B0Z.
           EXIT.

       C0 SECTION.
       *> THIS SECTION WILL MOVE SPACE IN FILDS THAT WILL BE USE

       C0A.
           MOVE 0                          TO ST-CLASA
           MOVE SPACE                      TO ST-NUME
           MOVE SPACE                      TO ST-PRENUME
           MOVE SPACE                      TO ST-SEX
           MOVE 0                          TO ST-CNP-SEX
           MOVE 0                          TO ST-AN-LAST2
           MOVE 0                          TO ST-AN-FIRST2
           MOVE 0                          TO ST-AN-NASTERE
           MOVE 0                          TO ST-ZI-AN
           MOVE 0                          TO ST-YEAR-DAY-JULIAN
           MOVE 0                          TO ST-YEAR-DATE-GREGORIAN
           MOVE 0                          TO ST-RANDOM-JUD
           MOVE 0                          TO ST-RANDOM-3CIF
           MOVE 0                          TO ST-CIF-CONTROL
           .
       C0Z.
         EXIT.

       E0 SECTION.
       *> WE WILL TAKE CHARACTER ONE BY ONE  FROM A LINE TO STORAGE
           *> CORESPONDENT DATA IN OUR DISERED STATEMENT.
           *> THE "," WILL "INCREMENT" OURS CORESPONDENT DATA
       E0A.

           IF NOT INP1-EOF

           MOVE 1                         TO WS-NUMERIC-BUFF
           MOVE 1                         TO WS-NUMERIC-BUFF-OF-WORD

           *> CLASA
           PERFORM UNTIL
             STUDENT-GRADES-LINE(WS-NUMERIC-BUFF:1) = ","
             MOVE STUDENT-GRADES-LINE(WS-NUMERIC-BUFF:1) TO
                                   ST-CLASA(WS-NUMERIC-BUFF-OF-WORD:1)
             ADD 1 TO WS-NUMERIC-BUFF
             ADD 1 TO WS-NUMERIC-BUFF-OF-WORD
             IF (STUDENT-GRADES-LINE(WS-NUMERIC-BUFF:1) = ","
               AND WS-NUMERIC-BUFF-OF-WORD = 4)
               MOVE 0 TO ST-CLASA(1:1)
               MOVE STUDENT-GRADES-LINE(1:3) TO ST-CLASA(2:3)
             END-IF
           END-PERFORM

           MOVE 1 TO WS-NUMERIC-BUFF-OF-WORD
           ADD 1 TO WS-NUMERIC-BUFF

           *> NUME
           PERFORM UNTIL
             STUDENT-GRADES-LINE(WS-NUMERIC-BUFF:1)= ","
             MOVE STUDENT-GRADES-LINE(WS-NUMERIC-BUFF:1) TO
                                   ST-NUME(WS-NUMERIC-BUFF-OF-WORD:1)
             ADD 1 TO WS-NUMERIC-BUFF
             ADD 1 TO WS-NUMERIC-BUFF-OF-WORD
             IF STUDENT-GRADES-LINE(WS-NUMERIC-BUFF:1)= ","
               MOVE 1 TO WS-NUMERIC-BUFF-OF-WORD
             END-IF
           END-PERFORM

           MOVE 1 TO WS-NUMERIC-BUFF-OF-WORD
           ADD 1 TO WS-NUMERIC-BUFF

           *> PRENUME
           PERFORM UNTIL
             STUDENT-GRADES-LINE(WS-NUMERIC-BUFF:1)= ","
             MOVE STUDENT-GRADES-LINE(WS-NUMERIC-BUFF:1) TO
                                   ST-PRENUME(WS-NUMERIC-BUFF-OF-WORD:1)
               IF STUDENT-GRADES-LINE(WS-NUMERIC-BUFF:1)= ","
               MOVE 1 TO WS-NUMERIC-BUFF-OF-WORD
             END-IF
             ADD 1 TO WS-NUMERIC-BUFF
             ADD 1 TO WS-NUMERIC-BUFF-OF-WORD
           END-PERFORM

           MOVE 1 TO WS-NUMERIC-BUFF-OF-WORD
           ADD 1 TO WS-NUMERIC-BUFF

           *> SEX - LITERAL
           PERFORM UNTIL STUDENT-GRADES-LINE(WS-NUMERIC-BUFF:1)= ","
             MOVE STUDENT-GRADES-LINE(WS-NUMERIC-BUFF:1) TO
                                   ST-SEX(WS-NUMERIC-BUFF-OF-WORD:1)
               IF STUDENT-GRADES-LINE(WS-NUMERIC-BUFF:1)= ","
               MOVE 0 TO WS-NUMERIC-BUFF-OF-WORD
             END-IF
             ADD 1 TO WS-NUMERIC-BUFF
             ADD 1 TO WS-NUMERIC-BUFF-OF-WORD

           END-PERFORM

           MOVE 1 TO WS-NUMERIC-BUFF-OF-WORD
           ADD 1 TO WS-NUMERIC-BUFF

           *> SEX NUMBER
           PERFORM UNTIL STUDENT-GRADES-LINE(WS-NUMERIC-BUFF:1)= ","
             MOVE STUDENT-GRADES-LINE(WS-NUMERIC-BUFF:1) TO
                               ST-CNP-SEX(WS-NUMERIC-BUFF-OF-WORD:1)
             ADD 1 TO WS-NUMERIC-BUFF
             ADD 1 TO WS-NUMERIC-BUFF-OF-WORD
             IF STUDENT-GRADES-LINE(WS-NUMERIC-BUFF:1)= ","
               MOVE 1 TO WS-NUMERIC-BUFF-OF-WORD
             END-IF
           END-PERFORM

           MOVE 1 TO WS-NUMERIC-BUFF-OF-WORD
           ADD 1 TO WS-NUMERIC-BUFF

           *> AN LAST 2 CIF
           PERFORM UNTIL STUDENT-GRADES-LINE(WS-NUMERIC-BUFF:1)= ","
             MOVE STUDENT-GRADES-LINE(WS-NUMERIC-BUFF:1) TO
                               ST-AN-LAST2(WS-NUMERIC-BUFF-OF-WORD:1)
             ADD 1 TO WS-NUMERIC-BUFF
             ADD 1 TO WS-NUMERIC-BUFF-OF-WORD
             IF STUDENT-GRADES-LINE(WS-NUMERIC-BUFF:1)= ","
               MOVE 1 TO WS-NUMERIC-BUFF-OF-WORD
             END-IF
           END-PERFORM

           MOVE 1 TO WS-NUMERIC-BUFF-OF-WORD
           ADD 1 TO WS-NUMERIC-BUFF

           *> AN FIRST 2 CIF
           PERFORM UNTIL STUDENT-GRADES-LINE(WS-NUMERIC-BUFF:1)= ","
             MOVE STUDENT-GRADES-LINE(WS-NUMERIC-BUFF:1) TO
                               ST-AN-FIRST2(WS-NUMERIC-BUFF-OF-WORD:1)
             ADD 1 TO WS-NUMERIC-BUFF
             ADD 1 TO WS-NUMERIC-BUFF-OF-WORD
             IF STUDENT-GRADES-LINE(WS-NUMERIC-BUFF:1)= ","
               MOVE 1 TO WS-NUMERIC-BUFF-OF-WORD
             END-IF
           END-PERFORM

           MOVE 1 TO WS-NUMERIC-BUFF-OF-WORD
           ADD 1 TO WS-NUMERIC-BUFF

           *> AN NASTERE COMPLET
           PERFORM UNTIL STUDENT-GRADES-LINE(WS-NUMERIC-BUFF:1)= ","
             MOVE STUDENT-GRADES-LINE(WS-NUMERIC-BUFF:1) TO
                               ST-AN-NASTERE(WS-NUMERIC-BUFF-OF-WORD:1)
             ADD 1 TO WS-NUMERIC-BUFF
             ADD 1 TO WS-NUMERIC-BUFF-OF-WORD
             IF STUDENT-GRADES-LINE(WS-NUMERIC-BUFF:1)= ","
               MOVE 1 TO WS-NUMERIC-BUFF-OF-WORD
             END-IF
           END-PERFORM

           MOVE 1 TO WS-NUMERIC-BUFF-OF-WORD
           ADD 1 TO WS-NUMERIC-BUFF

           *> ZI AN
           PERFORM UNTIL STUDENT-GRADES-LINE(WS-NUMERIC-BUFF:1)= ","
             MOVE STUDENT-GRADES-LINE(WS-NUMERIC-BUFF:1) TO
                               ST-ZI-AN(WS-NUMERIC-BUFF-OF-WORD:1)
             ADD 1 TO WS-NUMERIC-BUFF
             ADD 1 TO WS-NUMERIC-BUFF-OF-WORD
             IF STUDENT-GRADES-LINE(WS-NUMERIC-BUFF:1)= ","
               MOVE 1 TO WS-NUMERIC-BUFF-OF-WORD
             END-IF
           END-PERFORM

           MOVE 1 TO WS-NUMERIC-BUFF-OF-WORD
           ADD 1 TO WS-NUMERIC-BUFF

           *> AN + ZI JULIAN CALENDAR
           PERFORM UNTIL STUDENT-GRADES-LINE(WS-NUMERIC-BUFF:1)= ","
             MOVE STUDENT-GRADES-LINE(WS-NUMERIC-BUFF:1) TO
                         ST-YEAR-DAY-JULIAN(WS-NUMERIC-BUFF-OF-WORD:1)
             ADD 1 TO WS-NUMERIC-BUFF
             ADD 1 TO WS-NUMERIC-BUFF-OF-WORD
             IF STUDENT-GRADES-LINE(WS-NUMERIC-BUFF:1)= ","
               MOVE 1 TO WS-NUMERIC-BUFF-OF-WORD
             END-IF
           END-PERFORM

           MOVE 1 TO WS-NUMERIC-BUFF-OF-WORD
           ADD 1 TO WS-NUMERIC-BUFF
           MOVE 0 TO WS-NUMERIC-BUFF2

           *> MM/DD/YYYY GREGORIANS
           *> COMPUTE THE LENGTH OF MM/DD/YYYY
           COMPUTE WS-NUMERIC-BUFF-OF-WORD = WS-NUMERIC-BUFF
           PERFORM WITH TEST AFTER UNTIL STUDENT-GRADES-LINE
                                       (WS-NUMERIC-BUFF-OF-WORD:1) = ","
             ADD 1 TO WS-NUMERIC-BUFF-OF-WORD
             ADD 1 TO WS-NUMERIC-BUFF2
           END-PERFORM
           COMPUTE WS-NUMERIC-BUFF-OF-WORD = WS-NUMERIC-BUFF-OF-WORD - 1

           *> SET THE COMPUTET STRING IN SEPARATE DATA
             UNSTRING STUDENT-GRADES-LINE
             (WS-NUMERIC-BUFF:WS-NUMERIC-BUFF2) DELIMITED BY
             '/' INTO
               ST-DATE-MM
               ST-DATE-DD
               ST-DATE-YYYY
             END-UNSTRING

           PERFORM WITH TEST AFTER UNTIL STUDENT-GRADES-LINE
                                       (WS-NUMERIC-BUFF:1) = ","
               ADD 1 TO WS-NUMERIC-BUFF
           END-PERFORM

            *> COD JUDET
           MOVE 1 TO WS-NUMERIC-BUFF-OF-WORD
           ADD 1 TO WS-NUMERIC-BUFF

           PERFORM UNTIL
             STUDENT-GRADES-LINE(WS-NUMERIC-BUFF:1) = ","
             MOVE STUDENT-GRADES-LINE(WS-NUMERIC-BUFF:1) TO
                               ST-RANDOM-JUD(WS-NUMERIC-BUFF-OF-WORD:1)

             ADD 1 TO WS-NUMERIC-BUFF
             ADD 1 TO WS-NUMERIC-BUFF-OF-WORD
             IF (STUDENT-GRADES-LINE(WS-NUMERIC-BUFF:1) = ","
               AND WS-NUMERIC-BUFF-OF-WORD = 2)
               SUBTRACT 1 FROM WS-NUMERIC-BUFF
               MOVE 0 TO ST-RANDOM-JUD(1:1)
               MOVE STUDENT-GRADES-LINE(WS-NUMERIC-BUFF:1)
                                           TO ST-RANDOM-JUD(2:1)
               ADD 1 TO WS-NUMERIC-BUFF
             END-IF
           END-PERFORM

           *> CELE 3 CIFRE
           MOVE 1 TO WS-NUMERIC-BUFF-OF-WORD
           ADD 1 TO WS-NUMERIC-BUFF

           PERFORM UNTIL STUDENT-GRADES-LINE(WS-NUMERIC-BUFF:1)= SPACE
             MOVE STUDENT-GRADES-LINE(WS-NUMERIC-BUFF:1) TO
                      ST-RANDOM-3CIF(WS-NUMERIC-BUFF-OF-WORD:1)
             ADD 1 TO WS-NUMERIC-BUFF
             ADD 1 TO WS-NUMERIC-BUFF-OF-WORD
             IF STUDENT-GRADES-LINE(WS-NUMERIC-BUFF:1)= SPACE
               MOVE 1 TO WS-NUMERIC-BUFF-OF-WORD
             END-IF
           END-PERFORM

           END-IF
           .


       E0Z.
           EXIT.

       *> F0 SECTION.
       *> F0A.
          *> IN   CASE OF ERROR FOR DISPLAY
      *> *    PROCESSING THE LINE
           *> DISPLAY "ST-CLASA " ST-CLASA
           *> DISPLAY "ST-NUME "ST-NUME
           *> DISPLAY "ST-PRENUME "   ST-PRENUME
           *> DISPLAY "ST-SEX " ST-SEX
           *> DISPLAY "ST-CNP-SEX " ST-CNP-SEX
           *> DISPLAY  "ST-AN-LAST2 "  ST-AN-LAST2
           *> DISPLAY "ST-AN-FIRST2 " ST-AN-FIRST2
           *> DISPLAY "ST-AN-NASTERE " ST-AN-NASTERE
           *> DISPLAY "ST-ZI-AN " ST-ZI-AN
           *> DISPLAY "ST-DATE-YYYYYY " ST-DATE-YYYY
           *> DISPLAY "ST-YEAR-DAY-JULIAN " ST-YEAR-DAY-JULIAN
           *> DISPLAY "ST-YEAR-DATE-GREGORIAN " ST-YEAR-DATE-GREGORIAN
           *> DISPLAY "JUDET "       ST-RANDOM-JUD
           *> DISPLAY "3 CIFRE " ST-RANDOM-3CIF
           *> DISPLAY "CIF CONTROL " ST-CIF-CONTROL
           *> DISPLAY " =======" ST-DATE-MM " ===ST-DATE-MM"
           *> DISPLAY " =======" ST-DATE-DD " ===ST-DATE-DD"
           *> DISPLAY  "=====" ST-DATE-YYYY "=ST-DATE-YYYY"
           *> DISPLAY  "=====" ST-DATE-YYYY(3:2) "=ST-DATE-YYYY2:2"
           *> DISPLAY "NEEEEEXXXTTTTT"
          *> .
       *> F0Z.
           *> EXIT.
       F-COMPUTE-CNP SECTION.
       *> THIS SECTION WILL COMPUTE THE CNP FROM THE DATA PROCESED ABOVE

       F-COMPUTE-CNP0A.
           MOVE ST-CNP-SEX                   TO WS-CNP-FOR-MODULE(1:1)
           MOVE ST-DATE-YYYY(3:2)            TO WS-CNP-FOR-MODULE(2:2)
           MOVE ST-DATE-MM                   TO WS-CNP-FOR-MODULE(4:2)
           MOVE ST-DATE-DD                   TO WS-CNP-FOR-MODULE (6:2)
           MOVE ST-RANDOM-JUD                TO WS-CNP-FOR-MODULE(8:2)
           MOVE ST-RANDOM-3CIF               TO WS-CNP-FOR-MODULE(10:3)
           MOVE 0                            TO WS-CNP-FOR-MODULE(13:1)
           .
       F-COMPUTE-CNP0Z.
         EXIT.

       F-CALL-MODULE SECTION.
           DISPLAY "CALLING CNPMODULE FOR CIFRA-CTR WITH "
           WS-CNP-FOR-MODULE
           MOVE WS-CNP-FOR-MODULE            TO CNP-VAL
           CALL "CNPMODULE" USING CNP-VAL
           MOVE CNP-CIF-CTRL-CORECT          TO WS-CNP-FOR-MODULE(13:1)
           DISPLAY WS-CNP-FOR-MODULE " IS CORECT CNP, WITH CORECT "
           " CIFRA-CONTROL"
           " SO WRITTING THIS IN FILE..."
           .

       F-SET-OUTPUT SECTION.
       F-SET-OUTPUT0A.
           *> WRITE OUTPUT
           MOVE ST-CLASA                      TO OUT-F-CLASA
           MOVE ST-NUME                       TO OUT-F-NUME
           MOVE ST-PRENUME                    TO OUT-F-PRENUME
           MOVE WS-CNP-FOR-MODULE             TO OUT-F-CNP
           MOVE OUT-F-DATA TO ST-OUTPUT-FD
           WRITE ST-OUTPUT-FD
           .
       F-SET-OUTPUT0Z.
         EXIT.


      *    Close all I-O files
       Z0 SECTION.
       Z0A.
           CLOSE ST-INPUT
           CLOSE ST-OUTPUT
           .
       Z0Z.
           EXIT.
