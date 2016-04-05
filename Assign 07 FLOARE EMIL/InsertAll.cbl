      ******************************************************************
      * Author:
      * Date:
      * Purpose:
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
        *> IN CASE THAT THE MODULE IS RETURNING 1 (MEANING ERROR),
        *> THE DATA THAT WILL CURRENT TRY TO PROCEED
        *> IS STORED IN THIS FILE
           SELECT LOG-FILE ASSIGN TO 'LogFileIfError.txt'
           ORGANIZATION IS LINE SEQUENTIAL.

           SELECT PROF-FILE ASSIGN TO 'Materii-Profi-Clase.csv'
           ORGANIZATION IS LINE SEQUENTIAL.

           SELECT NOTE-FILE ASSIGN TO 'Note.csv'
           ORGANIZATION IS LINE SEQUENTIAL.


      *-----------------------
       DATA DIVISION.
      *-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-
       FILE SECTION.
          FD LOG-FILE.
       01 LOG-FILE-FD                       PIC X(120).

          FD PROF-FILE.
       01 PROF-FILE-FD                     PIC X(120).

         FD NOTE-FILE.
       01 NOTE-FILE-FD                     PIC X(120).
      *-----------------------
       WORKING-STORAGE SECTION.

       COPY PROFI-MATERII-INFO.
       COPY NOTE-INFO.

       01 SCH-PROF-FILE-STATUS            PIC X(1).
           88 X-PROF-EOF                  VALUE "E".
           88 X-PROF-NEOF                 VALUE "N".

       01 SCH-NOTE-FILE-STATUS            PIC X(1).
           88 X-NOTE-EOF                  VALUE "E".
           88 X-NOTE-NEOF                 VALUE "N".

      *-----------------------
       PROCEDURE DIVISION.
      *-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-

       S0  SECTION.
       S0A.
           *> OPEN INPUT FILE : PROF AND NOTE
           PERFORM A0

           *> FIRST: INSERT PROF
           PERFORM UNTIL X-PROF-EOF
               PERFORM B-PROF
               PERFORM F-PROF
               IF (PROF-ERROR = 1)
                 DISPLAY "F-WRITE-STATUS-PROF!!!!!!!!!!!!!!!!!!!!!!!!!"
                 PERFORM F-WRITE-STATUS-PROF
               END-IF
           END-PERFORM

           *> INSERT NOTE
           PERFORM UNTIL X-NOTE-EOF
               PERFORM B-NOTE
               PERFORM F-NOTE
               IF (NOTE-ERROR = 1)
                 DISPLAY "F-WRITE-STATUS-NOTE!!!!!!!!!!!!!!!!!!!!!!!!!"
                 PERFORM F-WRITE-STATUS-NOTE
               END-IF
           END-PERFORM

           PERFORM Z0
           .
       S0Z.
           STOP RUN.

       A0 SECTION.
       A0A.
           *> OPEN INPUT FILE : PROF AND NOTE
           OPEN INPUT PROF-FILE
           SET X-PROF-NEOF         TO TRUE

           OPEN INPUT NOTE-FILE
           SET X-NOTE-NEOF         TO TRUE
           .
       A0Z.
           EXIT.

       B-PROF SECTION.
       B-PROF0A.
           READ PROF-FILE
           AT END
               SET X-PROF-EOF      TO TRUE
           END-READ

           .
       B-PROF0Z.
           EXIT.

       F-PROF SECTION.
       F-PROF0A.
           *> UNSTRING CSV FILE IN PROFI-MATERI-INFO
           UNSTRING PROF-FILE-FD DELIMITED BY ',' INTO
               MAT-CLASA
               MAT-NUME
               PROF-NUME
               PROF-PRENUME
               PROF-CNP
           END-UNSTRING
           *> PREPAIRE THE STATUS ERROR FOR CALLING MODULE
           MOVE 0 TO PROF-ERROR
           CALL "PROFMODULE"                 USING PROFI-MATERII-INFO
           .
       F-PROF0Z.
           EXIT.
       F-WRITE-STATUS-PROF SECTION.
       F-WRITE-STATUS-PROF0A.
             OPEN OUTPUT LOG-FILE
             WRITE LOG-FILE-FD               FROM PROFI-MATERII-INFO
             CLOSE LOG-FILE
             .
       F-WRITE-STATUS-PROF0Z.
           EXIT.
       B-NOTE SECTION.
       B-NOTE0A.
           READ NOTE-FILE
           AT END
               SET X-NOTE-EOF      TO TRUE
           END-READ

           .
       B-NOTE0Z.
           EXIT.

       F-NOTE SECTION.
       F-NOTE0A.
           *> UNSTRING CSV FILE IN PROFI-MATERI-INFO
           UNSTRING NOTE-FILE-FD DELIMITED BY ';' INTO
               NOTE-CNP-STUD
               NOTE-MATERIE
               NOTE-NOTA
               NOTE-DATA
               NOTE-CLASA
           END-UNSTRING
           *> PREPAIRE THE STATUS ERROR FOR CALLING MODULE
           MOVE 0 TO NOTE-ERROR
           CALL "NOTEMODULE"                 USING NOTE-INFO
           .
       F-NOTE0Z.
           EXIT.
       F-WRITE-STATUS-NOTE SECTION.
       F-WRITE-STATUS-NOTE0A.
             OPEN OUTPUT LOG-FILE
             WRITE LOG-FILE-FD               FROM NOTE-INFO
             CLOSE LOG-FILE
             .
       F-WRITE-STATUS-NOTE0Z.
           EXIT.

       Z0 SECTION.
       Z0A.
           CLOSE PROF-FILE
           CLOSE NOTE-FILE
           .
       Z0Z.
           EXIT.

       END PROGRAM YOUR-PROGRAM-NAME.
