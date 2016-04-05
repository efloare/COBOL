      ******************************************************************
      * Author:
      * Date:
      * Purpose:
      * Tectonics: cobc
      ******************************************************************
       IDENTIFICATION DIVISION.
      *-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-
       PROGRAM-ID.ANGRYPARENTS.
       ENVIRONMENT DIVISION.
      *-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-
       CONFIGURATION SECTION.
      *-----------------------
       INPUT-OUTPUT SECTION.
       FILE-CONTROL.
        *> WRITE TO THIS FILE THE SQL STATEMENT WE NEED FOR USING,
        *> AFTER THAT, THE CURRENT BATCH WILL CALL THE JAR-INTERPRETER
           SELECT SQL-STATEMENT-FILE ASSIGN TO 'StatementSQLInput.txt'
           ORGANIZATION IS LINE SEQUENTIAL.

        *> OUTPUT FILE OF JAR, INPUT FOR THIS PGM, RELATIVE PATH!
           SELECT SQL-RESULTS ASSIGN TO 'SQLResults\SelectResult.txt'
           ORGANIZATION IS LINE SEQUENTIAL.
        *> OUTPUT OF JAR, INPUT FOR THIS PGM: SQL-STATUS OR ERROR?
           SELECT SQL-STATUS-FILE ASSIGN TO 'SQLResults\SQLSta.txt'
           ORGANIZATION IS LINE SEQUENTIAL.

      *-----------------------
       DATA DIVISION.
      *-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-
       FILE SECTION.

        FD SQL-STATEMENT-FILE.
       01 SQL-STATEMENT-FILE-FD             PIC X(1500).

         FD SQL-RESULTS.
       01 SQL-RESULTS-FD                    PIC X(1500).

        FD SQL-STATUS-FILE.
       01 SQL-STATUS-FD                     PIC X(10).

      *-----------------------
       WORKING-STORAGE SECTION.

       01 FIELDS-FOR-SQL.
           05 K-INVOKE-JAR                     PIC X(27) VALUE
               "java -jar JSqlAdapter.jar ".

           05 SQL-STATEMENT                    PIC X(1500).

        *> IN THIS WS-DEFINITION WILL BE MOVED REZULTED SELECTION
        *> OF ANGRY PARRENT
       01 WS-STUD-STUD.
            05 WS-STUD-CLASA               PIC X(04).
            05 WS-STUD-NUME                PIC X(30).
            05 WS-STUD-PRENUME             PIC X(30).
            05 WS-STUD-CNP                 PIC X(13).
            05 WS-STUD-MATERIE             PIC X(32).

      *> COUNTERS
       01 COUNTER-VARS.
           05 COUNTER-SELECT               PIC 9(02).
           05 COUNTER-CLASA                PIC 9(02).
           05 COUNTER-MATERIE              PIC 9(03).
           05 COUNTER-STUDENT              PIC 9(04).
           05 COUNTER-MATERI               PIC 9(03).
           05 MATERII-STUDENT              PIC 9(02).

      *> IN THIS WS-DEFINITION WILL BE MOVED SQL INTEROGATION
       *> SQL WILL BE INVOKE TO PUT REQUESTED SELECT LINE BY LINE
        01 WS-SQL-RES-CLASE.
           05 CLASE-CLASA                 PIC X(04)  OCCURS 100 TIMES.
           05 STUDENTI-CLASA                         OCCURS 100 TIMES.
               10 STUDENTI-CNP            PIC 9(13).
               10 FILLER                  PIC X(02).
               10 STUDENTI-NUME           PIC X(30).
               10 STUDENTI-PRENUME        PIC X(20).
           05 STUDENT-MATERII                        OCCURS 300 TIMES.
               10 MATERIE                 PIC X(30).
               10 NOTA                    PIC 9(02).
           *> FOR EVERY NEW NAME OF MATERIE WILL PUT HERE
           05 NEW-MATERIE                 PIC X(30) OCCURS 300 TIMES.
           05 PROF.
             10 CNP-PROF                  PIC 9(13).
             10 PROF-TO-BLAME.
               15 PROF-TO-BLAME-NUME      PIC X(30).
               15 PROF-TO-BLAME-PRENUME   PIC X(30).

       *> USEFULL STUFF
       01 LENGTH-OF-TEXT.
         05 LENGTH-TEXT                   PIC 9(02).
         05 LENGTH-TEXT2                  PIC 9(02).
       01 FLAGG                           PIC 9(01).

       01 INP-SWITCH                       PIC X(1) VALUE "N".
           88 INP1-EOF                     VALUE "E".
           88 INP1-NEOF                    VALUE "N".


       01 SQL-STATUS-VALUE                PIC X(4).

      *-----------------------
       PROCEDURE DIVISION.
      *-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-
       S0  SECTION.
       S0A.
           PERFORM A0
           PERFORM B0
           PERFORM F-SELECT-CLASA
           PERFORM F-SELECT-STUDENT
           PERFORM F-SELECT-MATERIE
           PERFORM F-BLAME-PROF
           PERFORM Z0
           .
       S0Z.
           STOP RUN.

       A0 SECTION.
       A0A.
           *> OPEN INPUT FILE?
           .
       A0Z.
           EXIT.

       B0 SECTION.
       B0A.
         *> PERFORM READ
         *> SETTING VARIABLE TO 0
           .
       B0Z.
           EXIT.

       F-SELECT-CLASA SECTION.
       F-SELECT-CLASAA.
         *> FIRST : INTERROGATION OF DB ABOUT ALL "CLASE".
         *> PUTTING IN FILE LINE BY LINE

           *> PREPARE SQL STATEMENT
           MOVE SPACE                           TO SQL-STATEMENT
           STRING "SELECT * FROM CLASE"
           INTO SQL-STATEMENT

           OPEN OUTPUT SQL-STATEMENT-FILE
           WRITE SQL-STATEMENT-FILE-FD FROM SQL-STATEMENT
           CLOSE SQL-STATEMENT-FILE

           *> INVOKE THE JAR
           CALL "SYSTEM" USING K-INVOKE-JAR

           *> DID SOMETHING GO WRONG? LET'S CHECK THE SQLSTA FILE.
           *> SQL STATUS WAS WRITTEN TO THE FILE.
           OPEN INPUT SQL-STATUS-FILE
           READ SQL-STATUS-FILE INTO SQL-STATUS-VALUE
           CLOSE SQL-STATUS-FILE

           IF SQL-STATUS-VALUE = "0   "
               DISPLAY "COBOL SAYS: SELECTED SUCCESSFULY!"
           ELSE
               *> SOMETHING WENT WRONG WHIT SELECTION
               DISPLAY "COBOL SAYS: WE HAVE NOTHING "
               "TO DISPLAY?"
               DISPLAY "SQL SAY: " SQL-STATUS-VALUE
               DISPLAY "EXITING..."
               GO TO Z0
           END-IF
           .

       F-SELECT-CLASAB.
           *> READ THE RESULT OF INTEROGATION FROM FILE INTO CLASE
           *> LINE BY LINE
           *> COUNTING EVERY LINE,
           OPEN INPUT SQL-RESULTS
           MOVE 1                          TO COUNTER-CLASA
           SET INP1-NEOF                   TO TRUE
           PERFORM WITH TEST BEFORE UNTIL INP1-EOF
             READ SQL-RESULTS INTO CLASE-CLASA(COUNTER-CLASA)
              AT END
                   SET INP1-EOF            TO TRUE
             END-READ
             IF NOT INP1-EOF
              ADD 1                        TO COUNTER-CLASA *> COUNTING
             END-IF
           END-PERFORM

           SUBTRACT 1 FROM COUNTER-CLASA
           *> IS ONE EMPTY LINE IN FILE FINAL

           CLOSE SQL-RESULTS
           .
       F-SELECT-CLASAF.
           *> DISPLAY ALL "CLASE"
           DISPLAY "Alegeti clasa:"
           DISPLAY " OPTIUNE       CLASA"
           MOVE 1 TO COUNTER-SELECT
           PERFORM WITH TEST AFTER UNTIL COUNTER-SELECT > COUNTER-CLASA
             DISPLAY "    " COUNTER-SELECT "   -     "
                                             CLASE-CLASA(COUNTER-SELECT)
             ADD 1 TO COUNTER-SELECT
           END-PERFORM
         *> ACCEPT SELECTION
           ACCEPT COUNTER-SELECT

           IF NOT (COUNTER-SELECT > 0
                       AND COUNTER-SELECT <= COUNTER-CLASA)
                       OR (COUNTER-SELECT NOT NUMERIC)
               DISPLAY "INVALID SELECTION"
               GO TO  Z0
           END-IF

           MOVE CLASE-CLASA(COUNTER-SELECT)      TO WS-STUD-CLASA
           .
       F-SELECT-CLASA0Z.
           EXIT.

       F-SELECT-STUDENT SECTION.
           *> THIS SECTION WILL INTEROGATE DB FOR STUDENTS FROM SELECTED CLASE
       F-SELECT-STUDENTA.
         *> FIRST : INTERROGATION OF DB ABOUT ALL "STUDENT" FROM DESIERED CLASE.
         *> PUTTING IN FILE LINE BY LINE

           *> PREPARE SQL STATEMENT
           MOVE SPACE                           TO SQL-STATEMENT
           STRING "SELECT * FROM STUDENTI WHERE CLASA = "
            "'"WS-STUD-CLASA"' "
           INTO SQL-STATEMENT

           OPEN OUTPUT SQL-STATEMENT-FILE
           WRITE SQL-STATEMENT-FILE-FD FROM SQL-STATEMENT
           CLOSE SQL-STATEMENT-FILE

           *> INVOKE THE JAR
           CALL "SYSTEM" USING K-INVOKE-JAR

           *> DID SOMETHING GO WRONG? LET'S CHECK THE SQLSTA FILE.
           *> SQL STATUS WAS WRITTEN TO THE FILE.
           OPEN INPUT SQL-STATUS-FILE
           READ SQL-STATUS-FILE INTO SQL-STATUS-VALUE
           CLOSE SQL-STATUS-FILE

           IF SQL-STATUS-VALUE = "0   "
               DISPLAY "COBOL SAYS: SELECTED SUCCESSFULY!"
           ELSE
               *> SOMETHING WENT WRONG WHIT SELECTION
               DISPLAY "COBOL SAYS: WE HAVE NOTHING "
               "TO DISPLAY?"
               DISPLAY "SQL SAY: " SQL-STATUS-VALUE
               DISPLAY "EXITING..."
               GO TO Z0
           END-IF
           .

       F-SELECT-STUDENTB.
           *> READ THE RESULT OF INTEROGATION FROM FILE INTO CLASE
           *> LINE BY LINE
           *> COUNTING EVERY LINE
           OPEN INPUT SQL-RESULTS
           MOVE 1                          TO COUNTER-STUDENT
           SET INP1-NEOF                   TO TRUE

           PERFORM WITH TEST BEFORE UNTIL INP1-EOF
             READ SQL-RESULTS INTO STUDENTI-CLASA(COUNTER-STUDENT)
              AT END
                   SET INP1-EOF            TO TRUE
             END-READ
             IF NOT INP1-EOF
              ADD 1                        TO COUNTER-STUDENT *> COUNTING
             END-IF
           END-PERFORM

           SUBTRACT 1 FROM COUNTER-STUDENT
           *> IS ONE EMPTY LINE IN FILE FINAL

           CLOSE SQL-RESULTS
           .
       F-SELECT-CLASAF.
           *> DISPLAY ALL STUNDENTS FROM CLASA
           DISPLAY "Alegeti studentul:"
           DISPLAY "OPTIUNE    CNP        NUME        "
           "PRENUME"
           MOVE 1 TO COUNTER-SELECT
           PERFORM WITH TEST AFTER UNTIL
                                 COUNTER-SELECT > COUNTER-STUDENT
             *> REMOVE SPACES FROM WS? BUT NEW DISPLAY WILL PUT A '\N', A NEW LINE
             MOVE 12 TO LENGTH-TEXT
             DISPLAY " " COUNTER-SELECT " -   "
             STUDENTI-CNP(COUNTER-SELECT)
             "  "
             STUDENTI-NUME(COUNTER-SELECT)(1:LENGTH-TEXT)
             STUDENTI-PRENUME(COUNTER-SELECT)
             ADD 1 TO COUNTER-SELECT
           END-PERFORM
         *> ACCEPT SELECTION
           ACCEPT COUNTER-SELECT

           IF NOT (COUNTER-SELECT > 0
                       AND COUNTER-SELECT <= COUNTER-STUDENT)
                       OR (COUNTER-STUDENT NOT NUMERIC)
               DISPLAY "INVALID SELECTION"
               GO TO  Z0
           END-IF


           MOVE STUDENTI-CNP(COUNTER-SELECT)         TO WS-STUD-CNP
           MOVE STUDENTI-NUME(COUNTER-SELECT)        TO WS-STUD-NUME
           MOVE STUDENTI-PRENUME(COUNTER-SELECT)     TO WS-STUD-PRENUME
           .
       F-SELECT-CLASA0Z.
           EXIT.

       F-SELECT-MATERIE SECTION.
           *> THIS SECTION WILL INTEROGATE DB FOR
           *> "NOTE" ON "MATERII" < 5 FROM
           *> SELECTED STUDENT WHIT SELECTED CLASS

       F-SELECT-MATERIEA.
         *> FIRST : INTERROGATION OF DB ABOUT
         *> ALL "MATERII" WITH CORENSPONING NOTE
         *> FROM DESIERED CLASE AND DESIERED STUDENT BY CNP
         *> PUTTING IN FILE LINE BY LINE

           *> PREPARE SQL STATEMENT
           MOVE SPACE                           TO SQL-STATEMENT
           STRING "SELECT MATERIE, NOTA FROM NOTE "
           "WHERE CNP = '"WS-STUD-CNP"'  AND NOTA < 5"
           INTO SQL-STATEMENT

           OPEN OUTPUT SQL-STATEMENT-FILE
           WRITE SQL-STATEMENT-FILE-FD FROM SQL-STATEMENT
           CLOSE SQL-STATEMENT-FILE

           *> INVOKE THE JAR
           CALL "SYSTEM" USING K-INVOKE-JAR

           *> DID SOMETHING GO WRONG? LET'S CHECK THE SQLSTA FILE.
           *> SQL STATUS WAS WRITTEN TO THE FILE.
           OPEN INPUT SQL-STATUS-FILE
           READ SQL-STATUS-FILE INTO SQL-STATUS-VALUE
           CLOSE SQL-STATUS-FILE

           IF SQL-STATUS-VALUE = "0   "
               DISPLAY "COBOL SAYS: SELECTED SUCCESSFULY!"
           ELSE
               *> SOMETHING WENT WRONG WHIT SELECTION
               DISPLAY "COBOL SAYS: WE HAVE NOTHING "
               "TO DISPLAY?"
               DISPLAY "SQL SAY: " SQL-STATUS-VALUE
               DISPLAY "EXITING..."
               GO TO Z0
           END-IF
           .

       F-SELECT-MATERIEB.
           *> READ THE RESULT OF INTEROGATION FROM FILE INTO STUDENT-MATERII
           *> LINE BY LINE
           *> COUNTING EVERY LINE
           OPEN INPUT SQL-RESULTS
           MOVE 1                          TO COUNTER-MATERIE
           SET INP1-NEOF                   TO TRUE

           PERFORM WITH TEST BEFORE UNTIL INP1-EOF
             READ SQL-RESULTS INTO STUDENT-MATERII(COUNTER-MATERIE)
              AT END
                   SET INP1-EOF            TO TRUE
             END-READ
             IF NOT INP1-EOF
              ADD 1                        TO COUNTER-MATERIE *> COUNTING
             END-IF
           END-PERFORM

           SUBTRACT 1 FROM COUNTER-MATERIE
           *> IS ONE EMPTY LINE IN FILE FINAL

           CLOSE SQL-RESULTS
           .
       F-SELECT-MATERIEF.
           *> DISPLAY ALL "NOTE" < 5 FROM STUDENT
           DISPLAY "Alegeti materia: "
           DISPLAY "OPTIUNE   MATERIE     NOTA"
           MOVE 1                          TO COUNTER-SELECT
           MOVE 0                          TO MATERII-STUDENT
           MOVE 1                          TO FLAGG
           PERFORM WITH TEST AFTER UNTIL
                                 COUNTER-SELECT > COUNTER-MATERIE
             *> REMOVE SPACES FROM WS? BUT NEW DISPLAY WILL PUT A '\N', A NEW LINE
             MOVE 12 TO LENGTH-TEXT
             PERFORM E0          *> NEW "MATERIE" ? YES, THAN FLAGG = 1
             IF FLAGG = 1
               DISPLAY "  " MATERII-STUDENT "  -   "
                MATERIE(COUNTER-SELECT)(1:LENGTH-TEXT)
                NOTA(COUNTER-SELECT)
               ADD 1 TO COUNTER-SELECT
             ELSE
               DISPLAY "      -   "
                MATERIE(COUNTER-SELECT)(1:LENGTH-TEXT)
                NOTA(COUNTER-SELECT)
               ADD 1 TO COUNTER-SELECT
             END-IF

           END-PERFORM
         *> ACCEPT SELECTION
           ACCEPT COUNTER-SELECT

           IF NOT(COUNTER-SELECT <= MATERII-STUDENT)
                       OR (COUNTER-SELECT NOT NUMERIC)
               DISPLAY "INVALID SELECTION"
               GO TO  Z0
           END-IF

           MOVE NEW-MATERIE(COUNTER-SELECT)      TO WS-STUD-MATERIE
           .

       F-SELECT-MATERIEAZ.
           EXIT.

       F-BLAME-PROF SECTION.
       F-BLAME-PROFA.
         *> FIRST : INTERROGATION OF PROFESORIMATERIECLASA
         *> ABOUT CNP OF PROFESOR THAT HAVE
         *> SELECTED "MATERIE" AND SELECTED "CLASA".
         *> AFTER THAT, PUT IN FILE

           *> PREPARE SQL STATEMENT
           MOVE SPACE                           TO SQL-STATEMENT
           STRING "SELECT CNP FROM PROFESORMATERIECLASA WHERE "
           "MATERIE = '"WS-STUD-MATERIE"' AND CLASA = "
           "'"WS-STUD-CLASA"'"
           INTO SQL-STATEMENT

           OPEN OUTPUT SQL-STATEMENT-FILE
           WRITE SQL-STATEMENT-FILE-FD FROM SQL-STATEMENT
           CLOSE SQL-STATEMENT-FILE

           *> INVOKE THE JAR
           CALL "SYSTEM" USING K-INVOKE-JAR

           *> DID SOMETHING GO WRONG? LET'S CHECK THE SQLSTA FILE.
           *> SQL STATUS WAS WRITTEN TO THE FILE.
           OPEN INPUT SQL-STATUS-FILE
           READ SQL-STATUS-FILE INTO SQL-STATUS-VALUE
           CLOSE SQL-STATUS-FILE

           IF SQL-STATUS-VALUE = "0   "
               DISPLAY "COBOL SAYS: SELECTED SUCCESSFULY!"
           ELSE
               *> SOMETHING WENT WRONG WHIT SELECTION
               DISPLAY "COBOL SAYS: WE HAVE NOTHING "
               "TO DISPLAY?"
               DISPLAY "SQL SAY: " SQL-STATUS-VALUE
               DISPLAY "EXITING..."
               GO TO Z0
           END-IF
           .

       F-BLAME-PROFB.
           *> READ THE RESULT OF INTEROGATION FROM FILE INTO CNP-PROF
           *> JUST A LINE
           OPEN INPUT SQL-RESULTS
           READ SQL-RESULTS INTO CNP-PROF
           CLOSE SQL-RESULTS

           *> CALL AGAIN THE JAR TO PUT "NUME" "PRENUME" PROFESOR
           *> WITH FOUNDED CNP-PROF

           MOVE SPACE                           TO SQL-STATEMENT
           STRING "SELECT NUME, PRENUME FROM PROFESORI WHERE "
           "CNP = '"CNP-PROF"'"
           INTO SQL-STATEMENT

           OPEN OUTPUT SQL-STATEMENT-FILE
           WRITE SQL-STATEMENT-FILE-FD FROM SQL-STATEMENT
           CLOSE SQL-STATEMENT-FILE

           *> INVOKE THE JAR
           CALL "SYSTEM" USING K-INVOKE-JAR

           *> DID SOMETHING GO WRONG? LET'S CHECK THE SQLSTA FILE.
           *> SQL STATUS WAS WRITTEN TO THE FILE.
           OPEN INPUT SQL-STATUS-FILE
           READ SQL-STATUS-FILE INTO SQL-STATUS-VALUE
           CLOSE SQL-STATUS-FILE

           IF SQL-STATUS-VALUE = "0   "
               DISPLAY "COBOL SAYS: SELECTED SUCCESSFULY!"
           ELSE
               *> SOMETHING WENT WRONG WHIT SELECTION
               DISPLAY "COBOL SAYS: WE HAVE NOTHING "
               "TO DISPLAY?"
               DISPLAY "SQL SAY: " SQL-STATUS-VALUE
               DISPLAY "EXITING..."
               GO TO Z0
           END-IF

           *> DISPLAY PROF  TO BLAME
           OPEN INPUT SQL-RESULTS
           READ SQL-RESULTS INTO PROF-TO-BLAME
           CLOSE SQL-RESULTS
           .
       F-BLAME-PROFF.
           *> BLAME
           *> REMOVE SPACE BEFORE PRINTING
           MOVE 0                                TO LENGTH-TEXT
           PERFORM VARYING LENGTH-TEXT FROM 1 BY 1
           UNTIL LENGTH-TEXT > 30 OR
             PROF-TO-BLAME-NUME(LENGTH-TEXT:1) = SPACE
           END-PERFORM

           MOVE 0                                TO LENGTH-TEXT2
           PERFORM VARYING LENGTH-TEXT2 FROM 1 BY 1
           UNTIL LENGTH-TEXT2 > 30 OR
             PROF-TO-BLAME-PRENUME(LENGTH-TEXT2:1) = SPACE
           END-PERFORM
           SUBTRACT 1 FROM LENGTH-TEXT2 *> WITHOUT SPACE AFTER WORD

           DISPLAY "Stimate parinte, aveti toate motivele sa fiti"
           " suparat pe profesorul: "
           PROF-TO-BLAME-NUME(1:LENGTH-TEXT)
           PROF-TO-BLAME-PRENUME(1:LENGTH-TEXT2)
           "!"
           .

       F-BLAME-PROFZ.
           EXIT.

       E0  SECTION.
       E0A.
         *> DETECT IF "MATERIE" IS CHANGED
         *> IF IS CHENGED ADD 1 TO MATERII-STUDENT ADD NEW-MATERIE
         *>AND PUT FLAGG TO 1

           IF MATERIE(COUNTER-SELECT - 1) NOT = MATERIE(COUNTER-SELECT)
             ADD 1                                   TO MATERII-STUDENT
             MOVE MATERIE(COUNTER-SELECT)
                                       TO NEW-MATERIE(MATERII-STUDENT)
             MOVE 1                                  TO FLAGG
           ELSE
             MOVE 0                                  TO FLAGG
           END-IF
           .
       E0Z.
           EXIT.

       Z0  SECTION.
       Z0A.
           *> CLOSE FILE
           .
       Z0Z.
           EXIT.

       END PROGRAM ANGRYPARENTS.
