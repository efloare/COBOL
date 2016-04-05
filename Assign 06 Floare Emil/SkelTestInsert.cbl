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
        *> WE SHALL WRITE TO THIS FILE THE SQL STATEMENT WE NEED.
        *> E.G. "SELECT * FROM STUDENTI WHERE NUME = 'NUME3'".
        *> AFTER THAT, THE CURRENT BATCH WILL CALL THE JAR-INTERPRETER
           SELECT SQL-STATEMENT-FILE ASSIGN TO 'StatementSQLInput.txt'
           ORGANIZATION IS LINE SEQUENTIAL.
        *> OUTPUT FILE OF JAR, INPUT FOR THIS PGM, RELATIVE PATH!
           SELECT SQL-RESULTS ASSIGN TO 'SQLResults/SelectResult.txt'
           ORGANIZATION IS LINE SEQUENTIAL.
        *> OUTPUT OF JAR, INPUT FOR THIS PGM: SQL-STATUS OR ERROR?
           SELECT SQL-STATUS-FILE ASSIGN TO 'SQLResults\SQLSta.txt'
           ORGANIZATION IS LINE SEQUENTIAL.

        *> INPUT FROM ASSIGNMENT 5 (DUMMY)
           SELECT STUDENTI-FILE ASSIGN TO 'SampleOutputA5.txt'
           ORGANIZATION IS LINE SEQUENTIAL.
        *> TRY TO CLEAN DB BEFORE INSERTING?
           SELECT STUDENT-DLL ASSIGN  TO 'StudMySQLDDL.sql'.


      *-----------------------
       DATA DIVISION.
      *-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-
       FILE SECTION.
             *> IS   SET TO BE 2500 BECAUSE I WILL PUT IN SQL-STATEMENT
             *> THE CLEANING SCRIPT OF DB
        FD SQL-STATEMENT-FILE.
       01 SQL-STATEMENT-FILE-FD             PIC X(2500).

         FD SQL-RESULTS.
       01 SQL-RESULTS-FD                    PIC X(2500).

        FD SQL-STATUS-FILE.
       01 SQL-STATUS-FD                     PIC X(10).

        FD STUDENTI-FILE.
        *> LENGTH = 4 + 30 + 30 + 13 = 77 CHARS
       01 STUDENTI-FILE-FD                  PIC X(100).

        FD STUDENT-DLL RECORD IS VARYING IN SIZE
                              DEPENDING ON SQLSTATEMENT-LENGTH.
        01 SQLSTATEMENT                    PIC X(2500).


      *-----------------------
       WORKING-STORAGE SECTION.

       01 SQLSTATEMENT-LENGTH              PIC 9(36).

       01 CNP-INTERFACE.
           COPY CNP-INT.
       01 FIELDS-FOR-SQL.
       05 K-INVOKE-JAR                     PIC X(27) VALUE
               "java -jar JSqlAdapter.jar ".

       05 SQL-STATEMENT                    PIC X(4400).

        *> WS-DEFINITION FOR STUDENTI-FILE (INPUT)
        01 WS-STUD-FILE.
            05 WS-STUD-CLASA               PIC X(04).
            05 WS-STUD-NUME                PIC X(30).
            05 WS-STUD-PRENUME             PIC X(30).
            05 WS-STUD-CNP                 PIC X(16).


      *> WS-DEFINITION FOR SQL RESULT (CLASE)
        01 WS-SQL-RES-CLASE.
           05 WS-CLASE-CLASA               PIC X(04).

        *> HOST VARIABLE FOR TABLE STUDENTI
        01 H-STUDENTI.
          05  H-STUD-CNP                   PIC X(13).
          05  H-STUD-NUME                  PIC X(30).
          05  H-STUD-PRENUME               PIC X(30).
          05  H-STUD-CLASA                 PIC X(04).
          05  H-STUD-DATANASTERII          PIC X(08).
          05  H-STUD-SEX                   PIC X(01).

        *> HOST VARIABLE FOR TABLE CLASE
        01 H-CLASE.
           05  H-CLASE-CLASA                PIC X(04).

        01 OTHER-HOST-VARS.
           05 H-COUNT-CLASA                PIC 9.
           05 H-COUNT-STUDENT              PIC 9.

        01 PRG-STATUS                      PIC X(1).
           88 STATUS-OK                    VALUE "O".
           88 ERR                          VALUE "E".

        01 SCH-STUDENTI-CURS-1             PIC X(1).
           88 X-CUR1-OP                    VALUE "O".
           88 X-CUR1-FE                    VALUE "F".
           88 X-CUR1-CL                    VALUE "C".
           88 X-CUR1-END                   VALUE "E".

        01 SCH-STUD-FILE-STATUS            PIC X(1).
           88 X-STUDENTI-EOF               VALUE "E".
           88 X-STUDENTI-NEOF              VALUE "N".

        01 SQL-STATUS-VALUE                PIC X(4).


      *-----------------------
       PROCEDURE DIVISION.
      *-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-

       S0  SECTION.
       S0A.
           SET STATUS-OK TO TRUE

           PERFORM A0
           PERFORM B0
           DISPLAY "FIRST LINE OF FILE READ: " WS-STUD-FILE

           *>IF A CLEANING OF DB IS NEADED, UNCOMMENT "PERFORM F-CLEAN-DB"
           *> *****************
           *> PERFORM F-CLEAN-DB

           PERFORM UNTIL X-STUDENTI-EOF OR ERR
               PERFORM F0
               PERFORM B0
           END-PERFORM

           PERFORM Z0
           .
       S0Z.
           STOP RUN.

       A0 SECTION.
       A0A.
           OPEN INPUT STUDENTI-FILE
           SET X-STUDENTI-NEOF         TO TRUE
           .
       A0Z.
           EXIT.

       B0 SECTION.
       B0A.
           READ STUDENTI-FILE
           AT END
               SET X-STUDENTI-EOF      TO TRUE
           END-READ

           IF X-STUDENTI-NEOF
               MOVE STUDENTI-FILE-FD   TO WS-STUD-FILE

           END-IF
           .
       B0Z.
           EXIT.

       F0 SECTION.
       F0A.
           *> FIRST, CHECK IF CURRENT CLASA EXISTS IN DB. IF IT DOES NOT
           *> INSERT IT.
           IF STATUS-OK
               PERFORM F-PROCESS-CLASA
           END-IF

           IF STATUS-OK
               PERFORM F-PROCESS-STUDENT
           END-IF

           .
       F0Z.
           EXIT.

       F-CLEAN-DB SECTION.
       F-CLEAN-DBA.
           *> PREPARE SQL STATEMENT FOR INSERT
           *> COPY THE SCRIP IN SQL-STATEMENT

           OPEN INPUT STUDENT-DLL
           READ STUDENT-DLL
           MOVE SPACE                                  TO SQL-STATEMENT
           MOVE SQLSTATEMENT                           TO SQL-STATEMENT
           CLOSE STUDENT-DLL

           *> *> OR DO IT 'MANUAL'?
           *> MOVE SPACE              TO SQL-STATEMENT

           *> STRING " SET FOREIGN_KEY_CHECKS = 0;
             *> DROP SCHEMA IF EXISTS `STUDENTI` ;
             *> CREATE SCHEMA IF NOT EXISTS `STUDENTI` ;
             *> CREATE TABLE IF NOT EXISTS `STUDENTI`.`MATERIEPROFESOR` (
               *> `CNP` CHAR(13) NULL,
               *> `MATERIE` char(30) NULL,
               *> CONSTRAINT `PROFESORIMATERIEPROFESOR`
                 *> FOREIGN KEY (`CNP`)
                 *> REFERENCES `STUDENTI`.`PROFESORI` (`CNP`)
                 *> ON DELETE RESTRICT
                 *> ON UPDATE RESTRICT,
               *> CONSTRAINT `MATERIIMATERIEPROFESOR`
                 *> FOREIGN KEY (`MATERIE`)
                 *> REFERENCES `STUDENTI`.`MATERII` (`MATERIE`)
                 *> ON DELETE RESTRICT
                 *> ON UPDATE RESTRICT);
             *> CREATE TABLE IF NOT EXISTS `STUDENTI`.`PROFESORMATERIECLASA` (
               *> `CNP` CHAR(13) NULL,
               *> `MATERIE` CHAR(30) NULL,
               *> `CLASA` CHAR(4) NULL,
               *> CONSTRAINT `PROFESORIPROFESORMATERIECLASA`
                 *> FOREIGN KEY (`CNP`)
                 *> REFERENCES `STUDENTI`.`PROFESORI` (`CNP`)
                 *> ON DELETE RESTRICT
                 *> ON UPDATE RESTRICT,
               *> CONSTRAINT `CLASEPROFESORMATERIECLASA`
                 *> FOREIGN KEY (`CLASA`)
                 *> REFERENCES `STUDENTI`.`CLASE` (`CLASA`)
                 *> ON DELETE RESTRICT
                 *> ON UPDATE RESTRICT,
               *> CONSTRAINT `MATERIIPROFESORMATERIECLASA`
                 *> FOREIGN KEY (`MATERIE`)
                 *> REFERENCES `STUDENTI`.`MATERII` (`MATERIE`)
                 *> ON DELETE RESTRICT
                 *> ON UPDATE RESTRICT);
             *> CREATE TABLE IF NOT EXISTS `STUDENTI`.`MATERII` (
               *> `MATERIE` CHAR(30) NOT NULL,
               *> INDEX `MATERIIMATERIE` (`MATERIE` ASC),
               *> PRIMARY KEY (`MATERIE`));
             *> CREATE TABLE IF NOT EXISTS `STUDENTI`.`CLASE` (
               *> `CLASA` CHAR(4) NOT NULL,
               *> PRIMARY KEY (`CLASA`),
               *> INDEX `CLASECLASA` (`CLASA` ASC));
             *> CREATE TABLE IF NOT EXISTS `STUDENTI`.`NOTE` (
               *> `CNP` CHAR(13) NULL,
               *> `MATERIE` CHAR(30) NULL,
               *> `NOTA` CHAR(2) NULL,
               *> `DATANOTA` CHAR(8) NULL,
               *> CONSTRAINT `STUDENTINOTE`
                 *> FOREIGN KEY (`CNP`)
                 *> REFERENCES `STUDENTI`.`STUDENTI` (`CNP`)
                 *> ON DELETE RESTRICT
                 *> ON UPDATE RESTRICT,
               *> CONSTRAINT `MATERIINOTE`
                 *> FOREIGN KEY (`MATERIE`)
                 *> REFERENCES `STUDENTI`.`MATERII` (`MATERIE`)
                 *> ON DELETE RESTRICT
                 *> ON UPDATE RESTRICT);
             *> CREATE TABLE IF NOT EXISTS `STUDENTI`.`STUDENTI` (
               *> `CNP` CHAR(13) NOT NULL,
               *> `NUME` CHAR(30) NULL,
               *> `PRENUME` CHAR(30) NULL,
               *> `CLASA` CHAR(4) NULL,
               *> `DATANASTERII` CHAR(8) NULL,
               *> `SEX` CHAR(1) NULL,
               *> PRIMARY KEY (`CNP`),
               *> CONSTRAINT `CLASESTUDENTI`
                 *> FOREIGN KEY (`CLASA`)
                 *> REFERENCES `STUDENTI`.`CLASE` (`CLASA`)
                 *> ON DELETE RESTRICT
                 *> ON UPDATE RESTRICT);
             *> CREATE TABLE IF NOT EXISTS `STUDENTI`.`PROFESORI` (
               *> `CNP` CHAR(13) NOT NULL,
               *> `NUME` CHAR(30) NULL,
               *> `PRENUME` CHAR(30) NULL,
               *> `DATANASTERII` CHAR(8) NULL,
               *> `SEX` CHAR(1) NULL,
               *> INDEX `PROFESORICNP` (`CNP` ASC),
               *> INDEX `NUME` (`NUME` ASC),
               *> PRIMARY KEY (`CNP`));
             *> SET FOREIGN_KEY_CHECKS = 1; "
           *> DELIMITED BY SIZE INTO SQL-STATEMENT

           *> LET'S SEE HOW DOES THE STATEMENT LOOK.
           DISPLAY "SQL-INSERT-STATEMENT: " SQL-STATEMENT

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
               DISPLAY "COBOL SAYS: INSERTED SUCCESSFULY, CONGRATS!"
           ELSE
               *> SOMETHING WENT WRONG, BUT IT'S NOT A REASON TO STOP THE
               *> PROGRAM.
               DISPLAY "COBOL SAYS, SOMETHING WENT WRONG."
           END-IF
           .
       F-CLEAN-DB0A.Z.
           EXIT.


       F-PROCESS-CLASA SECTION.
       F-PROCESS-CLASAA.
           MOVE WS-STUD-CLASA          TO H-CLASE-CLASA
            PERFORM R-CLASE-SE-1
      *      MOVE 0 TO H-COUNT-CLASA

           IF H-COUNT-CLASA > 0
               GO TO F-PROCESS-CLASAZ
           ELSE
               *> INSERT INTO CLASE CURRENT CLASA
               PERFORM R-CLASE-IN-1
           END-IF
           .
       F-PROCESS-CLASAZ.
           EXIT.

       F-PROCESS-STUDENT SECTION.
       F-PROCESS-STUDENTA.
           *> WS-STUD-CNP
           *> -    Obtinere an, luna, zi nastere (tot via CNP-Validator Module);
           *> -    Determinare sex, in format alphabetic (i.e. M/F);
           *> -    Mutare valori din Working-Storage in variabile Host;


           MOVE WS-STUD-CNP(3:13)           TO CNP-VAL
           CALL "CNPMODULE" USING CNP-INTERFACE
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
           IF (CNP-QUIT = 0)
             MOVE CNP-VAL                TO H-STUD-CNP
             MOVE WS-STUD-NUME           TO H-STUD-NUME
             MOVE WS-STUD-PRENUME        TO H-STUD-PRENUME
             MOVE WS-STUD-CLASA          TO H-STUD-CLASA
             MOVE CNP-DAT-NASTERE        TO H-STUD-DATANASTERII
             MOVE CNP-SEX                TO H-STUD-SEX

             *>CHECK THE EXISTANCE OF STUDENT
             PERFORM R-STUDENT-SE-1

             *> IF EXIST, SKIP INSERT, ELSE INSERT
             IF H-COUNT-STUDENT > 0
               GO TO F-PROCESS-STUDENTZ
             ELSE
               PERFORM R-STUDENT-IN-1
             END-IF
             .

       F-PROCESS-STUDENTZ.
           EXIT.

       R-CLASE-SE-1 SECTION.
       R-CLASE-SE-1A.

           *> PREPARE SQL STATEMENT
           MOVE SPACE              TO SQL-STATEMENT
           STRING "SELECT COUNT(*) FROM CLASE WHERE CLASA = '"
                   H-CLASE-CLASA "'"
                   DELIMITED BY SIZE
           INTO SQL-STATEMENT
           *> LET'S SEE HOW DOES THE STATEMENT LOOK.
           DISPLAY SQL-STATEMENT

           OPEN OUTPUT SQL-STATEMENT-FILE
           WRITE SQL-STATEMENT-FILE-FD FROM SQL-STATEMENT
           CLOSE SQL-STATEMENT-FILE

           *> INVOKE THE JAR
           CALL "SYSTEM" USING K-INVOKE-JAR

           *> READ THE RESULT FROM FILE
           OPEN INPUT SQL-RESULTS
           READ SQL-RESULTS INTO H-COUNT-CLASA
           CLOSE SQL-RESULTS
           .
       R-CLASE-SE-1Z.
           EXIT.

       R-CLASE-IN-1 SECTION.
       R-CLASE-IN-1A.
            *> PREPARE SQL STATEMENT
           MOVE SPACE              TO SQL-STATEMENT
           STRING "INSERT INTO CLASE (CLASA) VALUES ('"
                   H-CLASE-CLASA "')"
                   DELIMITED BY SIZE
           INTO SQL-STATEMENT
           *> LET'S SEE HOW DOES THE STATEMENT LOOK.
           DISPLAY "SQL-INSERT-STATEMENT: " SQL-STATEMENT

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
               DISPLAY "COBOL SAYS: INSERTED SUCCESSFULY, CONGRATS!"
               DISPLAY "SQL-STATUS-VALUE = " SQL-STATUS-VALUE
           ELSE
               *> SOMETHING WENT WRONG, BUT IT'S NOT A REASON TO STOP THE
               *> PROGRAM.
               DISPLAY "COBOL SAYS, SOMETHING WENT WRONG."
           END-IF
           .
       R-CLASE-IN-1Z.
           EXIT.


       R-STUDENT-SE-1 SECTION.
       R-STUDENT-SE-1A.
           *> PREPARE SQL STATEMENT
           *> THIS SECTION IS A CLONE OF R-CLASE-SE-1A. IN REPO
           *> IS  SUPPOSED TO BE NOTHING
           *> SO, WILL WE TRY TO CLEAN DB WITH THAT SCRIPT
           *> (LOOK UP FOR SECTION "CLEAN-DB")

           *> IF THE SELECT IS RETURNING 1, THIS WILL BE WRITTEN IN FILE
           *> THE FILE WILL BE CHECKED BEFOR INSERTING A NEW DATA AND
           *> IF IS 1, THE PROCEDURE WILL BE SKIPED


           MOVE SPACE              TO SQL-STATEMENT
           STRING "SELECT COUNT(*) FROM STUDENTI WHERE CNP = '"
                   H-STUD-CNP "'"
                   DELIMITED BY SIZE
           INTO SQL-STATEMENT
           *> LET'S SEE HOW DOES THE STATEMENT LOOK.
           DISPLAY SQL-STATEMENT

           OPEN OUTPUT SQL-STATEMENT-FILE
           WRITE SQL-STATEMENT-FILE-FD FROM SQL-STATEMENT
           CLOSE SQL-STATEMENT-FILE

           *> INVOKE THE JAR
           CALL "SYSTEM" USING K-INVOKE-JAR

           *> READ THE RESULT FROM FILE
           OPEN INPUT SQL-RESULTS
           READ SQL-RESULTS INTO H-COUNT-STUDENT
           CLOSE SQL-RESULTS
           .
       R-CLASE-SE-1Z.
           EXIT.

       R-STUDENT-IN-1 SECTION.
       R-STUDENT-IN-1A.
           *> PREPARE SQL STATEMENT FOR INSERT
           *> -    Inserare in baza de date, utilizand jar-ul pus la dispozitie.
           MOVE SPACE              TO SQL-STATEMENT

           STRING "INSERT INTO STUDENTI(CNP, CLASA, NUME, PRENUME, "
           " DATANASTERII, SEX) VALUES ('"H-STUD-CNP"', "
           "'"H-STUD-CLASA"' "
           ", '"H-STUD-NUME"','"H-STUD-PRENUME"',"
           "'"H-STUD-DATANASTERII"',"
           "'"H-STUD-SEX"')"
           DELIMITED BY SIZE INTO SQL-STATEMENT

           *> LET'S SEE HOW DOES THE STATEMENT LOOK.
           DISPLAY "SQL-INSERT-STATEMENT: " SQL-STATEMENT

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
               DISPLAY "COBOL SAYS: INSERTED SUCCESSFULY, CONGRATS!"
           ELSE
               *> SOMETHING WENT WRONG, BUT IT'S NOT A REASON TO STOP THE
               *> PROGRAM.
               DISPLAY "COBOL SAYS, SOMETHING WENT WRONG."
           END-IF
           .
       R-CLASE-IN-1Z.
           EXIT.

       Z0 SECTION.
       Z0A.
           CLOSE STUDENTI-FILE
           .
       Z0Z.
           EXIT.

       END PROGRAM YOUR-PROGRAM-NAME.
