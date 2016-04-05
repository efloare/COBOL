      ******************************************************************
      * Author:
      * Date:
      * Purpose:
      * Tectonics: cobc
      ******************************************************************
       IDENTIFICATION DIVISION.
      *-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-
       PROGRAM-ID. PROFMODULE.
       ENVIRONMENT DIVISION.
      *-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-
       CONFIGURATION SECTION.
      *-----------------------
       INPUT-OUTPUT SECTION.
       FILE-CONTROL.
           SELECT SQL-STATEMENT-FILE ASSIGN TO 'StatementSQLInput.txt'
           ORGANIZATION IS LINE SEQUENTIAL.
       DATA DIVISION.
      *-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-
       FILE SECTION.
        FD SQL-STATEMENT-FILE.
       01 SQL-STATEMENT-FILE-FD             PIC X(1000).

      *-----------------------
       WORKING-STORAGE SECTION.

       01 FIELDS-FOR-SQL.
       05 K-INVOKE-JAR                     PIC X(27) VALUE
               "java -jar JSqlAdapter.jar ".

       05 SQL-STATEMENT                    PIC X(300).
       01 CNP-INTERFACE.
           COPY CNP-INT.

      *-----------------------
       LINKAGE SECTION.
      **-*-*-*-*-*-*-*-*-*-*-*-*-*
       COPY PROFI-MATERII-INFO.

       PROCEDURE DIVISION USING PROFI-MATERII-INFO.
      *-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-
       S0  SECTION.
       S0A.
           MOVE PROF-CNP             TO CNP-VAL
           CALL "CNPMODULE"          USING CNP-VAL
           IF (CNP-QUIT = 0)
             *> FILL THE DATA-NASTERE AND SEX FROM CNPMODULE
             MOVE CNP-DAT-NASTERE    TO PROF-DATA-NASTERE
             MOVE CNP-SEX            TO PROF-SEX
             PERFORM F-INSERT-DB
           ELSE
             MOVE 1 TO PROF-ERROR
           END-IF
           PERFORM Z0
           .
       S0Z.
           GOBACK.
       F-INSERT-DB SECTION.
       F-INSERT-DB0A.
         *> PREPARE SQL STATEMENT FOR INSERT
         *> INSERT FOR PROFESORI FILD - THIS WILL BE FIRT BECAUSE
         *> INSERT FOR THIS IN NOT DEPENDING ON OTHER INSERT,
         *> WE JUST NEED TO HAVE CNP AND NAME
           MOVE SPACE              TO SQL-STATEMENT

           STRING "INSERT INTO PROFESORI(CNP, NUME, PRENUME, "
           " DATANASTERII, SEX) VALUES ('"PROF-CNP"', "
           "'"PROF-NUME"' "
           ", '"PROF-PRENUME"','"PROF-DATA-NASTERE"',"
           "'"PROF-SEX"')"
           DELIMITED BY SIZE INTO SQL-STATEMENT

           *> LET'S SEE HOW DOES THE STATEMENT LOOK.
           DISPLAY "SQL-INSERT-STATEMENT: " SQL-STATEMENT

           OPEN OUTPUT SQL-STATEMENT-FILE
           WRITE SQL-STATEMENT-FILE-FD FROM SQL-STATEMENT
           CLOSE SQL-STATEMENT-FILE

           *> INVOKE THE JAR
           CALL "SYSTEM" USING K-INVOKE-JAR

           *>INSERT MATERIE OF FROFESOR THAT WILL BE INSERTED AFTER
           *> DOING THIS WILL LET US INSERT PROFESORMATERIECLASA
           *> WHO HAVE FOREIGN KEY (`MATERIE`) REFERENCES `STUDENTI`.`MATERII` (`MATERIE`)
           MOVE SPACE              TO SQL-STATEMENT

           STRING "INSERT INTO MATERII (MATERIE) "
           "VALUES ('"MAT-NUME"')"
           DELIMITED BY SIZE INTO SQL-STATEMENT

           *> LET'S SEE HOW DOES THE STATEMENT LOOK.
           DISPLAY "SQL-INSERT-STATEMENT: " SQL-STATEMENT

           OPEN OUTPUT SQL-STATEMENT-FILE
           WRITE SQL-STATEMENT-FILE-FD FROM SQL-STATEMENT
           CLOSE SQL-STATEMENT-FILE

           *> INVOKE THE JAR
           CALL "SYSTEM" USING K-INVOKE-JAR

           *> INSERT FOR PROFESORMATERIECLASA FILD
           MOVE SPACE              TO SQL-STATEMENT

           STRING "INSERT INTO PROFESORMATERIECLASA"
           "(CNP, MATERIE, CLASA) "
           "VALUES ('"PROF-CNP"', "
           "'"MAT-NUME"', '"MAT-CLASA"')"
           DELIMITED BY SIZE INTO SQL-STATEMENT

           *> LET'S SEE HOW DOES THE STATEMENT LOOK.
           DISPLAY "SQL-INSERT-STATEMENT: " SQL-STATEMENT

           OPEN OUTPUT SQL-STATEMENT-FILE
           WRITE SQL-STATEMENT-FILE-FD FROM SQL-STATEMENT
           CLOSE SQL-STATEMENT-FILE

           *> INVOKE THE JAR
           CALL "SYSTEM" USING K-INVOKE-JAR

           *> AND AFTER ALL THIS WE NOW INSERT MATERIEPROFESOR
           MOVE SPACE              TO SQL-STATEMENT

           STRING "INSERT INTO MATERIEPROFESOR"
           "(CNP, MATERIE) "
           "VALUES ('"PROF-CNP"', "
           "'"MAT-NUME"')"
           DELIMITED BY SIZE INTO SQL-STATEMENT

           *> LET'S SEE HOW DOES THE STATEMENT LOOK.
           DISPLAY "SQL-INSERT-STATEMENT: " SQL-STATEMENT

           OPEN OUTPUT SQL-STATEMENT-FILE
           WRITE SQL-STATEMENT-FILE-FD FROM SQL-STATEMENT
           CLOSE SQL-STATEMENT-FILE

           *> INVOKE THE JAR
           CALL "SYSTEM" USING K-INVOKE-JAR
           .

       F-INSERT-DB0Z.
         EXIT.

       Z0 SECTION.
       Z0A.
           MOVE PROFI-MATERII-INFO TO RETURN-CODE
           .
       Z0Z.
           GOBACK.
       END PROGRAM PROFMODULE.
