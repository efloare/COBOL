      ******************************************************************
      * Author:
      * Date:
      * Purpose:
      * Tectonics: cobc
      ******************************************************************
       IDENTIFICATION DIVISION.
      *-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-
       PROGRAM-ID. NOTEMODULE.
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
       COPY NOTE-INFO.

       PROCEDURE DIVISION USING NOTE-INFO.
      *-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-
       S0  SECTION.
       S0A.
           MOVE NOTE-CNP-STUD             TO CNP-VAL
           CALL "CNPMODULE"                USING CNP-VAL
           IF (CNP-QUIT = 0)
             PERFORM F-INSERT-DB
           ELSE
             MOVE 1 TO NOTE-ERROR
           END-IF
           PERFORM Z0
           .
       S0Z.
           GOBACK.
       F-INSERT-DB SECTION.
       F-INSERT-DB0A.
         *> PREPARE SQL STATEMENT FOR INSERT
         *> INSERT FOR NOTE FILD CNP, MATERIE, NOTA,DATANOTA
           MOVE SPACE              TO SQL-STATEMENT

           STRING "INSERT INTO NOTE(CNP, MATERIE, NOTA, "
           "DATANOTA) VALUES ('"NOTE-CNP-STUD"', "
           "'"NOTE-MATERIE"' ,'"NOTE-NOTA"','"NOTE-DATA"')"
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
           MOVE NOTE-INFO TO RETURN-CODE
           .
       Z0Z.
           GOBACK.
       END PROGRAM NOTEMODULE.
