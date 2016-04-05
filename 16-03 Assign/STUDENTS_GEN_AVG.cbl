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
           SELECT ST-INPUT ASSIGN TO 'StGradesCobol.txt'
           ORGANIZATION IS LINE SEQUENTIAL.

           SELECT ST-AVGS ASSIGN TO 'Results.txt'
           ORGANIZATION IS LINE SEQUENTIAL.
      *-----------------------
       DATA DIVISION.
      *-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-
       FILE SECTION.
           FD ST-INPUT.
           01 ST-INPUT-FD                PIC X(999).

           FD ST-AVGS.
           01 ST-AVGS-FD                 PIC X(128).
      *-----------------------
       WORKING-STORAGE SECTION.
        01 STUDENT-GRADES.
         05 ST-CLASS                     PIC 9(02).
         05 ST-LAST-NAME                 PIC X(31).
         05 ST-FIRST-NAME                PIC X(31).
         05 ST-SUBJECT                   PIC X(31).
         05 ST-SCORE                       PIC 99.
         05 ST-DATE.
            10 ST-DATE-YEAR              PIC 9(4).
            10 ST-DATE-MONTH             PIC 9(2).
            10 ST-DATE-DAY               PIC 9(2).


        01 AVG-ST.
         05 AVG-ST-CLASS                 PIC X(02).
         05 FILLER                       PIC X(16).
         05 AVG-ST-LAST-NAME             PIC X(30).
         05 AVG-ST-FIRST-NAME            PIC X(30).
         05 AVG-ST-SUBJECT               PIC X(30).
         05 AVG-ST-SCORE                 PIC 9(2).9(2).
         05 FILLER                       PIC X(14).

        01 INP-SWITCH                    PIC X(1) VALUE "N".
            88  INP1-EOF                 VALUE "E".
            88  INP1-NEOF                VALUE "N".

        01 OGRU.
            05 OGRU-CLASS                PIC 9(02).
            05 OGRU-LAST-F-NAME          PIC X(60).
            05 OGRU-SUBJ                 PIC X(30).
        01 NGRU.
            05 NGRU-CLASS                PIC 9(02).
            05 NGRU-LAST-F-NAME          PIC X(60).
            05 NGRU-SUBJ                 PIC X(30).
        01 WS-COUNT-SPACES                  PIC 9(2).
        01 WS-COUNT-CHAR                   PIC 9(2).
        01 WS-FIELDS.
            05 WS-SUM-OF-GRADES-FOR-SUBJ PIC 99V9(2).
            05 WS-AVG-OF-SUBJECT         PIC 99V9(2).
            05 WS-AVG-OF-SUBJECT-DISP    PIC B9.9(2).
            05 WS-COUNT-GRADES           PIC 9(3).
            05 WS-MAX-GRADE              PIC 9(2).
            05 WS-COUNT-LINES            PIC 9(4).
            05 WS-GENERAL-AVG-SUM        PIC 9(4)V9(2).
            05 WS-COUNT-SUBJECTS         PIC 9(2).
            05 WS-GENERAL-AVG            PIC 9(2)V9(2).
            05 WS-GENERAL-AVG-SUBJECT    PIC 9(2)V9(2).

        01 KONSTANTEN.
            05 K-COLS                    PIC X(100) VALUE
           "----+----1----+----2----+----3----+----4----+----5----+----6
      -    "----+----7----+----8----+----9----+----0".
            05 K-HEADER                  PIC X(128) VALUE
           "CLASS OF STUDENT  LAST NAME                     FIRST NAME
      -    "                  SUBJECT                        AVERAGE FOR
      -    " SUBJECT".
      *-----------------------
       PROCEDURE DIVISION.
      *-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-
       S0 SECTION.
       S0A.
           DISPLAY "BEGINNING PROGRAM"
           PERFORM A0
           PERFORM B0
           PERFORM UNTIL INP1-EOF
              PERFORM F0
              PERFORM B0
              PERFORM E0
              MOVE NGRU                TO OGRU
           END-PERFORM

           PERFORM Z0
           STOP RUN.
       S0Z.
           EXIT.

       A0 SECTION.
       A0A.
           OPEN INPUT ST-INPUT
           OPEN OUTPUT ST-AVGS
           MOVE 0                      TO WS-SUM-OF-GRADES-FOR-SUBJ
           MOVE 0                      TO WS-COUNT-GRADES
           MOVE 0                      TO WS-MAX-GRADE
           MOVE SPACE                  TO OGRU
           PERFORM A-PREPARE-HEADLINES
           MOVE 0                      TO WS-COUNT-LINES
           .
       A0Z.
           EXIT.

       A-PREPARE-HEADLINES SECTION.
       A-PREPARE-HEADLINESA.
           MOVE SPACE                  TO ST-AVGS-FD
           STRING K-COLS
                  K-COLS(1:28) DELIMITED BY SIZE
                  INTO ST-AVGS-FD
           END-STRING
           PERFORM Y20

           MOVE SPACE                  TO ST-AVGS-FD
           MOVE K-HEADER               TO ST-AVGS-FD
           PERFORM Y20

           MOVE SPACE                  TO ST-AVGS-FD
           STRING K-COLS
                  K-COLS(1:28) DELIMITED BY SIZE
                  INTO ST-AVGS-FD
           END-STRING
           PERFORM Y20
           .
       A-PREPARE-HEADLINESZ.
           EXIT.

       B0 SECTION.
       B0A.
           MOVE SPACE                  TO STUDENT-GRADES
           READ ST-INPUT INTO STUDENT-GRADES
           AT END
               SET INP1-EOF            TO TRUE
           END-READ

           IF NOT INP1-EOF
               ADD 1 TO WS-COUNT-LINES
           END-IF

           .
       B0Z.
           EXIT.

       E0 SECTION.
       E0A.
           IF NOT INP1-EOF
               MOVE ST-CLASS               TO NGRU-CLASS
               MOVE SPACE                  TO NGRU-LAST-F-NAME
               STRING ST-LAST-NAME
                      ST-FIRST-NAME
                   DELIMITED BY SIZE INTO NGRU-LAST-F-NAME
               END-STRING
               MOVE ST-SUBJECT             TO NGRU-SUBJ
           ELSE
               MOVE HIGH-VALUE             TO NGRU
           END-IF
           IF OGRU = NGRU
           OR OGRU = SPACE
             CONTINUE
           ELSE
             PERFORM E-OUTPUT-AVERAGE
             MOVE 0                        TO WS-SUM-OF-GRADES-FOR-SUBJ
             MOVE 0                        TO WS-COUNT-GRADES
             MOVE 0                        TO WS-MAX-GRADE
           END-IF
           IF OGRU-LAST-F-NAME NOT = NGRU-LAST-F-NAME
           AND OGRU-LAST-F-NAME > SPACE
               ADD WS-AVG-OF-SUBJECT       TO WS-GENERAL-AVG-SUM
               ADD 1                       TO WS-COUNT-SUBJECTS
               COMPUTE WS-GENERAL-AVG = WS-GENERAL-AVG-SUM /
                                           WS-COUNT-SUBJECTS
               COMPUTE WS-GENERAL-AVG-SUBJECT = WS-GENERAL-AVG-SUM /
                                           5
               DISPLAY "GENERAL AVERAGE OF  " OGRU-LAST-F-NAME(1:30)
                   OGRU-LAST-F-NAME(31:)    "IS: "
                   WS-GENERAL-AVG-SUBJECT
               DISPLAY "GENERAL AVERAGE = "
                                  WS-GENERAL-AVG

               MOVE 0                      TO WS-GENERAL-AVG-SUM
                                              WS-GENERAL-AVG
                                              WS-COUNT-SUBJECTS
           END-IF
           .
       E0Z.
           EXIT.
       *> Task 3*> : Compute the general average for each student.
       *> General-average = Sum-of-averages-of-each-subject / number-of-subjects
       E-OUTPUT-AVERAGE SECTION.
       E-OUTPUT-AVERAGEA.
           DISPLAY "NEW GROUP ENCOUNTERED!"
           DISPLAY "current line = " WS-COUNT-LINES
           COMPUTE WS-AVG-OF-SUBJECT =
                        WS-SUM-OF-GRADES-FOR-SUBJ / WS-COUNT-GRADES
           INSPECT ST-LAST-NAME TALLYING WS-COUNT-SPACES FOR TRAILING
                                                           SPACES
           *> FOR LEADING
           COMPUTE WS-COUNT-CHAR = LENGTH OF ST-LAST-NAME -
                               WS-COUNT-SPACES

           DISPLAY "AVG = " WS-AVG-OF-SUBJECT
           MOVE SPACE                    TO ST-AVGS-FD
                                            AVG-ST
           MOVE OGRU-CLASS               TO AVG-ST-CLASS
           MOVE OGRU-LAST-F-NAME(1:30)   TO AVG-ST-LAST-NAME
           MOVE OGRU-LAST-F-NAME(31:)    TO AVG-ST-FIRST-NAME
           MOVE OGRU-SUBJ                TO AVG-ST-SUBJECT
           MOVE WS-AVG-OF-SUBJECT        TO AVG-ST-SCORE
           MOVE AVG-ST                   TO ST-AVGS-FD
           DISPLAY "MAX-GRADE = " WS-MAX-GRADE

           COMPUTE WS-GENERAL-AVG-SUM = WS-GENERAL-AVG-SUM +
                                       WS-AVG-OF-SUBJECT
           PERFORM Y20

           .
       E-OUTPUT-AVERAGEZ.
           EXIT.

       F0 SECTION.
       F0A.
      * PROCESSING THE LINE.

           COMPUTE WS-SUM-OF-GRADES-FOR-SUBJ =
           WS-SUM-OF-GRADES-FOR-SUBJ + ST-SCORE
           ADD 1 TO WS-COUNT-GRADES
           IF WS-MAX-GRADE < ST-SCORE
               MOVE ST-SCORE             TO WS-MAX-GRADE
               *> DISPLAY WS-MAX-GRADE
           END-IF
           .
       F0Z.
           EXIT.

       Y20 SECTION.
       Y20A.
           WRITE ST-AVGS-FD.
       Y20Z.
           EXIT.

       Z0 SECTION.
       Z0A.
           CLOSE ST-INPUT
           CLOSE ST-AVGS.
       Z0Z.
           EXIT.
