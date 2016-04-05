               05 CNP-VAL            PIC X(13).
               05 CNP-VAL-GROUP REDEFINES CNP-VAL.
                   10 CNP-VAL-SEX    PIC 9(01).
                   10 CNP-VAL-AN-2    PIC 9(02).
                   10 CNP-VAL-LUNA    PIC 9(02).
                   10 CNP-VAL-ZI    PIC 9(02).
                   10 CNP-VAL-JUD    PIC 9(02).
                   10 CNP-VAL-3CIF    PIC 9(03).
                   10 CNP-VAL-CIF-CTRL PIC 9(1).

           *> * OUTPUT
               05 CNP-QUIT            PIC X(2).
               05 CNP-ERR-MSG        PIC X(70).
               05 CNP-DAT-NASTERE.
                   10 CNP-DAT-AN    PIC 9(4).
                   10 CNP-DAT-AN-GROUP REDEFINES CNP-DAT-AN.
                     15 CNP-DAT-AN-FIRST   PIC 9(02).
                     15 CNP-DAT-AN-TWO     PIC 9(02).
                   10 CNP-DAT-LUNA    PIC X(2).
                   10 CNP-DAT-ZI    PIC X(2).
               05 CNP-NUME-JUDET        PIC X(30).

           *> * SEX (M/F)
               05 CNP-SEX            PIC X(01).

           *> * DACA QUIT = “02”, AICI SE PUNE CIFRA DE CONTROL CORECTA.
               05 CNP-CIF-CTRL-CORECT    PIC 9(01).
