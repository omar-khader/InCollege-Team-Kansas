IDENTIFICATION DIVISION.
PROGRAM-ID. LOGIN.

ENVIRONMENT DIVISION.
INPUT-OUTPUT SECTION.
FILE-CONTROL.
       SELECT USER-FILE ASSIGN TO "users.dat"
           ORGANIZATION IS LINE SEQUENTIAL.

DATA DIVISION.
FILE SECTION.
FD USER-FILE.
01 USER-LINE   PIC X(80)

WORKING-STORAGE SECTION.
01 USERNAME-IN     PIC X(20).
01 PASSWORD-IN     PIC X(20).
01 FILE-USERNAME   PIC X(20).
01 FILE-PASSWORD   PIC X(20).
01 FOUND-FLAG      PIC X VALUE "N".
01 EOF-FLAG        PIC X VALUE "N".
01 ATTEMPTS    PIC 9 VALUE 0.
01 MAX-ATTEMPTS    PIC 9 VALUE 3.

PROCEDURE DIVISION.
MAIN.
       PERFORM UNTIL ATTEMPTS >= MAX-ATTEMPTS OR FOUND-FLAG = "Y"
           ADD 1 TO ATTEMPTS
           DISPLAY "Username: " ACCEPT USERNAME-IN
           DISPLAY "Password: " ACCEPT PASSWORD-IN

           PERFORM TRY-LOGIN

           IF FOUND-FLAG = "Y"
               IF ATTEMPTS < MAX-ATTEMPTS
                   DISPLAY "Login failed: invalid username or password."
                   DISPLAY "Please try again."
               END-IF
           END-IF
       END-PERFORM

       IF FOUND-FLAG = "Y"
           DISPLAY "Login successful!"
       ELSE
           DISPLAY "Login failed, invalid username or password after" MAX-ATTEMPTS " attempts."
       END-IF
       STOP RUN.

TRY-LOGIN.
       MOVE "N" to FOUND-FLAG
       MOVE "N" TO EOF-FLAG
       OPEN INPUT USER-FILE
       PERFORM UNTIL EOF-FLAG = "Y"
           READ USER-FILE
               AT END MOVE "Y" TO EOF-FLAG
               NOT AT END
                   UNSTRING USER-LINE DELIMITED BY ","
                       INTO FILE-USERNAME FILE-PASSWORD
                   IF FUNCTION TRIM(FILE-USERNAME) =
                       FUNCTION TRIM(USERNAME-IN)
                       AND
                       FUNCTION TRIM(FILE-PASSWORD) =
                           FUNCTION TRIM(PASSWORD-IN)
                       MOVE "Y" TO FOUND-FLAG
                       MOVE "Y" TO EOF-FLAG
                   END-IF
           END-READ
       END-PERFORM
       CLOSE USER-FILE.
