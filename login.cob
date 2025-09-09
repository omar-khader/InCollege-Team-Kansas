       IDENTIFICATION DIVISION.
       PROGRAM-ID. LOGIN.

       ENVIRONMENT DIVISION.
       INPUT-OUTPUT SECTION.
       FILE-CONTROL.
           SELECT USER-FILE ASSIGN TO "users.dat"
               ORGANIZATION IS LINE SEQUENTIAL
               FILE STATUS IS WS-USER-STATUS.

       DATA DIVISION.
       FILE SECTION.
       FD  USER-FILE
           RECORD CONTAINS 80 CHARACTERS.
       01  USER-LINE                     PIC X(80).

       WORKING-STORAGE SECTION.
       01  WS-USER-STATUS                PIC XX     VALUE "00".
       01  EOF-FLAG                      PIC X      VALUE "N".
       01  FOUND-FLAG                    PIC X      VALUE "N".
       01  ATTEMPTS                      PIC 9      VALUE 0.
       01  MAX-ATTEMPTS                  PIC 9      VALUE 3.

       01  USERNAME-IN                   PIC X(10).   *> username up to 10
       01  PASSWORD-IN                   PIC X(12).   *> password up to 12
       01  FILE-USERNAME                 PIC X(10).
       01  FILE-PASSWORD                 PIC X(12).

       PROCEDURE DIVISION.
       MAIN.
           *> Ensure file exists (create if missing)
           OPEN INPUT USER-FILE
           IF WS-USER-STATUS NOT = "00"
              OPEN OUTPUT USER-FILE
              CLOSE USER-FILE
           ELSE
              CLOSE USER-FILE
           END-IF

           PERFORM UNTIL ATTEMPTS >= MAX-ATTEMPTS OR FOUND-FLAG = "Y"
               ADD 1 TO ATTEMPTS
               DISPLAY "Username: " WITH NO ADVANCING
               ACCEPT USERNAME-IN
               DISPLAY "Password: " WITH NO ADVANCING
               ACCEPT PASSWORD-IN

               PERFORM TRY-LOGIN

               IF FOUND-FLAG NOT = "Y"
                   IF ATTEMPTS < MAX-ATTEMPTS
                       DISPLAY "Login failed: invalid username or password." 
                       DISPLAY "Please try again."
                   END-IF
               END-IF
           END-PERFORM

           IF FOUND-FLAG = "Y"
               DISPLAY "Login successful!"
           ELSE
               DISPLAY "Login failed, invalid username or password after "
                       MAX-ATTEMPTS " attempts."
           END-IF
           STOP RUN.

       TRY-LOGIN.
           MOVE "N" TO FOUND-FLAG
           MOVE "N" TO EOF-FLAG

           OPEN INPUT USER-FILE
           PERFORM UNTIL EOF-FLAG = "Y"
               READ USER-FILE
                   AT END
                       MOVE "Y" TO EOF-FLAG
                   NOT AT END
                       UNSTRING USER-LINE DELIMITED BY ","
                           INTO FILE-USERNAME, FILE-PASSWORD
                       IF FUNCTION TRIM(FILE-USERNAME)
                          = FUNCTION TRIM(USERNAME-IN)
                          AND
                          FUNCTION TRIM(FILE-PASSWORD)
                          = FUNCTION TRIM(PASSWORD-IN)
                           MOVE "Y" TO FOUND-FLAG
                           MOVE "Y" TO EOF-FLAG
                       END-IF
               END-READ
           END-PERFORM
           CLOSE USER-FILE.
