       IDENTIFICATION DIVISION.
       PROGRAM-ID. CREATE-ACCOUNT.

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
           88  WS-FS-OK                            VALUE "00".
       01  WS-EOF                        PIC X      VALUE "N".
       01  WS-FOUND                      PIC X      VALUE "N".
       01  USERNAME-IN                   PIC X(10).
       01  PASSWORD-IN                   PIC X(12).
       01  FILE-USERNAME                 PIC X(10).
       01  FILE-PASSWORD                 PIC X(12).

       PROCEDURE DIVISION.
       MAIN.
           *> Ensure file exists (OPEN INPUT; if not, create with OUTPUT)
           OPEN INPUT USER-FILE
           IF NOT WS-FS-OK
              OPEN OUTPUT USER-FILE
              CLOSE USER-FILE
           ELSE
              CLOSE USER-FILE
           END-IF

           DISPLAY "Enter new username (<=10, letters/digits): " WITH NO ADVANCING
           ACCEPT USERNAME-IN
           DISPLAY "Enter new password (8-12 chars): " WITH NO ADVANCING
           ACCEPT PASSWORD-IN

           PERFORM USER-EXISTS
           IF WS-FOUND = "Y"
               DISPLAY "Username already exists. Account not created."
               STOP RUN
           END-IF

           PERFORM APPEND-USER

           DISPLAY "Account created successfully."
           STOP RUN.

       USER-EXISTS.
           MOVE "N" TO WS-FOUND
           MOVE "N" TO WS-EOF
           OPEN INPUT USER-FILE
           PERFORM UNTIL WS-EOF = "Y"
               READ USER-FILE
                   AT END MOVE "Y" TO WS-EOF
                   NOT AT END
                       UNSTRING USER-LINE DELIMITED BY ","
                           INTO FILE-USERNAME, FILE-PASSWORD
                       IF FUNCTION TRIM(FILE-USERNAME)
                          = FUNCTION TRIM(USERNAME-IN)
                           MOVE "Y" TO WS-FOUND
                           MOVE "Y" TO WS-EOF
                       END-IF
               END-READ
           END-PERFORM
           CLOSE USER-FILE.

       APPEND-USER.
           OPEN EXTEND USER-FILE
           STRING
               FUNCTION TRIM(USERNAME-IN) DELIMITED BY SIZE
               ","                         DELIMITED BY SIZE
               FUNCTION TRIM(PASSWORD-IN)  DELIMITED BY SIZE
               INTO USER-LINE
           END-STRING
           WRITE USER-LINE
           CLOSE USER-FILE.
