       IDENTIFICATION DIVISION.
       PROGRAM-ID. VIEW-RECEIVED-MESSAGES.

       ENVIRONMENT DIVISION.
       INPUT-OUTPUT SECTION.
       FILE-CONTROL.
           SELECT MESSAGE-FILE ASSIGN TO "messages.dat"
               ORGANIZATION IS LINE SEQUENTIAL.

       DATA DIVISION.
       FILE SECTION.
       FD MESSAGE-FILE.
       01 MESSAGE-LINE PIC X(500).

       WORKING-STORAGE SECTION.
       01 CURRENT-USER         PIC X(32).
       01 MSG-FROM-USER        PIC X(32).
       01 MSG-TO-USER          PIC X(32).
       01 MSG-CONTENT          PIC X(200).
       01 MSG-TIMESTAMP        PIC X(20).
       01 MESSAGE-FOUND        PIC X VALUE 'N'.
       01 WS-MESSAGE-COUNT     PIC 9(3) VALUE 0.

       PROCEDURE DIVISION.
       DISPLAY "Enter logged-in username:".
       ACCEPT CURRENT-USER.
       OPEN INPUT MESSAGE-FILE.

       PERFORM UNTIL MESSAGE-FOUND = 'Y'
           READ MESSAGE-FILE
               AT END
                   EXIT PERFORM
               NOT AT END
                   UNSTRING MESSAGE-LINE DELIMITED BY ',' INTO
                       MSG-FROM-USER, MSG-TO-USER, MSG-CONTENT, MSG-TIMESTAMP
                   IF TRIM(MSG-TO-USER) = TRIM(CURRENT-USER)
                       ADD 1 TO WS-MESSAGE-COUNT
                       MOVE 'Y' TO MESSAGE-FOUND
                   END-IF
           END-READ
       END-PERFORM

       CLOSE MESSAGE-FILE.
       STOP RUN.
