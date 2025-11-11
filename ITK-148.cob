       IDENTIFICATION DIVISION.
       PROGRAM-ID. OUTPUT-TO-FILE.

       ENVIRONMENT DIVISION.
       INPUT-OUTPUT SECTION.
       FILE-CONTROL.
           SELECT OUT-FILE ASSIGN TO "InCollege-Output.txt"
               ORGANIZATION IS LINE SEQUENTIAL.
           SELECT MESSAGE-FILE ASSIGN TO "messages.dat"
               ORGANIZATION IS LINE SEQUENTIAL.

       DATA DIVISION.
       FILE SECTION.
       FD OUT-FILE.
       01 OUT-RECORD PIC X(80).
       FD MESSAGE-FILE.
       01 MESSAGE-LINE PIC X(500).

       WORKING-STORAGE SECTION.
       01 CURRENT-USER         PIC X(32).
       01 MSG-FROM-USER        PIC X(32).
       01 MSG-TO-USER          PIC X(32).
       01 MSG-CONTENT          PIC X(200).
       01 MSG-TIMESTAMP        PIC X(20).
       01 WS-FOUNDMESSAGE      PIC X VALUE 'N'.

       PROCEDURE DIVISION.
       DISPLAY "Enter logged-in username:".
       ACCEPT CURRENT-USER.

       OPEN OUTPUT OUT-FILE.
       OPEN INPUT MESSAGE-FILE.

       PERFORM UNTIL WS-FOUNDMESSAGE = 'Y'
           READ MESSAGE-FILE
               AT END
                   MOVE "You have no messages." TO OUT-RECORD
                   WRITE OUT-RECORD
                   EXIT PERFORM
               NOT AT END
                   UNSTRING MESSAGE-LINE DELIMITED BY ',' INTO
                       MSG-FROM-USER, MSG-TO-USER, MSG-CONTENT, MSG-TIMESTAMP
                   IF TRIM(MSG-TO-USER) = TRIM(CURRENT-USER)
                       MOVE "From: " & TRIM(MSG-FROM-USER) TO OUT-RECORD
                       WRITE OUT-RECORD
                       MOVE "Message: " & TRIM(MSG-CONTENT) TO OUT-RECORD
                       WRITE OUT-RECORD
                       MOVE "Date: " & TRIM(MSG-TIMESTAMP) TO OUT-RECORD
                       WRITE OUT-RECORD
                       MOVE 'Y' TO WS-FOUNDMESSAGE
                   END-IF
           END-READ
       END-PERFORM

       CLOSE MESSAGE-FILE.
       CLOSE OUT-FILE.
       STOP RUN.
