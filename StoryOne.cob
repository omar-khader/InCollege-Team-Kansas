       IDENTIFICATION DIVISION.
       PROGRAM-ID. StoryOne.

       ENVIRONMENT DIVISION.
       INPUT-OUTPUT SECTION.
       FILE-CONTROL.
           SELECT USER-FILE ASSIGN TO "users.dat"
               ORGANIZATION IS LINE SEQUENTIAL
               FILE STATUS IS WS-USER-STATUS.
           SELECT PROFILE-FILE ASSIGN TO "profiles.dat"
               ORGANIZATION IS LINE SEQUENTIAL
               FILE STATUS IS WS-PROFILE-STATUS.
           SELECT CONNECTION-FILE ASSIGN TO "connections.dat"
               ORGANIZATION IS LINE SEQUENTIAL
               FILE STATUS IS WS-CONN-STATUS.
           SELECT INP-FILE ASSIGN TO "InCollege-Input.txt"
               ORGANIZATION IS LINE SEQUENTIAL
               FILE STATUS IS WS-INP-STATUS.

       DATA DIVISION.
       FILE SECTION.
       FD  USER-FILE
           RECORD CONTAINS 200 CHARACTERS.
       01  USER-LINE                     PIC X(200).

       FD  PROFILE-FILE
           RECORD CONTAINS 1500 CHARACTERS.
       01  PROFILE-LINE                  PIC X(1500).

       FD  CONNECTION-FILE
           RECORD CONTAINS 200 CHARACTERS.
       01  CONNECTION-LINE               PIC X(200).

       FD  INP-FILE
           RECORD CONTAINS 200 CHARACTERS.
       01  INP-REC                       PIC X(200).

       WORKING-STORAGE SECTION.
       01  WS-USER-STATUS                PIC XX     VALUE "00".
       01  WS-PROFILE-STATUS             PIC XX     VALUE "00".
       01  WS-CONN-STATUS                PIC XX     VALUE "00".
    01  WS-INP-STATUS                 PIC XX     VALUE "00".

       01  USERNAME-IN                   PIC X(32).
       01  PASSWORD-IN                   PIC X(64).
       01  FILE-USERNAME                 PIC X(32).
       01  FILE-PASSWORD                 PIC X(64).

       01  EOF-FLAG                      PIC X      VALUE "N".
       01  FOUND-FLAG                    PIC X      VALUE "N".

       01  CUR-USER                      PIC X(32).

       01  REQ-SENDER                    PIC X(32).
       01  REQ-RECIPIENT                 PIC X(32).
       01  REQ-STATUS                    PIC X(20).

       01  TEMP-CONN-LINES               PIC X(200) OCCURS 100 TIMES.
       01  TEMP-CONN-COUNT               PIC 9(3) VALUE 0.

       01  I                             PIC 9(3) VALUE 1.
         01  CHOICE                        PIC 9 VALUE 0.
     01  SENDER                        PIC X(32).
     01  RECIP                         PIC X(32).
         01  INP-TRIM                     PIC X(200).

       PROCEDURE DIVISION.
       MAIN.
           *> Ensure files exist
           OPEN INPUT USER-FILE
           IF WS-USER-STATUS NOT = "00"
               OPEN OUTPUT USER-FILE
               CLOSE USER-FILE
           ELSE
               CLOSE USER-FILE
           END-IF

           OPEN INPUT PROFILE-FILE
           IF WS-PROFILE-STATUS NOT = "00"
               OPEN OUTPUT PROFILE-FILE
               CLOSE PROFILE-FILE
           ELSE
               CLOSE PROFILE-FILE
           END-IF

           OPEN INPUT CONNECTION-FILE
           IF WS-CONN-STATUS NOT = "00"
               OPEN OUTPUT CONNECTION-FILE
               CLOSE CONNECTION-FILE
           ELSE
               CLOSE CONNECTION-FILE
           END-IF

           OPEN INPUT INP-FILE
           IF WS-INP-STATUS NOT = "00"
               DISPLAY "WARNING: could not open InCollege-Input.txt"
           END-IF

           DISPLAY "Welcome to InCollege!"
           DISPLAY "1. Log In"
           DISPLAY "2. Exit"
           DISPLAY "Enter your choice: "
           PERFORM GET-NEXT
           IF INP-TRIM NOT = SPACE
               MOVE FUNCTION NUMVAL(INP-TRIM) TO CHOICE
           ELSE
               MOVE 2 TO CHOICE
           END-IF
           IF CHOICE NOT = 1
               STOP RUN
           END-IF

           PERFORM LOGIN

           IF FOUND-FLAG = "Y"
               DISPLAY "You have successfully logged in."
               DISPLAY "Welcome, " FUNCTION TRIM(CUR-USER) "!"
               PERFORM MAIN-MENU
           ELSE
               DISPLAY "Login failed."
           END-IF

           STOP RUN.

       LOGIN.
           PERFORM GET-NEXT
           MOVE FUNCTION TRIM(INP-TRIM) TO USERNAME-IN
           PERFORM GET-NEXT
           MOVE FUNCTION TRIM(INP-TRIM) TO PASSWORD-IN
           MOVE FUNCTION TRIM(USERNAME-IN) TO CUR-USER

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
                       IF FUNCTION TRIM(FILE-USERNAME) = FUNCTION TRIM(USERNAME-IN)
                          AND FUNCTION TRIM(FILE-PASSWORD) = FUNCTION TRIM(PASSWORD-IN)
                           MOVE "Y" TO FOUND-FLAG
                           MOVE "Y" TO EOF-FLAG
                       END-IF
               END-READ
           END-PERFORM
           CLOSE USER-FILE.

       MAIN-MENU.
           PERFORM UNTIL CHOICE = 9
               DISPLAY "1. View My Pending Connection Requests"
               DISPLAY "2. View My Network"
               DISPLAY "9. Exit"
               DISPLAY "Enter your choice: "
               PERFORM GET-NEXT
               IF INP-TRIM NOT = SPACE
                   MOVE FUNCTION NUMVAL(INP-TRIM) TO CHOICE
               ELSE
                   MOVE 9 TO CHOICE
               END-IF
               EVALUATE CHOICE
                   WHEN 1
                       PERFORM VIEW-PENDING-REQUESTS
                   WHEN 2
                       PERFORM VIEW-NETWORK
                   WHEN OTHER
                       CONTINUE
               END-EVALUATE
           END-PERFORM.

       VIEW-PENDING-REQUESTS.
           *> Load pending requests where recipient = CUR-USER
           MOVE 0 TO TEMP-CONN-COUNT
           MOVE "N" TO EOF-FLAG
           OPEN INPUT CONNECTION-FILE
           PERFORM UNTIL EOF-FLAG = "Y"
               READ CONNECTION-FILE
                   AT END
                       MOVE "Y" TO EOF-FLAG
                   NOT AT END
                       UNSTRING CONNECTION-LINE DELIMITED BY "|"
                           INTO REQ-SENDER, REQ-RECIPIENT, REQ-STATUS
                       IF FUNCTION TRIM(REQ-RECIPIENT) = FUNCTION TRIM(CUR-USER)
                          AND FUNCTION TRIM(REQ-STATUS) = "pending"
                           ADD 1 TO TEMP-CONN-COUNT
                           MOVE CONNECTION-LINE TO TEMP-CONN-LINES(TEMP-CONN-COUNT)
                       END-IF
               END-READ
           END-PERFORM

           IF TEMP-CONN-COUNT = 0
               DISPLAY "--- Pending Connection Requests ---"
               DISPLAY "You have no pending connection requests at this time."
               CLOSE CONNECTION-FILE
               EXIT PARAGRAPH
           END-IF

           *> For each pending request, ask accept/reject and update file
           PERFORM VARYING I FROM 1 BY 1 UNTIL I > TEMP-CONN-COUNT
               MOVE TEMP-CONN-LINES(I) TO CONNECTION-LINE
               UNSTRING CONNECTION-LINE DELIMITED BY "|"
                   INTO REQ-SENDER, REQ-RECIPIENT, REQ-STATUS
               DISPLAY "--- Pending Connection Requests ---"
               DISPLAY "Request from: " FUNCTION TRIM(REQ-SENDER)
               DISPLAY "1. Accept"
               DISPLAY "2. Reject"
               DISPLAY "Enter your choice for " FUNCTION TRIM(REQ-SENDER) ": "
               PERFORM GET-NEXT
               IF INP-TRIM NOT = SPACE
                   MOVE FUNCTION NUMVAL(INP-TRIM) TO CHOICE
               ELSE
                   MOVE 2 TO CHOICE
               END-IF
               DISPLAY "DEBUG: read choice input='" INP-TRIM "' CHOICE='" CHOICE "'"
               IF CHOICE = 1
                   MOVE FUNCTION TRIM(REQ-SENDER) TO SENDER
                   MOVE FUNCTION TRIM(CUR-USER) TO RECIP
                   PERFORM ACCEPT-REQUEST
                   DISPLAY "Connection request from " FUNCTION TRIM(REQ-SENDER) " accepted!"
               ELSE
                   MOVE FUNCTION TRIM(REQ-SENDER) TO SENDER
                   MOVE FUNCTION TRIM(CUR-USER) TO RECIP
                   PERFORM REJECT-REQUEST
                   DISPLAY "Connection request from " FUNCTION TRIM(REQ-SENDER) " rejected."
               END-IF
           END-PERFORM

           CLOSE CONNECTION-FILE.

    ACCEPT-REQUEST.
        DISPLAY "DEBUG: ACCEPT start. SENDER=" FUNCTION TRIM(SENDER) " RECIP=" FUNCTION TRIM(RECIP)
           *> Read all connections, rewrite updating the accepted entry to 'connected'
           *> Also add reciprocal connection if not present
           *> We'll rewrite the entire connections file to a temp table and then overwrite file
           MOVE 0 TO TEMP-CONN-COUNT
           MOVE "N" TO EOF-FLAG
           OPEN INPUT CONNECTION-FILE
           PERFORM UNTIL EOF-FLAG = "Y"
               READ CONNECTION-FILE
                   AT END
                       MOVE "Y" TO EOF-FLAG
                   NOT AT END
                       ADD 1 TO TEMP-CONN-COUNT
                       MOVE CONNECTION-LINE TO TEMP-CONN-LINES(TEMP-CONN-COUNT)
               END-READ
           END-PERFORM
           CLOSE CONNECTION-FILE

           *> Update the matching pending to connected
           PERFORM VARYING I FROM 1 BY 1 UNTIL I > TEMP-CONN-COUNT
               UNSTRING TEMP-CONN-LINES(I) DELIMITED BY "|"
                   INTO REQ-SENDER, REQ-RECIPIENT, REQ-STATUS
               IF FUNCTION TRIM(REQ-SENDER) = SENDER AND FUNCTION TRIM(REQ-RECIPIENT) = RECIP
                   IF FUNCTION TRIM(REQ-STATUS) = "pending"
                       STRING REQ-SENDER DELIMITED BY SIZE "|" DELIMITED BY SIZE REQ-RECIPIENT DELIMITED BY SIZE "|connected" DELIMITED BY SIZE INTO TEMP-CONN-LINES(I)
                   END-IF
               END-IF
           END-PERFORM

           *> Ensure reciprocal connected exists; if not, append
           MOVE "N" TO FOUND-FLAG
           PERFORM VARYING I FROM 1 BY 1 UNTIL I > TEMP-CONN-COUNT
               UNSTRING TEMP-CONN-LINES(I) DELIMITED BY "|"
                   INTO REQ-SENDER, REQ-RECIPIENT, REQ-STATUS
               IF FUNCTION TRIM(REQ-SENDER) = RECIP AND FUNCTION TRIM(REQ-RECIPIENT) = SENDER
                   IF FUNCTION TRIM(REQ-STATUS) = "connected"
                       MOVE "Y" TO FOUND-FLAG
                   END-IF
               END-IF
           END-PERFORM

           IF FOUND-FLAG = "N"
               ADD 1 TO TEMP-CONN-COUNT
               STRING RECIP DELIMITED BY SIZE "|" DELIMITED BY SIZE SENDER DELIMITED BY SIZE "|connected" DELIMITED BY SIZE INTO TEMP-CONN-LINES(TEMP-CONN-COUNT)
           END-IF

           *> Rewrite connections file
           OPEN OUTPUT CONNECTION-FILE
           PERFORM VARYING I FROM 1 BY 1 UNTIL I > TEMP-CONN-COUNT
               MOVE TEMP-CONN-LINES(I) TO CONNECTION-LINE
               WRITE CONNECTION-LINE
           END-PERFORM
           CLOSE CONNECTION-FILE
           DISPLAY "DEBUG: ACCEPT finished. Rewrote " TEMP-CONN-COUNT " lines."
           .

    REJECT-REQUEST.
           MOVE 0 TO TEMP-CONN-COUNT
           MOVE "N" TO EOF-FLAG
           OPEN INPUT CONNECTION-FILE
           PERFORM UNTIL EOF-FLAG = "Y"
               READ CONNECTION-FILE
                   AT END
                       MOVE "Y" TO EOF-FLAG
                   NOT AT END
                       ADD 1 TO TEMP-CONN-COUNT
                       MOVE CONNECTION-LINE TO TEMP-CONN-LINES(TEMP-CONN-COUNT)
               END-READ
           END-PERFORM
           CLOSE CONNECTION-FILE

           *> Remove the pending entry by shifting down
           MOVE 0 TO I
           PERFORM VARYING CHOICE FROM 1 BY 1 UNTIL CHOICE > TEMP-CONN-COUNT
               UNSTRING TEMP-CONN-LINES(CHOICE) DELIMITED BY "|"
                   INTO REQ-SENDER, REQ-RECIPIENT, REQ-STATUS
               IF FUNCTION TRIM(REQ-SENDER) = SENDER AND FUNCTION TRIM(REQ-RECIPIENT) = RECIP AND FUNCTION TRIM(REQ-STATUS) = "pending"
                   CONTINUE
               ELSE
                   ADD 1 TO I
                   MOVE TEMP-CONN-LINES(CHOICE) TO TEMP-CONN-LINES(I)
               END-IF
           END-PERFORM

           MOVE I TO TEMP-CONN-COUNT

           OPEN OUTPUT CONNECTION-FILE
           PERFORM VARYING I FROM 1 BY 1 UNTIL I > TEMP-CONN-COUNT
               MOVE TEMP-CONN-LINES(I) TO CONNECTION-LINE
               WRITE CONNECTION-LINE
           END-PERFORM
           CLOSE CONNECTION-FILE
           .

       VIEW-NETWORK.
           *> Show all connections where either sender or recipient is CUR-USER and status connected
           MOVE "N" TO EOF-FLAG
           OPEN INPUT CONNECTION-FILE
           DISPLAY "--- Your Network ---"
           PERFORM UNTIL EOF-FLAG = "Y"
               READ CONNECTION-FILE
                   AT END
                       MOVE "Y" TO EOF-FLAG
                   NOT AT END
                       UNSTRING CONNECTION-LINE DELIMITED BY "|"
                           INTO REQ-SENDER, REQ-RECIPIENT, REQ-STATUS
                       IF FUNCTION TRIM(REQ-STATUS) = "connected"
                           IF FUNCTION TRIM(REQ-SENDER) = FUNCTION TRIM(CUR-USER)
                               DISPLAY "Connected with: " FUNCTION TRIM(REQ-RECIPIENT)
                           ELSE IF FUNCTION TRIM(REQ-RECIPIENT) = FUNCTION TRIM(CUR-USER)
                               DISPLAY "Connected with: " FUNCTION TRIM(REQ-SENDER)
                           END-IF
                       END-IF
               END-READ
           END-PERFORM
           CLOSE CONNECTION-FILE
           DISPLAY "--------------------"
           .

       GET-NEXT.
           MOVE SPACES TO INP-TRIM
           READ INP-FILE
               AT END
                   MOVE SPACES TO INP-TRIM
               NOT AT END
                   MOVE FUNCTION TRIM(INP-REC) TO INP-TRIM
           END-READ
           .
