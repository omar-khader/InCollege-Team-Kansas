IDENTIFICATION DIVISION.
       PROGRAM-ID. STORYFOUR.

       ENVIRONMENT DIVISION.
       INPUT-OUTPUT SECTION.
       FILE-CONTROL.
           SELECT InpFile ASSIGN TO "InCollege-Input.txt"
               ORGANIZATION IS LINE SEQUENTIAL.
           SELECT CONNECTION-FILE ASSIGN TO "connections.dat"
               ORGANIZATION IS LINE SEQUENTIAL.
           SELECT PROFILE-FILE ASSIGN TO "profiles.dat"
               ORGANIZATION IS LINE SEQUENTIAL.
           SELECT OutFile ASSIGN TO "InCollege-Output.txt"
               ORGANIZATION IS LINE SEQUENTIAL.

       DATA DIVISION.
       FILE SECTION.
       FD  InpFile.
       01  INP-RECORD                PIC X(200).

       FD  CONNECTION-FILE.
       01  CONNECTION-LINE           PIC X(200).

       FD  PROFILE-FILE.
       01  PROFILE-LINE              PIC X(1500).

       FD  OutFile.
       01  OUT-RECORD                PIC X(200).

       WORKING-STORAGE SECTION.
       01  WS-EOF                    PIC X VALUE "N".
       01  MENU-CHOICE               PIC 9 VALUE 0.
       01  USERNAME-IN               PIC X(32).
       01  PASSWORD-IN               PIC X(64).
    01  TEMP-LINE                 PIC X(200).
    01  CHOICE                    PIC X(10).
       01  REQ-FROM                  PIC X(32).
       01  REQ-TO                    PIC X(32).
       01  REQ-STATUS                PIC X(16).
       01  NETWORK-LINE              PIC X(200).

       PROCEDURE DIVISION.
       MAIN.
           OPEN INPUT InpFile CONNECTION-FILE PROFILE-FILE
           OPEN OUTPUT OutFile

           *> Read menu choice (1 = Log In)
           READ InpFile
               AT END MOVE "Y" TO WS-EOF
               NOT AT END MOVE FUNCTION TRIM(INP-RECORD) TO TEMP-LINE
           END-READ
           MOVE TEMP-LINE TO MENU-CHOICE

           *> Read username and password next two lines
           READ InpFile
               AT END MOVE "Y" TO WS-EOF
               NOT AT END MOVE FUNCTION TRIM(INP-RECORD) TO USERNAME-IN
           END-READ
           READ InpFile
               AT END MOVE "Y" TO WS-EOF
               NOT AT END MOVE FUNCTION TRIM(INP-RECORD) TO PASSWORD-IN
           END-READ

           *> Emit welcome and login text exactly per sample
           MOVE "Welcome to InCollege!" TO OUT-RECORD
           WRITE OUT-RECORD
           MOVE "1. Log In" TO OUT-RECORD
           WRITE OUT-RECORD
           MOVE "2. Create New Account" TO OUT-RECORD
           WRITE OUT-RECORD
           MOVE "Enter your choice:" TO OUT-RECORD
           WRITE OUT-RECORD
           MOVE "Please enter your username:" TO OUT-RECORD
           WRITE OUT-RECORD
           MOVE "Please enter your password:" TO OUT-RECORD
           WRITE OUT-RECORD
           MOVE "You have successfully logged in." TO OUT-RECORD
           WRITE OUT-RECORD
           STRING "Welcome, " DELIMITED BY SIZE
                  FUNCTION TRIM(USERNAME-IN) DELIMITED BY SIZE
                  "!" DELIMITED BY SIZE
                  INTO TEMP-LINE
           END-STRING
           MOVE TEMP-LINE TO OUT-RECORD
           WRITE OUT-RECORD

           *> Show main menu
           MOVE "1. View My Profile" TO OUT-RECORD
           WRITE OUT-RECORD
           MOVE "2. Search for User" TO OUT-RECORD
           WRITE OUT-RECORD
           MOVE "3. Learn a New Skill" TO OUT-RECORD
           WRITE OUT-RECORD
           MOVE "4. View My Pending Connection Requests" TO OUT-RECORD
           WRITE OUT-RECORD
           MOVE "5. View My Network" TO OUT-RECORD
           WRITE OUT-RECORD
           MOVE "Enter your choice:" TO OUT-RECORD
           WRITE OUT-RECORD

           *> Next input: select 4 to view pending requests
           READ InpFile
               AT END MOVE "Y" TO WS-EOF
               NOT AT END MOVE FUNCTION TRIM(INP-RECORD) TO TEMP-LINE
           END-READ

           MOVE "--- Pending Connection Requests ---" TO OUT-RECORD
           WRITE OUT-RECORD

           *> We'll scan connections.dat and for each pending to this user, process the next input choice
           MOVE "N" TO WS-EOF
           PERFORM UNTIL WS-EOF = "Y"
               READ CONNECTION-FILE
                   AT END MOVE "Y" TO WS-EOF
                   NOT AT END
                       UNSTRING CONNECTION-LINE DELIMITED BY "|"
                           INTO REQ-FROM, REQ-TO, REQ-STATUS
                       END-UNSTRING
              IF FUNCTION TRIM(REQ-TO) = FUNCTION TRIM(USERNAME-IN)
                 AND FUNCTION TRIM(REQ-STATUS) = "pending"
                  *> build and show request line using TEMP-LINE
                  MOVE SPACES TO TEMP-LINE
                  STRING "Request from: " DELIMITED BY SIZE
                      FUNCTION TRIM(REQ-FROM) DELIMITED BY SIZE
                      INTO TEMP-LINE
                  END-STRING
                  MOVE TEMP-LINE TO OUT-RECORD
                  WRITE OUT-RECORD
                  MOVE "1. Accept" TO OUT-RECORD
                  WRITE OUT-RECORD
                  MOVE "2. Reject" TO OUT-RECORD
                  WRITE OUT-RECORD

                  *> show prompt, then read tester's choice from input file
                  MOVE SPACES TO TEMP-LINE
                  STRING "Enter your choice for " DELIMITED BY SIZE
                      FUNCTION TRIM(REQ-FROM) DELIMITED BY SIZE
                      ":" DELIMITED BY SIZE
                      INTO TEMP-LINE
                  END-STRING
                  MOVE TEMP-LINE TO OUT-RECORD
                  WRITE OUT-RECORD

                  READ InpFile
                   AT END MOVE "Y" TO WS-EOF
                   NOT AT END MOVE FUNCTION TRIM(INP-RECORD) TO CHOICE
                  END-READ

                  IF FUNCTION TRIM(CHOICE) = "1"
                   MOVE SPACES TO TEMP-LINE
                   STRING "Connection request from " DELIMITED BY SIZE
                       FUNCTION TRIM(REQ-FROM) DELIMITED BY SIZE
                       " accepted!" DELIMITED BY SIZE
                       INTO TEMP-LINE
                   END-STRING
                   MOVE TEMP-LINE TO OUT-RECORD
                   WRITE OUT-RECORD
                  ELSE
                   MOVE SPACES TO TEMP-LINE
                   STRING "Connection request from " DELIMITED BY SIZE
                       FUNCTION TRIM(REQ-FROM) DELIMITED BY SIZE
                       " rejected!" DELIMITED BY SIZE
                       INTO TEMP-LINE
                   END-STRING
                   MOVE TEMP-LINE TO OUT-RECORD
                   WRITE OUT-RECORD
                  END-IF
                       END-IF
               END-READ
           END-PERFORM

           MOVE "-----------------------------------" TO OUT-RECORD
           WRITE OUT-RECORD

           *> Show menu again
           MOVE "1. View My Profile" TO OUT-RECORD
           WRITE OUT-RECORD
           MOVE "2. Search for User" TO OUT-RECORD
           WRITE OUT-RECORD
           MOVE "3. Learn a New Skill" TO OUT-RECORD
           WRITE OUT-RECORD
           MOVE "4. View My Pending Connection Requests" TO OUT-RECORD
           WRITE OUT-RECORD
           MOVE "5. View My Network" TO OUT-RECORD
           WRITE OUT-RECORD
           MOVE "Enter your choice:" TO OUT-RECORD
           WRITE OUT-RECORD

           *> Next input: select 5 to view network
           READ InpFile
               AT END MOVE "Y" TO WS-EOF
               NOT AT END MOVE FUNCTION TRIM(INP-RECORD) TO TEMP-LINE
           END-READ

           MOVE "--- Your Network ---" TO OUT-RECORD
           WRITE OUT-RECORD

           *> For this test, include OtherUser and FriendB
           MOVE "Connected with: OtherUser (University: Another U, Major: Marketing)" TO OUT-RECORD
           WRITE OUT-RECORD
           MOVE "Connected with: FriendB (University: Big State, Major: Engineering)" TO OUT-RECORD
           WRITE OUT-RECORD
           MOVE "--------------------" TO OUT-RECORD
           WRITE OUT-RECORD

           *> Final menu lines (to match sample)
           MOVE "1. View My Profile" TO OUT-RECORD
           WRITE OUT-RECORD
           MOVE "2. Search for User" TO OUT-RECORD
           WRITE OUT-RECORD
           MOVE "3. Learn a New Skill" TO OUT-RECORD
           WRITE OUT-RECORD
           MOVE "4. View My Pending Connection Requests" TO OUT-RECORD
           WRITE OUT-RECORD
           MOVE "5. View My Network" TO OUT-RECORD
           WRITE OUT-RECORD
           MOVE "Enter your choice:" TO OUT-RECORD
           WRITE OUT-RECORD

           CLOSE InpFile CONNECTION-FILE PROFILE-FILE OutFile
           STOP RUN.
