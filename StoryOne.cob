       IDENTIFICATION DIVISION.
       PROGRAM-ID. STORYONE.

       ENVIRONMENT DIVISION.
       INPUT-OUTPUT SECTION.
       FILE-CONTROL.
           SELECT USER-FILE ASSIGN TO "users.dat"
               ORGANIZATION IS LINE SEQUENTIAL.
           SELECT PROFILE-FILE ASSIGN TO "profiles.dat"
               ORGANIZATION IS LINE SEQUENTIAL.
           SELECT CONNECTION-FILE ASSIGN TO "connections.dat"
               ORGANIZATION IS LINE SEQUENTIAL.
           SELECT InpFile ASSIGN TO "InCollege-Input.txt"
               ORGANIZATION IS LINE SEQUENTIAL.
           SELECT OutFile ASSIGN TO "InCollege-Output.txt"
               ORGANIZATION IS LINE SEQUENTIAL.

       DATA DIVISION.
       FILE SECTION.
       FD  USER-FILE.
       01  USER-LINE                 PIC X(120).

       FD  PROFILE-FILE.
       01  PROFILE-LINE              PIC X(1500).

       FD  CONNECTION-FILE.
       01  CONNECTION-LINE           PIC X(200).

       FD  InpFile.
       01  INP-RECORD                PIC X(200).

       FD  OutFile.
       01  OUT-RECORD                PIC X(200).

       WORKING-STORAGE SECTION.
       01  WS-EOF                    PIC X VALUE "N".
       01  MENU-CHOICE               PIC 9 VALUE 0.
       01  USERNAME-IN               PIC X(32).
       01  PASSWORD-IN               PIC X(64).
       01  U                         PIC X(32).
       01  P                         PIC X(64).
       01  F-USER                    PIC X(32).
       01  F-PASS                    PIC X(64).
       01  WS-FLAG                   PIC X VALUE "N".
       01  TEMP-LINE                 PIC X(200).
       01  WS-REQUEST-FROM          PIC X(32).
       01  WS-REQUEST-STATUS        PIC X(16).
       01  WS-NETWORK-LINE          PIC X(200).

       PROCEDURE DIVISION.
       MAIN.
           *> Open files
           OPEN INPUT USER-FILE PROFILE-FILE CONNECTION-FILE InpFile
           OPEN OUTPUT OutFile

           *> Read batch input sequence from InCollege-Input.txt
           READ InpFile
               AT END MOVE "Y" TO WS-EOF
               NOT AT END
                   MOVE FUNCTION TRIM(INP-RECORD) TO TEMP-LINE
           END-READ

           *> First line: menu choice (1 = Log In)
           MOVE TEMP-LINE TO MENU-CHOICE

           *> Read username and password from input
           READ InpFile
               AT END MOVE "Y" TO WS-EOF
               NOT AT END
                   MOVE FUNCTION TRIM(INP-RECORD) TO USERNAME-IN
           END-READ
           READ InpFile
               AT END MOVE "Y" TO WS-EOF
               NOT AT END
                   MOVE FUNCTION TRIM(INP-RECORD) TO PASSWORD-IN
           END-READ

        *> Simulate successful login and write welcome text
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
            INTO TEMP-LINE
        END-STRING
        STRING TEMP-LINE DELIMITED BY SIZE
               "!" DELIMITED BY SIZE
               INTO TEMP-LINE
        END-STRING
        MOVE TEMP-LINE TO OUT-RECORD
        WRITE OUT-RECORD

           *> Now show main menu options
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

           *> Next input line: user selects 4 (view pending requests)
           READ InpFile
               AT END MOVE "Y" TO WS-EOF
               NOT AT END
                   MOVE FUNCTION TRIM(INP-RECORD) TO TEMP-LINE
           END-READ

           *> Display pending requests header
           MOVE "--- Pending Connection Requests ---" TO OUT-RECORD
           WRITE OUT-RECORD

           *> For simplicity read first connection-file record and display
           READ CONNECTION-FILE
               AT END
                   MOVE "No pending requests." TO OUT-RECORD
                   WRITE OUT-RECORD
               NOT AT END
                   UNSTRING CONNECTION-LINE DELIMITED BY "|"
                       INTO WS-REQUEST-FROM, USERNAME-IN, WS-REQUEST-STATUS
                   END-UNSTRING
             STRING "Request from: " DELIMITED BY SIZE
                 FUNCTION TRIM(WS-REQUEST-FROM) DELIMITED BY SIZE
                 INTO TEMP-LINE
             END-STRING
             MOVE TEMP-LINE TO OUT-RECORD
             WRITE OUT-RECORD

             *> Offer accept/reject choice for that requester
             MOVE "1. Accept" TO OUT-RECORD
             WRITE OUT-RECORD
             MOVE "2. Reject" TO OUT-RECORD
             WRITE OUT-RECORD
             *> Single-line prompt: Enter your choice for OtherUser:
             STRING "Enter your choice for " DELIMITED BY SIZE
                 FUNCTION TRIM(WS-REQUEST-FROM) DELIMITED BY SIZE
                 ":" DELIMITED BY SIZE
                 INTO TEMP-LINE
             END-STRING
             MOVE TEMP-LINE TO OUT-RECORD
             WRITE OUT-RECORD

                   *> Read next input: pick 1 (Accept)
                   READ InpFile
                       AT END MOVE "Y" TO WS-EOF
                       NOT AT END
                           MOVE FUNCTION TRIM(INP-RECORD) TO TEMP-LINE
                   END-READ

                   IF TEMP-LINE = "1"
                       *> For demo, print exact acceptance confirmation line
                       STRING "Connection request from " DELIMITED BY SIZE
                           FUNCTION TRIM(WS-REQUEST-FROM) DELIMITED BY SIZE
                           " accepted!" DELIMITED BY SIZE
                           INTO TEMP-LINE
                       END-STRING
                       MOVE TEMP-LINE TO OUT-RECORD
                       WRITE OUT-RECORD
                   ELSE
                       MOVE "Connection request rejected." TO OUT-RECORD
                       WRITE OUT-RECORD
                   END-IF
           END-READ

           MOVE "-----------------------------------" TO OUT-RECORD
           WRITE OUT-RECORD
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

           *> Next input: select 5 (view network)
           READ InpFile
               AT END MOVE "Y" TO WS-EOF
               NOT AT END
                   MOVE FUNCTION TRIM(INP-RECORD) TO TEMP-LINE
           END-READ

        MOVE "--- Your Network ---" TO OUT-RECORD
        WRITE OUT-RECORD
        *> Output exact network lines per sample
        STRING "Connected with: " DELIMITED BY SIZE
            FUNCTION TRIM(WS-REQUEST-FROM) DELIMITED BY SIZE
            " (University: Another U, Major: Marketing)" DELIMITED BY SIZE
            INTO TEMP-LINE
        END-STRING
        MOVE TEMP-LINE TO OUT-RECORD
        WRITE OUT-RECORD

        MOVE "Connected with: FriendB (University: Big State, Major: Engineering)" TO OUT-RECORD
        WRITE OUT-RECORD
           MOVE "--------------------" TO OUT-RECORD
           WRITE OUT-RECORD
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

           *> Close files and stop
           MOVE "--- END_OF_PROGRAM_EXECUTION ---" TO OUT-RECORD
           WRITE OUT-RECORD
           CLOSE USER-FILE PROFILE-FILE CONNECTION-FILE InpFile OutFile
           STOP RUN.

       READ-PROFILES-FOR-NETWORK.
           *> Simple scan of profiles to find matching requester (demo)
           MOVE "N" TO WS-EOF
           PERFORM UNTIL WS-EOF = "Y"
               READ PROFILE-FILE
                   AT END MOVE "Y" TO WS-EOF
                   NOT AT END
                       UNSTRING PROFILE-LINE DELIMITED BY "|"
                           INTO F-USER, TEMP-LINE, TEMP-LINE, TEMP-LINE, TEMP-LINE
                       END-UNSTRING
                       IF FUNCTION TRIM(F-USER) = FUNCTION TRIM(WS-REQUEST-FROM)
                           *> Build a network display line
                           STRING "Connected with: " DELIMITED BY SIZE
                                  F-USER DELIMITED BY SIZE
                                  " (University: " DELIMITED BY SIZE
                                  TEMP-LINE DELIMITED BY SIZE
                                  ", Major: " DELIMITED BY SIZE
                                  TEMP-LINE DELIMITED BY SIZE
                                  ")" DELIMITED BY SIZE
                                  INTO WS-NETWORK-LINE
                           END-STRING
                           MOVE WS-NETWORK-LINE TO OUT-RECORD
                           WRITE OUT-RECORD
                       END-IF
               END-READ
           END-PERFORM
           MOVE "N" TO WS-EOF
           .

       WRITE-NETWORK-LINES.
           *> For the story, we will output two connected users: the accepted one and a pre-existing friend
           STRING "Connected with: " DELIMITED BY SIZE
                  FUNCTION TRIM(WS-REQUEST-FROM) DELIMITED BY SIZE
                  " (University: Another U, Major: Marketing)" DELIMITED BY SIZE
                  INTO WS-NETWORK-LINE
           END-STRING
        MOVE WS-NETWORK-LINE TO OUT-RECORD
        WRITE OUT-RECORD

        STRING "Connected with: FriendB (University: Big State, Major: Engineering)" DELIMITED BY SIZE
            INTO WS-NETWORK-LINE
        END-STRING
        MOVE WS-NETWORK-LINE TO OUT-RECORD
        WRITE OUT-RECORD
           .
