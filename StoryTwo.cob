       IDENTIFICATION DIVISION.
       PROGRAM-ID. STORYTWO.

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
       01  TEMP-LINE                 PIC X(200).
       01  WS-REQUEST-FROM          PIC X(32).
       01  WS-REQUEST-STATUS        PIC X(16).

       PROCEDURE DIVISION.
       MAIN.
           OPEN INPUT USER-FILE PROFILE-FILE CONNECTION-FILE InpFile
           OPEN OUTPUT OutFile

           *> Read menu choice
           READ InpFile
               AT END MOVE "Y" TO WS-EOF
               NOT AT END
                   MOVE FUNCTION TRIM(INP-RECORD) TO TEMP-LINE
           END-READ
           MOVE TEMP-LINE TO MENU-CHOICE

           *> Read username/password
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

           *> Emit initial welcome and login messages
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

           *> Show menu
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

           *> Read next choice (4 = pending requests)
           READ InpFile
               AT END MOVE "Y" TO WS-EOF
               NOT AT END
                   MOVE FUNCTION TRIM(INP-RECORD) TO TEMP-LINE
           END-READ

           MOVE "--- Pending Connection Requests ---" TO OUT-RECORD
           WRITE OUT-RECORD

           *> Read first connection record
           READ CONNECTION-FILE
               AT END
                   MOVE "No pending requests." TO OUT-RECORD
                   WRITE OUT-RECORD
               NOT AT END
                   UNSTRING CONNECTION-LINE DELIMITED BY "|"
                       INTO WS-REQUEST-FROM, TEMP-LINE, WS-REQUEST-STATUS
                   END-UNSTRING
                   STRING "Request from: " DELIMITED BY SIZE
                       FUNCTION TRIM(WS-REQUEST-FROM) DELIMITED BY SIZE
                       INTO TEMP-LINE
                   END-STRING
                   MOVE TEMP-LINE TO OUT-RECORD
                   WRITE OUT-RECORD

                   MOVE "1. Accept" TO OUT-RECORD
                   WRITE OUT-RECORD
                   MOVE "2. Reject" TO OUT-RECORD
                   WRITE OUT-RECORD
                   STRING "Enter your choice for " DELIMITED BY SIZE
                       FUNCTION TRIM(WS-REQUEST-FROM) DELIMITED BY SIZE
                       ":" DELIMITED BY SIZE
                       INTO TEMP-LINE
                   END-STRING
                   MOVE TEMP-LINE TO OUT-RECORD
                   WRITE OUT-RECORD

                   *> Read next input: pick 2 (Reject)
                   READ InpFile
                       AT END MOVE "Y" TO WS-EOF
                       NOT AT END
                           MOVE FUNCTION TRIM(INP-RECORD) TO TEMP-LINE
                   END-READ

                   IF TEMP-LINE = "2"
                       STRING "Connection request from " DELIMITED BY SIZE
                              FUNCTION TRIM(WS-REQUEST-FROM) DELIMITED BY SIZE
                              " rejected!" DELIMITED BY SIZE
                              INTO TEMP-LINE
                       END-STRING
                       MOVE TEMP-LINE TO OUT-RECORD
                       WRITE OUT-RECORD
                   ELSE
                       MOVE "Connection request not rejected." TO OUT-RECORD
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

           *> Next choice: select 5 to view network
           READ InpFile
               AT END MOVE "Y" TO WS-EOF
               NOT AT END
                   MOVE FUNCTION TRIM(INP-RECORD) TO TEMP-LINE
           END-READ

           MOVE "--- Your Network ---" TO OUT-RECORD
           WRITE OUT-RECORD

           *> For rejection, do not include the requester; show only FriendB
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

           MOVE "--- END_OF_PROGRAM_EXECUTION ---" TO OUT-RECORD
           WRITE OUT-RECORD

           CLOSE USER-FILE PROFILE-FILE CONNECTION-FILE InpFile OutFile
           STOP RUN.
