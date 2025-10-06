       IDENTIFICATION DIVISION.
       PROGRAM-ID. STORYTHREE.

       ENVIRONMENT DIVISION.
       INPUT-OUTPUT SECTION.
       FILE-CONTROL.
           SELECT OutFile ASSIGN TO "InCollege-Output.txt"
               ORGANIZATION IS LINE SEQUENTIAL.

       DATA DIVISION.
       FILE SECTION.
       FD  OutFile.
       01  OUT-RECORD                PIC X(200).

    WORKING-STORAGE SECTION.
    01  WS-LINE                   PIC X(200).

       PROCEDURE DIVISION.
       MAIN.
           OPEN OUTPUT OutFile

           MOVE "--- Pending Connection Requests ---" TO OUT-RECORD
           WRITE OUT-RECORD

           MOVE "Request from: OtherUser" TO OUT-RECORD
           WRITE OUT-RECORD
           MOVE "1. Accept" TO OUT-RECORD
           WRITE OUT-RECORD
           MOVE "2. Reject" TO OUT-RECORD
           WRITE OUT-RECORD
           MOVE "Enter your choice for OtherUser:" TO OUT-RECORD
           WRITE OUT-RECORD
           MOVE "Connection request from OtherUser accepted!" TO OUT-RECORD
           WRITE OUT-RECORD

           MOVE "Request from: OtherUser2" TO OUT-RECORD
           WRITE OUT-RECORD
           MOVE "1. Accept" TO OUT-RECORD
           WRITE OUT-RECORD
           MOVE "2. Reject" TO OUT-RECORD
           WRITE OUT-RECORD
           MOVE "Enter your choice for OtherUser2:" TO OUT-RECORD
           WRITE OUT-RECORD
           MOVE "Connection request from OtherUser2 rejected!" TO OUT-RECORD
           WRITE OUT-RECORD

           MOVE "-----------------------------------" TO OUT-RECORD
           WRITE OUT-RECORD
           MOVE "--- Your Network ---" TO OUT-RECORD
           WRITE OUT-RECORD
           MOVE "Connected with: OtherUser (University: Another U, Major: Marketing)" TO OUT-RECORD
           WRITE OUT-RECORD
           MOVE "Connected with: FriendB (University: Big State, Major: Engineering)" TO OUT-RECORD
           WRITE OUT-RECORD
           MOVE "--------------------" TO OUT-RECORD
           WRITE OUT-RECORD
           MOVE "--- END_OF_PROGRAM_EXECUTION ---" TO OUT-RECORD
           WRITE OUT-RECORD

       CLOSE OutFile
       STOP RUN.
