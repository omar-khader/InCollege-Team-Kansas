IDENTIFICATION DIVISION.
       PROGRAM-ID. PROFILE-STANDALONE-TEST.
       AUTHOR. InCollege Team.

       ENVIRONMENT DIVISION.
       INPUT-OUTPUT SECTION.
       FILE-CONTROL.
           *> Define the input and output files for the test
           SELECT InpFile ASSIGN TO 'InCollege-Input.txt'
               ORGANIZATION IS LINE SEQUENTIAL
               ACCESS MODE IS SEQUENTIAL.
           SELECT OutFile ASSIGN TO 'InCollege-Output.txt'
               ORGANIZATION IS LINE SEQUENTIAL
               ACCESS MODE IS SEQUENTIAL.

       DATA DIVISION.
       FILE SECTION.
       FD  InpFile.
       01  Inp-Record-Area           PIC X(80).

       FD  OutFile.
       01  Out-Record-Area           PIC X(80).

       WORKING-STORAGE SECTION.
       01  WS-DISPLAY              PIC X(80).
       01  WS-EOF                  PIC X(1) VALUE 'N'.
           88  END-OF-FILE                VALUE 'Y'.

       *> Define a hardcoded current user for this test program
       01  current-user            PIC X(32) VALUE 'tester123'.

       *> Data structures for basic profile info (from your code)
       01  basic-profile-data.
           05  prof-username       PIC X(32).
           05  prof-first-name     PIC X(50).
           05  prof-last-name      PIC X(50).
           05  prof-university     PIC X(100).
           05  prof-major          PIC X(50).
           05  prof-grad-year      PIC 9(4).

       01  temp-input              PIC X(200).
       01  temp-year               PIC X(10).
       01  ws-year-num             PIC 9(4).
       01  ws-valid-year           PIC X VALUE "n".

       PROCEDURE DIVISION.
       MAIN-LOGIC-SECTION.
           OPEN INPUT InpFile, OUTPUT OutFile.
           
           PERFORM get-basic-profile-info.

           IF NOT END-OF-FILE
               PERFORM validate-basic-profile-info
               PERFORM display-basic-profile-info
           END-IF.
           
           CLOSE InpFile, OutFile.
           STOP RUN.

       get-basic-profile-info.
           *> Initialize basic profile data
           initialize basic-profile-data
           move current-user to prof-username
           
           move "Enter First Name:" to WS-DISPLAY
           perform say
           read InpFile into temp-input
               at end 
                   move "Y" to WS-EOF
                   exit paragraph
           end-read
           move function trim(temp-input) to prof-first-name

           move "Enter Last Name:" to WS-DISPLAY
           perform say
           read InpFile into temp-input
               at end 
                   move "Y" to WS-EOF
                   exit paragraph
           end-read
           move function trim(temp-input) to prof-last-name

           move "Enter University/College Attended:" to WS-DISPLAY
           perform say
           read InpFile into temp-input
               at end 
                   move "Y" to WS-EOF
                   exit paragraph
           end-read
           move function trim(temp-input) to prof-university

           move "Enter Major:" to WS-DISPLAY
           perform say
           read InpFile into temp-input
               at end 
                   move "Y" to WS-EOF
                   exit paragraph
           end-read
           move function trim(temp-input) to prof-major

           move "n" to ws-valid-year
           perform until ws-valid-year = "y"
               move "Enter Graduation Year (YYYY):" to WS-DISPLAY
               perform say
               read InpFile into temp-year
                   at end 
                       move "Y" to WS-EOF
                       exit paragraph
               end-read
               move function trim(temp-year) to temp-year
               if function test-numval(temp-year) = 0
                   move function numval(temp-year) to ws-year-num
                   if ws-year-num >= 1900 and ws-year-num <= 2100
                       move ws-year-num to prof-grad-year
                       move "y" to ws-valid-year
                   else
                       move "Please enter a valid 4-digit year." to WS-DISPLAY
                       perform say
                   end-if
               else
                   move "Please enter a valid 4-digit year." to WS-DISPLAY
                   perform say
               end-if
           end-perform.

       validate-basic-profile-info.
           if function length(function trim(prof-first-name)) = 0
               move "First Name is required." to WS-DISPLAY
               perform say
               exit paragraph
           end-if

           if function length(function trim(prof-last-name)) = 0
               move "Last Name is required." to WS-DISPLAY  
               perform say
               exit paragraph
           end-if

           if function length(function trim(prof-university)) = 0
               move "University/College is required." to WS-DISPLAY
               perform say
               exit paragraph
           end-if

           if function length(function trim(prof-major)) = 0
               move "Major is required." to WS-DISPLAY
               perform say  
               exit paragraph
           end-if

           if prof-grad-year = 0
               move "Graduation Year is required." to WS-DISPLAY
               perform say
               exit paragraph
           end-if.

       display-basic-profile-info.
           move spaces to WS-DISPLAY
           string "Name: " function trim(prof-first-name) " " 
                function trim(prof-last-name) delimited by size into WS-DISPLAY
           perform say

           move spaces to WS-DISPLAY
           string "University: " function trim(prof-university) delimited by size into WS-DISPLAY
           perform say

           move spaces to WS-DISPLAY  
           string "Major: " function trim(prof-major) delimited by size into WS-DISPLAY
           perform say

           move spaces to WS-DISPLAY
           string "Graduation Year: " prof-grad-year delimited by size into WS-DISPLAY
           perform say.

       say.
           *> This paragraph handles displaying output to the screen and the file
           DISPLAY WS-DISPLAY.
           WRITE Out-Record-Area FROM WS-DISPLAY.
