IDENTIFICATION DIVISION.
       PROGRAM-ID. PROFILE-VIEWING-TEST.
       AUTHOR. InCollege-Team-Kansas.

       ENVIRONMENT DIVISION.
       INPUT-OUTPUT SECTION.
       FILE-CONTROL.

           *> Define the output and data files for the test
           SELECT InpFile ASSIGN TO 'StoryThree-Input.txt'
               ORGANIZATION IS LINE SEQUENTIAL
               ACCESS MODE IS SEQUENTIAL
               FILE STATUS IS WS-INPFILE-STATUS.

           SELECT OutFile ASSIGN TO 'StoryThree-Output.txt'
               ORGANIZATION IS LINE SEQUENTIAL
               ACCESS MODE IS SEQUENTIAL.

           SELECT USER-FILE ASSIGN TO "userprof.dat"
           ORGANIZATION IS SEQUENTIAL
           ACCESS MODE IS SEQUENTIAL
           FILE STATUS IS WS-USERFILE-STATUS.

       DATA DIVISION.
       FILE SECTION.

       FD  InpFile.
       01  Inp-Record-Area           PIC X(80).

       FD  OutFile.
       01  Out-Record-Area           PIC X(80).

       FD USER-FILE.
       01 USER-RECORD.
           05 prof-username        PIC X(32).
           05 prof-first-name      PIC X(50).
           05 prof-last-name       PIC X(50).
           05 prof-university      PIC X(50).
           05 prof-major           PIC X(30).
           05 prof-grad-year       PIC 9(4).
           05 prof-about-me        PIC X(200).

           05  prof-exp-count      PIC 9.
           05  prof-experience     occurs 3 times.
               10  exp-title       pic x(50).
               10  exp-company     pic x(50).
               10  exp-dates       pic x(30).
               10  exp-description pic x(100).

           05  prof-edu-count      PIC 9.
           05  prof-education      occurs 3 times.
               10  edu-degree      pic x(50).
               10  edu-university  pic x(100).
               10  edu-years       pic x(30).

       WORKING-STORAGE SECTION.

       *> Define a hardcoded current user for this test program
       01  current-user            PIC X(32) VALUE 'Test.Username'.

       01  WS-DISPLAY              PIC X(80).
       01 WS-USERFILE-STATUS          PIC XX.
       01 WS-INPFILE-STATUS          PIC XX.

       01 WS-USER-FOUND            PIC X VALUE 'N'.
       01 WS-EOF                   PIC X VALUE 'N'.

       01 WS-USER-RECORD.
           05 ws-prof-username        PIC X(32).
           05 ws-prof-first-name      PIC X(50).
           05 ws-prof-last-name       PIC X(50).
           05 ws-prof-university      PIC X(50).
           05 ws-prof-major           PIC X(30).
           05 ws-prof-grad-year       PIC 9(4).
           05 ws-prof-about-me        PIC X(200).

           05  ws-prof-exp-count      PIC 9.
           05  ws-prof-experience     occurs 3 times.
               10  ws-exp-title       pic x(50).
               10  ws-exp-company     pic x(50).
               10  ws-exp-dates       pic x(30).
               10  ws-exp-description pic x(100).

           05  ws-prof-edu-count      PIC 9.
           05  ws-prof-education      occurs 3 times.
               10  ws-edu-degree      pic x(50).
               10  ws-edu-university  pic x(100).
               10  ws-edu-years       pic x(30).

       PROCEDURE DIVISION.
       MAIN.
           OPEN INPUT InpFile, OUTPUT OutFile.

           perform post-login-profile-retrieval
           perform display-user-profile

           close OutFile, InpFile.
           STOP RUN.

       say.
           *> This paragraph handles displaying output to the screen and the file
           DISPLAY WS-DISPLAY.
           WRITE Out-Record-Area FROM WS-DISPLAY.

       post-login-profile-retrieval.
           *> This paragraph will retrieve a user's profile from userprof.dat
           *> based off the inputted username. (Case Sensitive)

           open input USER-FILE
           if WS-USERFILE-STATUS NOT = '00'
               string "Error opening file. Status: " WS-USERFILE-STATUS
               delimited by size into WS-DISPLAY
               perform say
               close USER-FILE
               stop run
           end-if.

           string "Searching for " function trim(current-user) " Profile Data"
           delimited by size into WS-DISPLAY
           perform say
           perform until WS-EOF = 'Y' OR WS-USER-FOUND = 'Y'
               read USER-FILE into WS-USER-RECORD
               evaluate WS-USERFILE-STATUS
                   when "00"
                       if ws-prof-username = current-user
                           move "Y" to WS-USER-FOUND
                       end-if
                   when "10"
                       move "Y" to WS-EOF
                   when other
                       string "Error reading file. Status: " WS-USERFILE-STATUS
                       delimited by size into WS-DISPLAY
                       perform say
                       move "Y" to WS-EOF
               end-evaluate
           END-PERFORM

           IF WS-USER-FOUND = 'Y'
               move "User successfully found" to WS-DISPLAY
               perform say
           ELSE
               string "User '" current-user "' not found."
               delimited by size into WS-DISPLAY
               perform say
           END-IF

           *> Explicitly close the USER-FILE to avoid warnings
           close USER-FILE.

       display-user-profile.

           *> This paragraph prints out profile using previously retrieved data
           string "-----" function trim(current-user) "-----" into WS-DISPLAY
           perform say

           move spaces to WS-DISPLAY
           string "Name: " function trim(ws-prof-first-name) " "
           function trim(ws-prof-last-name) into WS-DISPLAY
           perform say

           move spaces to WS-DISPLAY
           string "University: " function trim(ws-prof-university) into WS-DISPLAY
           perform say

           move spaces to WS-DISPLAY
           string "Major: " function trim(ws-prof-major) into WS-DISPLAY
           perform say

           move spaces to WS-DISPLAY
           string "Graduation Year: " function trim(ws-prof-grad-year) into WS-DISPLAY
           perform say

           move spaces to WS-DISPLAY
           string "About me: " function trim(ws-prof-about-me) into WS-DISPLAY
           perform say

           move "Experience:" to WS-DISPLAY
           perform say

           move spaces to WS-DISPLAY
           string "  Title: " function trim(ws-exp-title(1)) into WS-DISPLAY
           perform say

           move spaces to WS-DISPLAY
           string "  Company: " function trim(ws-exp-company(1)) into WS-DISPLAY
           perform say

           move spaces to WS-DISPLAY
           string "  Dates: " function trim(ws-exp-dates(1)) into WS-DISPLAY
           perform say

           move spaces to WS-DISPLAY
           string "  Description: " function trim(ws-exp-description(1)) into WS-DISPLAY
           perform say

           move "Education:" to WS-DISPLAY
           perform say

           move spaces to WS-DISPLAY
           string "  Degree: " function trim(ws-edu-degree(1)) into WS-DISPLAY
           perform say

           move spaces to WS-DISPLAY
           string "  University: " function trim(ws-edu-university(1)) into WS-DISPLAY
           perform say

           move spaces to WS-DISPLAY
           string "  Years: " function trim(ws-edu-years(1)) into WS-DISPLAY
           perform say
           .


