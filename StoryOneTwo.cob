IDENTIFICATION DIVISION.
       PROGRAM-ID. PROFILE-STANDALONE-TEST.
       AUTHOR. InCollege Team.

       ENVIRONMENT DIVISION.
       INPUT-OUTPUT SECTION.
       FILE-CONTROL.
           *> Define the input and output files for the test
           SELECT InpFile ASSIGN TO 'StoryOneTwo-Input.txt'
               ORGANIZATION IS LINE SEQUENTIAL
               ACCESS MODE IS SEQUENTIAL.
           SELECT OutFile ASSIGN TO 'StoryOneTwo-Output.txt'
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

       01  WS-USER-CHOICE            pic 9 value 0.
       01  WS-EOF                  PIC X(1) VALUE 'N'.
           88  END-OF-FILE                VALUE 'Y'.

      



       PROCEDURE DIVISION.
       MAIN-LOGIC-SECTION.
           OPEN INPUT InpFile, OUTPUT OutFile.

           PERFORM post-login-menu.

           CLOSE InpFile, OutFile.
           STOP RUN.

       post-login-menu.
        perform until WS-EOF = "Y"
        move "1. Create/Edit my Profile" to WS-DISPLAY
        perform say

        move "2. View your profile" to WS-DISPLAY
        perform say

        move "3. Search for a job" to WS-DISPLAY
        perform say

        move "4. Find someone you know" to WS-DISPLAY
        perform say

        move "5. Learn a new skill" to WS-DISPLAY
        perform say

        move "Enter your choice:" to WS-DISPLAY
        perform say

        read InpFile into Inp-Record-Area
            at end move "Y" to WS-EOF
            not at end
                move function numval(function trim(Inp-Record-Area))
                    to WS-USER-CHOICE
        end-read

        if WS-EOF = "N"
            evaluate WS-USER-CHOICE
                when 1
                    move "Profile Creation/Editing to be implemented"
                        to WS-DISPLAY
                    perform say
                when 2
                    move "Profile viewing to be implemented"
                        to WS-DISPLAY
                    perform say
                when 3
                    move "Job search/internship is under construction."
                        to WS-DISPLAY
                    perform say
                when 4
                    move "Find someone you know is under construction."
                        to WS-DISPLAY
                    perform say
                when 5
                    move "Skills not implemented in this test file" to WS-DISPLAY
                    perform say
            end-evaluate
        end-if
    end-perform
    .



       say.
           *> This paragraph handles displaying output to the screen and the file
           DISPLAY WS-DISPLAY.
           WRITE Out-Record-Area FROM WS-DISPLAY.
