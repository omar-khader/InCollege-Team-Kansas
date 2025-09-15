IDENTIFICATION DIVISION.
       PROGRAM-ID. ABOUT-ME-STANDALONE-TEST.
       AUTHOR. InCollege Team.

       ENVIRONMENT DIVISION.
       INPUT-OUTPUT SECTION.
       FILE-CONTROL.
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

       01  about-me-data.
           05  prof-about-me       PIC X(80).

       01  temp-about-input        PIC X(80).

       PROCEDURE DIVISION.
       MAIN-LOGIC-SECTION.
           OPEN INPUT InpFile, OUTPUT OutFile.
           
           PERFORM get-about-me-section.
           IF NOT END-OF-FILE
               PERFORM validate-about-me-section
               PERFORM display-about-me-section
           END-IF.
           
           CLOSE InpFile, OutFile.
           STOP RUN.
       
       get-about-me-section.
           initialize about-me-data
           move "Enter About Me (optional, max 80 chars, enter blank line to skip):" to WS-DISPLAY
           perform say
           
           read InpFile into Inp-Record-Area
               at end
                   move "Y" to WS-EOF
                   exit paragraph
           end-read
           
           if function length(function trim(Inp-Record-Area)) > 0
               if function length(function trim(Inp-Record-Area)) <= 80
                   move function trim(Inp-Record-Area) to prof-about-me
               else
                   move Inp-Record-Area(1:80) to prof-about-me
                   move "About Me section truncated to 200 characters." to WS-DISPLAY
                   perform say
               end-if
           else
               move spaces to prof-about-me
           end-if.

       validate-about-me-section.
           if function length(function trim(prof-about-me)) > 80
               move "About Me section must be 200 characters or less." to WS-DISPLAY
               perform say
               exit paragraph
           end-if.

       display-about-me-section.
           if function length(function trim(prof-about-me)) > 0
               move spaces to WS-DISPLAY
               string "About Me: " function trim(prof-about-me) delimited by size into WS-DISPLAY
               perform say
           else
               move "About Me: (Not provided)" to WS-DISPLAY
               perform say
           end-if.

       say.
           DISPLAY WS-DISPLAY.
           WRITE Out-Record-Area FROM WS-DISPLAY.
