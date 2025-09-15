IDENTIFICATION DIVISION.
       PROGRAM-ID. EXPERIENCE-STANDALONE-TEST.
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
       01  Inp-Record-Area           PIC X(200).

       FD  OutFile.
       01  Out-Record-Area           PIC X(200).

       WORKING-STORAGE SECTION.
       01  WS-DISPLAY              PIC X(200).
       01  WS-EOF                  PIC X(1) VALUE 'N'.
           88  END-OF-FILE                VALUE 'Y'.


       01  experience-data.
           05  prof-exp-count      PIC 9.
           05  prof-experience     occurs 3 times.
               10  exp-title       pic x(50).
               10  exp-company     pic x(50).
               10  exp-dates       pic x(30).
               10  exp-description pic x(100).

       01  temp-exp-input          PIC X(200).
       01  ws-exp-index            PIC 9.
       01  ws-entry-number         PIC 9.

       PROCEDURE DIVISION.
       MAIN-LOGIC-SECTION.
           OPEN INPUT InpFile, OUTPUT OutFile.

           PERFORM get-experience-entries.
           IF NOT END-OF-FILE
               PERFORM validate-experience-entries
               PERFORM display-experience-entries
           END-IF.

           CLOSE InpFile, OutFile.
           STOP RUN.
       
       get-experience-entries.
           initialize experience-data
           move 0 to prof-exp-count

           perform until prof-exp-count >= 3
               move "Add Experience (optional, max 3 entries. Enter 'DONE' to finish):" to WS-DISPLAY
               perform say

               add 1 to prof-exp-count
               move prof-exp-count to ws-exp-index
               move ws-exp-index to ws-entry-number
               
               move spaces to WS-DISPLAY
               string "Experience #" ws-entry-number " - Title:"
                   delimited by size into WS-DISPLAY
               perform say

               read InpFile into temp-exp-input
                   at end
                       move "Y" to WS-EOF
                       exit paragraph
               end-read

               if function upper-case(function trim(temp-exp-input)) = "DONE"
                   subtract 1 from prof-exp-count
                   exit perform
               end-if

               move function trim(temp-exp-input) to exp-title(ws-exp-index)

               move spaces to WS-DISPLAY
               string "Experience #" ws-exp-index " - Company/Organization:"
                   delimited by size into WS-DISPLAY
               perform say
               read InpFile into temp-exp-input
                   at end
                       move "Y" to WS-EOF
                       exit paragraph
               end-read
               move function trim(temp-exp-input) to exp-company(ws-exp-index)

               move spaces to WS-DISPLAY
               string "Experience #" ws-exp-index " - Dates (e.g., Summer 2024):"
                   delimited by size into WS-DISPLAY
               perform say
               read InpFile into temp-exp-input
                   at end
                       move "Y" to WS-EOF
                       exit paragraph
               end-read
               move function trim(temp-exp-input) to exp-dates(ws-exp-index)

               move spaces to WS-DISPLAY
               string "Experience #" ws-exp-index " - Description (optional, max 100 chars, blank to skip):"
                   delimited by size into WS-DISPLAY
               perform say
               read InpFile into temp-exp-input
                   at end
                       move "Y" to WS-EOF
                       exit paragraph
               end-read

               if function length(function trim(temp-exp-input)) > 0
                   if function length(function trim(temp-exp-input)) <= 100
                       move function trim(temp-exp-input) to exp-description(ws-exp-index)
                   else
                       move temp-exp-input(1:100) to exp-description(ws-exp-index)
                   end-if
               else
                   move spaces to exp-description(ws-exp-index)
               end-if
           end-perform.

       validate-experience-entries.
           if prof-exp-count > 3
               move "Maximum 3 experience entries allowed." to WS-DISPLAY
               perform say
               move 3 to prof-exp-count
           end-if

           perform varying ws-exp-index from 1 by 1
                   until ws-exp-index > prof-exp-count
               if function length(function trim(exp-title(ws-exp-index))) = 0
                   move spaces to WS-DISPLAY
                   string "Experience #" ws-exp-index " title is required."
                       delimited by size into WS-DISPLAY
                   perform say
               end-if

               if function length(function trim(exp-company(ws-exp-index))) = 0
                   move spaces to WS-DISPLAY
                   string "Experience #" ws-exp-index " company is required."
                       delimited by size into WS-DISPLAY
                   perform say
               end-if

               if function length(function trim(exp-dates(ws-exp-index))) = 0
                   move spaces to WS-DISPLAY
                   string "Experience #" ws-exp-index " dates are required."
                       delimited by size into WS-DISPLAY
                   perform say
               end-if
           end-perform.

       display-experience-entries.
           if prof-exp-count > 0
               move "Experience:" to WS-DISPLAY
               perform say
               
               perform varying ws-exp-index from 1 by 1
                       until ws-exp-index > prof-exp-count
                   move spaces to WS-DISPLAY
                   string "Title: " function trim(exp-title(ws-exp-index))
                       delimited by size into WS-DISPLAY
                   perform say
                   
                   move spaces to WS-DISPLAY
                   string "Company: " function trim(exp-company(ws-exp-index))
                       delimited by size into WS-DISPLAY
                   perform say
                   
                   move spaces to WS-DISPLAY
                   string "Dates: " function trim(exp-dates(ws-exp-index))
                       delimited by size into WS-DISPLAY
                   perform say
                   
                   if function length(function trim(exp-description(ws-exp-index))) > 0
                       move spaces to WS-DISPLAY
                       string "Description: " function trim(exp-description(ws-exp-index))
                           delimited by size into WS-DISPLAY
                       perform say
                   end-if
               end-perform
           else
               move "Experience: (None provided)" to WS-DISPLAY
               perform say
           end-if.

       say.
           DISPLAY WS-DISPLAY.
           WRITE Out-Record-Area FROM WS-DISPLAY.
