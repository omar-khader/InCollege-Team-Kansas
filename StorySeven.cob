       IDENTIFICATION DIVISION.
       PROGRAM-ID. EDUCATION-STANDALONE-TEST.
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

       01  education-data.
           05  prof-edu-count      PIC 9.
           05  prof-education      occurs 3 times.
               10  edu-degree      pic x(50).
               10  edu-university  pic x(100).
               10  edu-years       pic x(30).

       01  temp-edu-input          PIC X(200).
       01  ws-edu-index            PIC 9.
       01  ws-entry-number         PIC 9.

       PROCEDURE DIVISION.
       MAIN-LOGIC-SECTION.
           OPEN INPUT InpFile, OUTPUT OutFile.

           PERFORM get-education-entries.
           IF NOT END-OF-FILE
               PERFORM validate-education-entries
               PERFORM display-education-entries
           END-IF.

           CLOSE InpFile, OutFile.
           STOP RUN.
       
       get-education-entries.
           initialize education-data
           move 0 to prof-edu-count

           perform until prof-edu-count >= 3
               move "Add Education (optional, max 3 entries. Enter 'DONE' to finish):" to WS-DISPLAY
               perform say

               add 1 to prof-edu-count
               move prof-edu-count to ws-edu-index
               move ws-edu-index to ws-entry-number
               
               move spaces to WS-DISPLAY
               string "Education #" ws-entry-number " - Degree:"
                   delimited by size into WS-DISPLAY
               perform say

               read InpFile into temp-edu-input
                   at end
                       move "Y" to WS-EOF
                       exit paragraph
               end-read

               if function upper-case(function trim(temp-edu-input)) = "DONE"
                   subtract 1 from prof-edu-count
                   exit perform
               end-if

               move function trim(temp-edu-input) to edu-degree(ws-edu-index)

               move spaces to WS-DISPLAY
               string "Education #" ws-edu-index " - University/College:"
                   delimited by size into WS-DISPLAY
               perform say
               read InpFile into temp-edu-input
                   at end
                       move "Y" to WS-EOF
                       exit paragraph
               end-read
               move function trim(temp-edu-input) to edu-university(ws-edu-index)

               move spaces to WS-DISPLAY
               string "Education #" ws-edu-index " - Years Attended (e.g., 2023-2025):"
                   delimited by size into WS-DISPLAY
               perform say
               read InpFile into temp-edu-input
                   at end
                       move "Y" to WS-EOF
                       exit paragraph
               end-read
               move function trim(temp-edu-input) to edu-years(ws-edu-index)
           end-perform.

       validate-education-entries.
           if prof-edu-count > 3
               move "Maximum 3 education entries allowed." to WS-DISPLAY
               perform say
               move 3 to prof-edu-count
           end-if

           perform varying ws-edu-index from 1 by 1
                   until ws-edu-index > prof-edu-count
               if function length(function trim(edu-degree(ws-edu-index))) = 0
                   move spaces to WS-DISPLAY
                   string "Education #" ws-edu-index " degree is required."
                       delimited by size into WS-DISPLAY
                   perform say
               end-if

               if function length(function trim(edu-university(ws-edu-index))) = 0
                   move spaces to WS-DISPLAY
                   string "Education #" ws-edu-index " university is required."
                       delimited by size into WS-DISPLAY
                   perform say
               end-if

               if function length(function trim(edu-years(ws-edu-index))) = 0
                   move spaces to WS-DISPLAY
                   string "Education #" ws-edu-index " years attended are required."
                       delimited by size into WS-DISPLAY
                   perform say
               end-if
           end-perform.

       display-education-entries.
           if prof-edu-count > 0
               move "Education:" to WS-DISPLAY
               perform say
               
               perform varying ws-edu-index from 1 by 1
                       until ws-edu-index > prof-edu-count
                   move spaces to WS-DISPLAY
                   string "Degree: " function trim(edu-degree(ws-edu-index))
                       delimited by size into WS-DISPLAY
                   perform say
                   
                   move spaces to WS-DISPLAY
                   string "University: " function trim(edu-university(ws-edu-index))
                       delimited by size into WS-DISPLAY
                   perform say
                   
                   move spaces to WS-DISPLAY
                   string "Years: " function trim(edu-years(ws-edu-index))
                       delimited by size into WS-DISPLAY
                   perform say
               end-perform
           else
               move "Education: (None provided)" to WS-DISPLAY
               perform say
           end-if.

       say.
           DISPLAY WS-DISPLAY.
           WRITE Out-Record-Area FROM WS-DISPLAY.
