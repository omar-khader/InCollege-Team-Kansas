identification division.
       program-id. Story6.

       environment division.
       input-output section.
       file-control.
           select InpFile assign to "InCollege-Input.txt"
               organization is line sequential
               file status is FILESTAT.
           select OutFile assign to "InCollege-Output.txt"
               organization is line sequential
               file status is FILESTAT-Out.

       data division.
       file section.
       fd  InpFile.
       01  InpRecord                 pic x(200).

       fd  OutFile.
       01  OutRecord                 pic x(80).

       working-storage section.
       01  FILESTAT                  pic xx.
       01  FILESTAT-Out              pic xx.
       01  WS-EOF                    pic x value "N".
       01  WS-DISPLAY                pic x(80).
       01  temp-input                pic x(200).
       01  input-count               pic 9(02) value 0.

       procedure division.
       main.
           open input InpFile
           if FILESTAT not = "00"
              display "ERROR opening InCollege-Input.txt, status: " FILESTAT
              stop run
           end-if

           open output OutFile
           if FILESTAT-Out not = "00"
              display "ERROR opening InCollege-Output.txt, status: " FILESTAT-Out
              stop run
           end-if

           move "=== File-Based Input Test ===" to WS-DISPLAY
           perform say
           move "This story verifies all inputs are read from file" to WS-DISPLAY
           perform say
           move " " to WS-DISPLAY
           perform say

           perform read-job-inputs

           move " " to WS-DISPLAY
           perform say
           move spaces to WS-DISPLAY
           string "Total inputs read from file: " input-count 
               delimited by size into WS-DISPLAY
           perform say
           move "=== File Input Test Complete ===" to WS-DISPLAY
           perform say

           close InpFile
           close OutFile
           stop run.

       read-job-inputs.
           move "Reading Job Title from file..." to WS-DISPLAY
           perform say
           read InpFile into temp-input
               at end move "Y" to WS-EOF exit paragraph
           end-read
           add 1 to input-count
           move spaces to WS-DISPLAY
           string "  Input #" input-count ": " function trim(temp-input)
               delimited by size into WS-DISPLAY
           perform say
           
           move "Reading Description from file..." to WS-DISPLAY
           perform say
           read InpFile into temp-input
               at end move "Y" to WS-EOF exit paragraph
           end-read
           add 1 to input-count
           move spaces to WS-DISPLAY
           string "  Input #" input-count ": " function trim(temp-input)
               delimited by size into WS-DISPLAY
           perform say
           
           move "Reading Employer from file..." to WS-DISPLAY
           perform say
           read InpFile into temp-input
               at end move "Y" to WS-EOF exit paragraph
           end-read
           add 1 to input-count
           move spaces to WS-DISPLAY
           string "  Input #" input-count ": " function trim(temp-input)
               delimited by size into WS-DISPLAY
           perform say
           
           move "Reading Location from file..." to WS-DISPLAY
           perform say
           read InpFile into temp-input
               at end move "Y" to WS-EOF exit paragraph
           end-read
           add 1 to input-count
           move spaces to WS-DISPLAY
           string "  Input #" input-count ": " function trim(temp-input)
               delimited by size into WS-DISPLAY
           perform say
           
           move "Reading Salary from file..." to WS-DISPLAY
           perform say
           read InpFile into temp-input
               at end move "Y" to WS-EOF exit paragraph
           end-read
           add 1 to input-count
           move spaces to WS-DISPLAY
           string "  Input #" input-count ": " function trim(temp-input)
               delimited by size into WS-DISPLAY
           perform say
           .

       say.
           display WS-DISPLAY
           move WS-DISPLAY to OutRecord
           write OutRecord
           .
