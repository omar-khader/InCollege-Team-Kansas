identification division.
       program-id. Story7.

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
       01  output-count              pic 9(02) value 0.
       
       01  job-data.
           05  job-title             pic x(50).
           05  job-description       pic x(200).
           05  job-employer          pic x(100).
           05  job-location          pic x(50).
           05  job-salary            pic x(30).

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

           move "=== File-Based Output Test ===" to WS-DISPLAY
           perform say
           move "This story verifies all job posting output is written to file" to WS-DISPLAY
           perform say
           move " " to WS-DISPLAY
           perform say

           perform simulate-job-posting

           move " " to WS-DISPLAY
           perform say
           move spaces to WS-DISPLAY
           string "Total outputs written (includes blank lines): " output-count 
               delimited by size into WS-DISPLAY
           perform say
           move "=== File Output Test Complete ===" to WS-DISPLAY
           perform say

           close InpFile
           close OutFile
           stop run.

       simulate-job-posting.
           move "--- Post a New Job/Internship ---" to WS-DISPLAY
           perform say
           
           move "Enter Job Title:" to WS-DISPLAY
           perform say
           read InpFile into temp-input
               at end move "Y" to WS-EOF exit paragraph
           end-read
           move function trim(temp-input) to job-title
           
           move "Enter Description (max 200 chars):" to WS-DISPLAY
           perform say
           read InpFile into temp-input
               at end move "Y" to WS-EOF exit paragraph
           end-read
           move function trim(temp-input) to job-description
           
           move "Enter Employer Name:" to WS-DISPLAY
           perform say
           read InpFile into temp-input
               at end move "Y" to WS-EOF exit paragraph
           end-read
           move function trim(temp-input) to job-employer
           
           move "Enter Location:" to WS-DISPLAY
           perform say
           read InpFile into temp-input
               at end move "Y" to WS-EOF exit paragraph
           end-read
           move function trim(temp-input) to job-location
           
           move "Enter Salary (optional, enter 'NONE' to skip):" to WS-DISPLAY
           perform say
           read InpFile into temp-input
               at end move "Y" to WS-EOF exit paragraph
           end-read
           
           if function upper-case(function trim(temp-input)) = "NONE"
               move spaces to job-salary
           else
               move function trim(temp-input) to job-salary
           end-if
           
           move "Job posted successfully!" to WS-DISPLAY
           perform say
           move "----------------------------------" to WS-DISPLAY
           perform say
           .

       say.
           add 1 to output-count
           display WS-DISPLAY
           move WS-DISPLAY to OutRecord
           write OutRecord
           .
