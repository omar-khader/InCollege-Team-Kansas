identification division.
       program-id. Story4.

       environment division.
       input-output section.
       file-control.
           select job-file assign to "jobs.dat"
               organization is line sequential
               file status is FILESTAT-JOB.
           select InpFile assign to "InCollege-Input.txt"
               organization is line sequential
               file status is FILESTAT.
           select OutFile assign to "InCollege-Output.txt"
               organization is line sequential
               file status is FILESTAT-Out.

       data division.
       file section.
       fd  job-file.
       01  job-line                  pic x(500).

       fd  InpFile.
       01  InpRecord                 pic x(200).

       fd  OutFile.
       01  OutRecord                 pic x(80).

       working-storage section.
       01  FILESTAT                  pic xx.
       01  FILESTAT-JOB              pic xx.
       01  FILESTAT-Out              pic xx.
       01  WS-EOF                    pic x value "N".
       01  WS-DISPLAY                pic x(80).
       01  temp-input                pic x(200).
       01  job-count                 pic 9(03) value 0.
       01  ws-choice                 pic 9 value 0.

       01  job-data.
           05  job-poster-username    pic x(32).
           05  job-title              pic x(50).
           05  job-description        pic x(200).
           05  job-employer           pic x(100).
           05  job-location           pic x(50).
           05  job-salary             pic x(30).

       procedure division.
       main.
           open input job-file
           if FILESTAT-JOB = "35"  
              open output job-file
              close job-file
           end-if
           close job-file
           
           open input InpFile
           if FILESTAT not = "00"
              display "ERROR opening InCollege-Input.txt"
              stop run
           end-if

           open output OutFile
           if FILESTAT-Out not = "00"
              display "ERROR opening InCollege-Output.txt"
              stop run
           end-if

           move "=== Job Persistence Test ===" to WS-DISPLAY
           perform say
           move "1. Save Job" to WS-DISPLAY
           perform say
           move "2. Load and Display Jobs" to WS-DISPLAY
           perform say
           move "Enter choice:" to WS-DISPLAY
           perform say
           
           read InpFile into InpRecord
               at end move "Y" to WS-EOF
           end-read
           
           if WS-EOF = "N"
               move function numval(function trim(InpRecord)) to ws-choice
               if ws-choice = 1
                   perform save-test-job
               else
                   perform load-and-display-jobs
               end-if
           end-if

           close InpFile
           close OutFile
           stop run.

       save-test-job.
           move "testuser" to job-poster-username
           
           move "Enter Job Title:" to WS-DISPLAY
           perform say
           read InpFile into temp-input
               at end move "Y" to WS-EOF exit paragraph
           end-read
           move function trim(temp-input) to job-title
           
           move "Enter Description:" to WS-DISPLAY
           perform say
           read InpFile into temp-input
               at end move "Y" to WS-EOF exit paragraph
           end-read
           move function trim(temp-input) to job-description
           
           move "Enter Employer:" to WS-DISPLAY
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
           
           move "Enter Salary (or NONE):" to WS-DISPLAY
           perform say
           read InpFile into temp-input
               at end move "Y" to WS-EOF exit paragraph
           end-read
           
           if function upper-case(function trim(temp-input)) = "NONE"
               move spaces to job-salary
           else
               move function trim(temp-input) to job-salary
           end-if
           
           perform save-job-to-file
           move "Job saved to persistent storage!" to WS-DISPLAY
           perform say
           .

       save-job-to-file.
           open extend job-file
           if FILESTAT-JOB not = "00"
               open output job-file
               close job-file
               open extend job-file
           end-if
           
           move spaces to job-line
           string 
               function trim(job-poster-username) "|"
               function trim(job-title) "|"
               function trim(job-description) "|"
               function trim(job-employer) "|"
               function trim(job-location) "|"
               function trim(job-salary)
               delimited by size
               into job-line
           end-string
           
           write job-line
           close job-file
           .

       load-and-display-jobs.
           move 0 to job-count
           move "=== Loading Jobs from Persistent Storage ===" to WS-DISPLAY
           perform say
           
           open input job-file
           if FILESTAT-JOB = "00"
               perform until 1 = 2
                   read job-file into job-line
                       at end exit perform
                   end-read
                   add 1 to job-count
                   move spaces to WS-DISPLAY
                   string "Job #" job-count ": " function trim(job-line)
                       delimited by size into WS-DISPLAY
                   perform say
               end-perform
               close job-file
           end-if
           
           if job-count = 0
               move "No jobs found in storage." to WS-DISPLAY
               perform say
           else
               move spaces to WS-DISPLAY
               string "Total jobs loaded: " job-count 
                   delimited by size into WS-DISPLAY
               perform say
           end-if
           .

       say.
           display WS-DISPLAY
           move WS-DISPLAY to OutRecord
           write OutRecord
           .

