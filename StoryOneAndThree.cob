identification division.
       program-id. Story1and3.

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

           move "testuser" to job-poster-username
           perform post-job-internship

           close InpFile
           close OutFile
           stop run.

       post-job-internship.
           move "--- Post a New Job/Internship ---" to WS-DISPLAY
           perform say
           
           initialize job-data
           move "testuser" to job-poster-username
           
           move "Enter Job Title:" to WS-DISPLAY
           perform say
           read InpFile into temp-input
               at end move "Y" to WS-EOF exit paragraph
           end-read
           move function trim(temp-input) to job-title
           
           if function length(function trim(job-title)) = 0
               move "Job title is required." to WS-DISPLAY
               perform say
               exit paragraph
           end-if
           
           move "Enter Description (max 200 chars):" to WS-DISPLAY
           perform say
           read InpFile into temp-input
               at end move "Y" to WS-EOF exit paragraph
           end-read
           
           if function length(function trim(temp-input)) > 200
               move temp-input(1:200) to job-description
           else
               move function trim(temp-input) to job-description
           end-if
           
           if function length(function trim(job-description)) = 0
               move "Job description is required." to WS-DISPLAY
               perform say
               exit paragraph
           end-if
           
           move "Enter Employer Name:" to WS-DISPLAY
           perform say
           read InpFile into temp-input
               at end move "Y" to WS-EOF exit paragraph
           end-read
           move function trim(temp-input) to job-employer
           
           if function length(function trim(job-employer)) = 0
               move "Employer name is required." to WS-DISPLAY
               perform say
               exit paragraph
           end-if
           
           move "Enter Location:" to WS-DISPLAY
           perform say
           read InpFile into temp-input
               at end move "Y" to WS-EOF exit paragraph
           end-read
           move function trim(temp-input) to job-location
           
           if function length(function trim(job-location)) = 0
               move "Location is required." to WS-DISPLAY
               perform say
               exit paragraph
           end-if
           
           *> story three part added here as its not much
           
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
           
           perform save-job-posting
           
           move "Job posted successfully!" to WS-DISPLAY
           perform say
           move "----------------------------------" to WS-DISPLAY
           perform say
           .

       save-job-posting.
           open extend job-file
           if FILESTAT-JOB not = "00"
               open output job-file
               close job-file
               open extend job-file
           end-if
           
           if FILESTAT-JOB not = "00"
               move "Error: Could not save job posting." to WS-DISPLAY
               perform say
               exit paragraph
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

       say.
           display WS-DISPLAY
           move WS-DISPLAY to OutRecord
           write OutRecord
           .
