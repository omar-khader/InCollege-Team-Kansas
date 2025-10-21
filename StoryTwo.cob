identification division.
       program-id. Story2.

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

       01  job-data.
           05  job-title              pic x(50).
           05  job-description        pic x(200).
           05  job-employer           pic x(100).
           05  job-location           pic x(50).

       procedure division.
       main.
           open input InpFile
           if FILESTAT not = "00"
              display "ERROR opening InCollege-Input.txt"
              stop run
           end-if

           open output OutFile
           if FILESTAT-Out not = "00"
              display "ERROR opening Incollege-Output.txt"
              stop run
           end-if

           perform capture-job-fields

           close InpFile
           close OutFile
           stop run.

       capture-job-fields.
           move "=== Capturing Job Fields ===" to WS-DISPLAY
           perform say
           
           move "Enter Job Title:" to WS-DISPLAY
           perform say
           read InpFile into temp-input
               at end move "Y" to WS-EOF exit paragraph
           end-read
           move function trim(temp-input) to job-title
           
           if function length(function trim(job-title)) = 0
               move "ERROR: Job title is required." to WS-DISPLAY
               perform say
               exit paragraph
           else
               move spaces to WS-DISPLAY
               string "Captured Title: " function trim(job-title) 
                   delimited by size into WS-DISPLAY
               perform say
           end-if
           
           move "Enter Description (max 200 chars):" to WS-DISPLAY
           perform say
           read InpFile into temp-input
               at end move "Y" to WS-EOF exit paragraph
           end-read
           
           if function length(function trim(temp-input)) > 200
               move temp-input(1:200) to job-description
               move "WARNING: Description truncated to 200 chars" to WS-DISPLAY
               perform say
           else
               move function trim(temp-input) to job-description
           end-if
           
           if function length(function trim(job-description)) = 0
               move "ERROR: Job description is required." to WS-DISPLAY
               perform say
               exit paragraph
           else
               move spaces to WS-DISPLAY
               string "Captured Description: " function trim(job-description) 
                   delimited by size into WS-DISPLAY
               perform say
           end-if
           
           move "Enter Employer Name:" to WS-DISPLAY
           perform say
           read InpFile into temp-input
               at end move "Y" to WS-EOF exit paragraph
           end-read
           move function trim(temp-input) to job-employer
           
           if function length(function trim(job-employer)) = 0
               move "ERROR: Employer name is required." to WS-DISPLAY
               perform say
               exit paragraph
           else
               move spaces to WS-DISPLAY
               string "Captured Employer: " function trim(job-employer) 
                   delimited by size into WS-DISPLAY
               perform say
           end-if
           
           move "Enter Location:" to WS-DISPLAY
           perform say
           read InpFile into temp-input
               at end move "Y" to WS-EOF exit paragraph
           end-read
           move function trim(temp-input) to job-location
           
           if function length(function trim(job-location)) = 0
               move "ERROR: Location is required." to WS-DISPLAY
               perform say
               exit paragraph
           else
               move spaces to WS-DISPLAY
               string "Captured Location: " function trim(job-location) 
                   delimited by size into WS-DISPLAY
               perform say
           end-if
           
           move "=== All Required Fields Captured Successfully ===" to WS-DISPLAY
           perform say
           .

       say.
           display WS-DISPLAY
           move WS-DISPLAY to OutRecord
           write OutRecord
           .
