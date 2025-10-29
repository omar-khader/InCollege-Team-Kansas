identification division.
program-id. US6-Application-Confirmation.

environment division.
input-output section.
file-control.
    select job-file assign to "jobs.dat"
        organization is line sequential
        file status is FILESTAT-JOB.
    select application-file assign to "applications.dat"
        organization is line sequential
        file status is FILESTAT-APP.
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

fd  application-file.
01  application-line          pic x(300).

fd  InpFile.
01  InpRecord                 pic x(200).

fd  OutFile.
01  OutRecord                 pic x(80).

working-storage section.
01  FILESTAT                  pic xx.
01  FILESTAT-JOB              pic xx.
01  FILESTAT-APP              pic xx.
01  FILESTAT-Out              pic xx.
01  WS-EOF                    pic x value "N".
01  WS-DISPLAY                pic x(80).
01  ws-job-count              pic 9(03) value 0.
01  ws-i                      pic 9(03) value 0.
01  ws-job-selection          pic 9(03) value 0.
01  temp-input                pic x(200).
01  current-user              pic x(32) value "testuser".

01  JOBS-TABLE.
    05 JOB-TABLE-ENTRY occurs 100 times pic x(500).

01  job-data.
    05  job-poster-username    pic x(32).
    05  job-title              pic x(50).
    05  job-description        pic x(200).
    05  job-employer           pic x(100).
    05  job-location           pic x(50).
    05  job-salary             pic x(30).

01  PARSE-FIELDS.
    05 PARSE-FIELD occurs 50 times pic x(200).

procedure division.
main.
    open input job-file
    if FILESTAT-JOB = "35"
        open output job-file
        close job-file
        open input job-file
    end-if
    
    if FILESTAT-JOB = "00"
        perform until 1 = 2
            read job-file into job-line
                at end exit perform
            end-read
            add 1 to ws-job-count
            move job-line to JOB-TABLE-ENTRY(ws-job-count)
        end-perform
        close job-file
    end-if
    
    open input application-file
    if FILESTAT-APP = "35"
        open output application-file
        close application-file
    end-if
    close application-file
    
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
    
    perform test-confirmation-message
    
    close InpFile
    close OutFile
    stop run.

test-confirmation-message.
    move "=== Testing Confirmation Message ===" to WS-DISPLAY
    perform say
    
    move "--- Available Job Listings ---" to WS-DISPLAY
    perform say
    
    if ws-job-count = 0
        move "No job listings available." to WS-DISPLAY
        perform say
        exit paragraph
    end-if
    
    perform varying ws-i from 1 by 1 until ws-i > ws-job-count
        move JOB-TABLE-ENTRY(ws-i) to job-line
        perform parse-job-line
        perform display-job-summary
    end-perform
    
    move "-----------------------------" to WS-DISPLAY
    perform say
    
    move "Enter job number to apply:" to WS-DISPLAY
    perform say
    
    read InpFile into temp-input
        at end move "Y" to WS-EOF exit paragraph
    end-read
    
    move function numval(function trim(temp-input)) to ws-job-selection
    
    if ws-job-selection < 1 or ws-job-selection > ws-job-count
        move "Invalid job number." to WS-DISPLAY
        perform say
        exit paragraph
    end-if
    
    move JOB-TABLE-ENTRY(ws-job-selection) to job-line
    perform parse-job-line
    perform save-job-application
    .

parse-job-line.
    move spaces to PARSE-FIELD(1)
    move spaces to PARSE-FIELD(2)
    move spaces to PARSE-FIELD(3)
    move spaces to PARSE-FIELD(4)
    move spaces to PARSE-FIELD(5)
    move spaces to PARSE-FIELD(6)
    
    unstring job-line delimited by "|" into
        PARSE-FIELD(1)
        PARSE-FIELD(2)
        PARSE-FIELD(3)
        PARSE-FIELD(4)
        PARSE-FIELD(5)
        PARSE-FIELD(6)
    end-unstring
    
    move function trim(PARSE-FIELD(1)) to job-poster-username
    move function trim(PARSE-FIELD(2)) to job-title
    move function trim(PARSE-FIELD(3)) to job-description
    move function trim(PARSE-FIELD(4)) to job-employer
    move function trim(PARSE-FIELD(5)) to job-location
    move function trim(PARSE-FIELD(6)) to job-salary
    .

display-job-summary.
    move spaces to WS-DISPLAY
    string ws-i ". " 
           function trim(job-title) " at " 
           function trim(job-employer) " (" 
           function trim(job-location) ")"
           delimited by size into WS-DISPLAY
    perform say
    .

save-job-application.
    open extend application-file
    if FILESTAT-APP not = "00"
        open output application-file
        close application-file
        open extend application-file
    end-if
    
    if FILESTAT-APP not = "00"
        move "Error: Could not save application." to WS-DISPLAY
        perform say
        exit paragraph
    end-if
    
    move spaces to application-line
    string 
        function trim(current-user) "|"
        function trim(job-title) "|"
        function trim(job-employer) "|"
        function trim(job-location)
        delimited by size
        into application-line
    end-string
    
    write application-line
    close application-file
    
    *> THIS IS THE CRITICAL CONFIRMATION MESSAGE
    move " " to WS-DISPLAY
    perform say
    move "*** CONFIRMATION MESSAGE ***" to WS-DISPLAY
    perform say
    move spaces to WS-DISPLAY
    string "Your application for " 
           function trim(job-title) " at " 
           function trim(job-employer) 
           " has been submitted."
           delimited by size into WS-DISPLAY
    perform say
    move "****************************" to WS-DISPLAY
    perform say
    .

say.
    display WS-DISPLAY
    move WS-DISPLAY to OutRecord
    write OutRecord
    .
