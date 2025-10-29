identification division.
program-id. StoryEight.

environment division.
input-output section.
file-control.
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
fd  application-file.
01  application-line          pic x(300).

fd  InpFile.
01  InpRecord                 pic x(200).

fd  OutFile.
01  OutRecord                 pic x(80).

working-storage section.
01  FILESTAT                  pic xx.
01  FILESTAT-APP              pic xx.
01  FILESTAT-Out              pic xx.
01  WS-EOF                    pic x value "N".
01  WS-DISPLAY                pic x(80).
01  ws-application-count      pic 9(03) value 0.
01  current-user              pic x(32) value "testuser".

01  application-data.
    05  app-username           pic x(32).
    05  app-job-title          pic x(50).
    05  app-employer           pic x(100).
    05  app-location           pic x(50).

01  PARSE-FIELDS.
    05 PARSE-FIELD occurs 50 times pic x(200).

procedure division.
main.
    open input application-file
    if FILESTAT-APP = "35"
        open output application-file
        close application-file
        open input application-file
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
    
    perform view-my-applications
    
    close InpFile
    close OutFile
    stop run.

view-my-applications.
    move "--- Your Job Applications ---" to WS-DISPLAY
    perform say
    
    move spaces to WS-DISPLAY
    string "Application Summary for " function trim(current-user)
           delimited by size into WS-DISPLAY
    perform say
    
    move "------------------------------" to WS-DISPLAY
    perform say
    
    move 0 to ws-application-count
    
    open input application-file
    if FILESTAT-APP = "00"
        perform until 1 = 2
            read application-file into application-line
                at end exit perform
            end-read
            
            perform parse-application-line
            
            if function trim(app-username) = current-user
                add 1 to ws-application-count
                perform display-application-summary
            end-if
        end-perform
        close application-file
    end-if
    
    move "------------------------------" to WS-DISPLAY
    perform say
    
    if ws-application-count = 0
        move "You have not applied to any jobs yet." to WS-DISPLAY
        perform say
    else
        move spaces to WS-DISPLAY
        string "Total Applications: " ws-application-count
               delimited by size into WS-DISPLAY
        perform say
    end-if
    
    move "------------------------------" to WS-DISPLAY
    perform say
    .

parse-application-line.
    move spaces to PARSE-FIELD(1)
    move spaces to PARSE-FIELD(2)
    move spaces to PARSE-FIELD(3)
    move spaces to PARSE-FIELD(4)
    
    unstring application-line delimited by "|" into
        PARSE-FIELD(1)
        PARSE-FIELD(2)
        PARSE-FIELD(3)
        PARSE-FIELD(4)
    end-unstring
    
    move function trim(PARSE-FIELD(1)) to app-username
    move function trim(PARSE-FIELD(2)) to app-job-title
    move function trim(PARSE-FIELD(3)) to app-employer
    move function trim(PARSE-FIELD(4)) to app-location
    .

display-application-summary.
    move spaces to WS-DISPLAY
    string "Job Title: " function trim(app-job-title)
           delimited by size into WS-DISPLAY
    perform say
    
    move spaces to WS-DISPLAY
    string "Employer: " function trim(app-employer)
           delimited by size into WS-DISPLAY
    perform say
    
    move spaces to WS-DISPLAY
    string "Location: " function trim(app-location)
           delimited by size into WS-DISPLAY
    perform say
    
    move "---" to WS-DISPLAY
    perform say
    .

say.
    display WS-DISPLAY
    move WS-DISPLAY to OutRecord
    write OutRecord
    .
