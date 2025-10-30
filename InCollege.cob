identification division.
       program-id. InCollege.

       environment division.
       input-output section.
       file-control.
           select user-file assign to "users.dat"
               organization is line sequential
               file status is FILESTAT.
           select profile-file assign to "profiles.dat"
               organization is line sequential
               file status is FILESTAT-PROFILE.
           select connection-file assign to "connections.dat"
               organization is line sequential
               file status is FILESTAT-CONN.
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
           select ConnOutFile assign to "Connections-Output.txt"
               organization is line sequential
               file status is FILESTAT-ConnOut.

           select message-file assign to "messages.dat"
               organization is line sequential
               file status is FILESTAT-Msg.

       data division.
       file section.
       fd  user-file.
       01  user-line                 pic x(120).

       fd  profile-file.
       01  profile-line              pic x(1500).

       fd  connection-file.
       01  connection-line           pic x(200).

       fd  InpFile.
       01  InpRecord                 pic x(200).

       fd  OutFile.
       01  OutRecord                 pic x(80).

       fd  ConnOutFile.
       01  ConnOutRecord            pic x(80).

       fd  job-file.
       01  job-line                  pic x(500).

       fd  application-file.
       01  application-line           pic x(300).

       fd  message-file.
       01  message-line              pic x(500).


       working-storage section.
       01  FILESTAT                  pic xx.
       01  FILESTAT-PROFILE          pic xx.
       01  FILESTAT-CONN             pic xx.
       01  FILESTAT-Out              pic xx.
       01  FILESTAT-ConnOut          pic xx.
       01  FILESTAT-JOB              pic xx.


       01  WS-EOF                    pic x value "N".
       01  WS-USER-CHOICE            pic 9 value 0.
       01  username-in               pic x(32).
       01  password-in               pic x(64).

       01  u                         pic x(32).
       01  p                         pic x(64).

       01  f-user                    pic x(32).
       01  f-pass                    pic x(64).

       01  ws-i                      pic 9(03) value 0.
       01  ws-j                      pic 9(03) value 0.
       01  ws-len-u                  pic 9(03) value 0.
       01  ws-len-p                  pic 9(03) value 0.
       01  ws-char                   pic x value space.

       01  ws-found                  pic x value "n".
       01  ws-ok-user                pic x value "n".
       01  ws-ok-pass                pic x value "n".

       01  ws-has-upper              pic x value "n".
       01  ws-has-lower              pic x value "n".
       01  ws-has-digit              pic x value "n".
       01  ws-has-special            pic x value "n".

       01  ws-commas                 pic 9(04) value 0.
       01  ws-spaces                 pic 9(04) value 0.
       01  ws-bad-char               pic x value "n".

       01  ws-user-count             pic 9(02) value 0.
       01  WS-DISPLAY                pic x(80).

       01  current-user              pic x(32).
       01  ws-profile-exists         pic x value "n".

       01  profile-data.
           05  profile-username      pic x(32).
           05  profile-firstname     pic x(50).
           05  profile-lastname      pic x(50).
           05  profile-university    pic x(100).
           05  profile-major         pic x(50).
           05  profile-gradyear      pic 9(4).
           05  profile-aboutme       pic x(200).
           05  profile-exp-count     pic 9.
           05  profile-experiences   occurs 3 times.
               10  exp-title         pic x(50).
               10  exp-company       pic x(50).
               10  exp-dates         pic x(30).
               10  exp-description   pic x(100).
           05  profile-edu-count     pic 9.
           05  profile-educations    occurs 3 times.
               10  edu-degree        pic x(50).
               10  edu-university    pic x(100).
               10  edu-years         pic x(30).

       01  temp-input                pic x(200).
       01  ws-year                   pic 9(4).
       01  ws-year-valid             pic x value "n".
       01  ws-exp-index              pic 9.
       01  ws-edu-index              pic 9.
       01  ws-entry-number           pic 9.
       01  ws-login-successful       pic x value "n".
       01  WS-CR-LOGGING             pic x value "N".


       01  ws-parse-pos              pic 9(04).
       01  ws-field-start            pic 9(04).
       01  ws-field-len              pic 9(04).
       01  ws-field-num              pic 9.
       01  WS-FIELD-POS              pic 9(04).

       01  PROFILES-TABLE.
           05 PROFILE-TABLE-ENTRY occurs 100 times pic x(1500).
       01  PROFILES-COUNT            pic 9(03) value 0.
       01  TEMP-PROFILE-LINE         pic x(1500).
       01  WS-LINE-LEN               pic 9(04) value 0.

       01  PARSE-FIELDS.
           05 PARSE-FIELD occurs 50 times pic x(200).
       01  ws-parse-idx              pic 9(02) value 0.

       01  TEMP-EDU-COUNT-STR       pic x(200).

      *>>    Epic #3: Variables for search functionality
       01  search-firstname          pic x(50).
       01  search-lastname           pic x(50).
       01  search-results-count      pic 9(02) value 0.
      *>>    Temporary profile data structure for search results
       01  temp-profile-data.
           05  temp-profile-username      pic x(32).
           05  temp-profile-firstname     pic x(50).
           05  temp-profile-lastname      pic x(50).
           05  temp-profile-university    pic x(100).
           05  temp-profile-major         pic x(50).

      *>>    Connection request variables
       01  connection-data.
           05  conn-from-user         pic x(32).
           05  conn-to-user           pic x(32).
           05  conn-status            pic x(10).
           01  conn-u1                  pic x(32).
           01  conn-u2                  pic x(32).

       01  ws-connection-exists      pic x value "n".
       01  ws-reverse-conn-exists    pic x value "n".
       01  connection-count          pic 9(03) value 0.
       01  CONNECTIONS-TABLE.
           05 CONNECTION-ENTRY occurs 100 times pic x(200).
       01  CONNECTIONS-COUNT         pic 9(03) value 0.
       01  target-username           pic x(32).
       01  ws-conn-choice            pic 9 value 0.

       01  job-data.
           05  job-poster-username    pic x(32).
           05  job-title              pic x(50).
           05  job-description        pic x(200).
           05  job-employer           pic x(100).
           05  job-location           pic x(50).
           05  job-salary             pic x(30).

       01  ws-job-choice              pic 9 value 0.

       *> Epic #7: Job browsing and application variables
       01  ws-job-count              pic 9(03) value 0.
       01  ws-job-selection          pic 9(03) value 0.
       01  ws-application-exists     pic x value "n".
       01  ws-application-count      pic 9(03) value 0.
       01  FILESTAT-APP              pic xx.

       01  JOBS-TABLE.
           05 JOB-TABLE-ENTRY occurs 100 times pic x(500).

       01  application-data.
           05  app-username           pic x(32).
           05  app-job-title          pic x(50).
           05  app-employer           pic x(100).
           05  app-location           pic x(50).

       01  FILESTAT-Msg              pic xx.

       01  message-data.
           05  msg-sender            pic x(32).
           05  msg-recipient         pic x(32).
           05  msg-content           pic x(200).
           05  msg-timestamp         pic x(20).

       01  ws-msg-choice             pic 9 value 0.
       01  ws-recipient-valid        pic x value "n".
       01  ws-is-connected           pic x value "n".

       procedure division.
       main.
           open input user-file
           if FILESTAT = "35"
              open output user-file
              close user-file
              open input user-file
           end-if
           if FILESTAT not = "00"
              display "ERROR opening users.dat, file status: " FILESTAT
              stop run
           end-if
           close user-file

           open input profile-file
           if FILESTAT-PROFILE = "35"
              open output profile-file
              close profile-file
           end-if
           close profile-file

           open input connection-file
           if FILESTAT-CONN = "35"
              open output connection-file
              close connection-file
           end-if
           close connection-file

           open input job-file
	   if FILESTAT-JOB = "35"
              open output job-file
              close job-file
           end-if
           close job-file

           open input application-file
           if FILESTAT-APP = "35"
              open output application-file
              close application-file
           end-if
           close application-file

           open input message-file
           if FILESTAT-Msg = "35"
               open output message-file
               close message-file
           end-if
           close message-file

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

           perform until WS-EOF = "Y"
              perform show-menu
              read InpFile into InpRecord
                 at end
                    move "Y" to WS-EOF
                    exit perform
              end-read
              move function numval(function trim(InpRecord)) to WS-USER-CHOICE
              evaluate WS-USER-CHOICE
                 when 1
                    perform interactive-login
                 when 2
                    perform interactive-create
                 when 3
                    move spaces to WS-DISPLAY
                    move "goodbye" to WS-DISPLAY
                    perform say
                    move "Y" to WS-EOF
                 when other
                    move spaces to WS-DISPLAY
                    move "please enter 1 2 or 3" to WS-DISPLAY
                    perform say
              end-evaluate
           end-perform

           close InpFile
           close OutFile
           stop run.

       show-menu.
           move spaces to WS-DISPLAY
           move "Welcome to InCollege!" to WS-DISPLAY
           perform say
           move "1. Log In" to WS-DISPLAY
           perform say
           move "2. Create New Account" to WS-DISPLAY
           perform say
           move "Enter your choice:" to WS-DISPLAY
           perform say
           .

       interactive-create.
           move spaces to WS-DISPLAY
           move "Please enter your username:" to WS-DISPLAY
           perform say
           read InpFile into username-in
              at end
                 move "Y" to WS-EOF
                 exit paragraph
           end-read

           move spaces to WS-DISPLAY
           move "Please enter your password:" to WS-DISPLAY
           perform say
           read InpFile into password-in
              at end
                 move "Y" to WS-EOF
                 exit paragraph
           end-read

           move function trim(username-in) to u
           move function trim(password-in) to p
           perform do-create
           .

       interactive-login.
           move "n" to ws-login-successful
           perform until WS-EOF = "Y" or ws-login-successful = "y"
              move spaces to WS-DISPLAY
              move "Please enter your username:" to WS-DISPLAY
              perform say
              read InpFile into username-in
                 at end
                    move "Y" to WS-EOF
                    exit paragraph
              end-read
              move spaces to WS-DISPLAY
              move "Please enter your password:" to WS-DISPLAY
              perform say
              read InpFile into password-in
                 at end
                    move "Y" to WS-EOF
                    exit paragraph
              end-read

              move function trim(username-in) to u
              move function trim(password-in) to p

              perform do-login
              if ws-found = "y"
                 move function trim(u) to current-user
                 move "y" to ws-login-successful
              end-if
           end-perform
           .

       do-create.
           perform check-username
           if ws-ok-user not = "y"
              move "Invalid username. Please try again." to WS-DISPLAY
              perform say
           exit paragraph
           end-if

           perform check-password
           if ws-ok-pass not = "y"
               move "Invalid password. Please try again." to WS-DISPLAY
               perform say
           exit paragraph
           end-if

           move 0 to ws-user-count
           open input user-file
           if FILESTAT = "00"
              perform until 1 = 2
                 read user-file into user-line
                    at end
                       exit perform
                 end-read
                 add 1 to ws-user-count
              end-perform
              close user-file
           else
              open output user-file
              close user-file
           end-if

           if ws-user-count >= 5
              move "All permitted accounts have been created, please come back later" to WS-DISPLAY
              perform say
              exit paragraph
           end-if

           move "n" to ws-found
           open input user-file
           if FILESTAT = "00"
              perform until 1 = 2
                 read user-file into user-line
                    at end
                       exit perform
                 end-read
                 unstring user-line delimited by "," into f-user f-pass
                 end-unstring
                 if function trim(f-user) = u
                    move "y" to ws-found
                    exit perform
                 end-if
              end-perform
              close user-file
           end-if

           if ws-found = "y"
              move spaces to WS-DISPLAY
              string "Rejected: user already exists: " u delimited by size into WS-DISPLAY
              perform say
              exit paragraph
           end-if

           open extend user-file
           if FILESTAT not = "00"
              open output user-file
              close user-file
              open extend user-file
           end-if

           move spaces to user-line
           string function trim(u) delimited by size
                  "," delimited by size
                  function trim(p) delimited by size
             into user-line
           end-string
           write user-line
           close user-file

           move spaces to WS-DISPLAY
           string "created: " u delimited by size into WS-DISPLAY
           perform say
           .

       do-login.
           move "n" to ws-found
           open input user-file
           if FILESTAT = "00"
              perform until 1 = 2
                 read user-file into user-line
                    at end
                       exit perform
                 end-read
                 unstring user-line delimited by "," into f-user f-pass
                 end-unstring
                 if function trim(f-user) = u
                    and function trim(f-pass) = p
                    move "y" to ws-found
                    exit perform
                 end-if
              end-perform
              close user-file
           end-if

           if ws-found = "y"
              move spaces to WS-DISPLAY
              string "You have successfully logged in." delimited by size into WS-DISPLAY
              perform say

              move function trim(u) to current-user

              move spaces to WS-DISPLAY
              string "Welcome, " function trim(u) "!" delimited by size into WS-DISPLAY
              perform say

              perform post-login-menu
           else
              move "Incorrect username/password, please try again" to WS-DISPLAY
              perform say
           end-if
           .

post-login-menu.
    perform until WS-EOF = "Y"
        move "1. Create/Edit My Profile" to WS-DISPLAY
        perform say

        move "2. Search for a job" to WS-DISPLAY
        perform say

        move "3. View My Profile" to WS-DISPLAY
        perform say

        move "4. Find someone you know" to WS-DISPLAY
        perform say

        move "5. View My Network" to WS-DISPLAY
        perform say

        move "6. Learn a new skill" to WS-DISPLAY
        perform say

        move "7. View My Pending Connection Requests" to WS-DISPLAY
        perform say

        move "8. Messages" to WS-DISPLAY
        perform say

        move "Enter your choice:" to WS-DISPLAY
        perform say

        read InpFile into InpRecord
            at end move "Y" to WS-EOF
            not at end
                move function numval(function trim(InpRecord))
                    to WS-USER-CHOICE
        end-read

        if WS-EOF = "N"
            evaluate WS-USER-CHOICE
                when 1
                    perform create-edit-profile
                when 2
                    perform job-search-menu
                when 3
                    perform view-profile
                when 4
                    perform search-for-user
                when 5
                    perform view-my-network
                when 6
                    perform show-skill-menu
                when 7
                    perform cr-view-pending-requests
                when 8
                    perform show-messages-menu
                when other
                    exit perform
            end-evaluate
        end-if
    end-perform
    .

       create-edit-profile.
           move "--- Create/Edit Profile ---" to WS-DISPLAY
           perform say

           perform load-profile

           move "Enter First Name:" to WS-DISPLAY
           perform say
           read InpFile into temp-input
               at end move "Y" to WS-EOF
               not at end move function trim(temp-input) to profile-firstname
           end-read

           move "Enter Last Name:" to WS-DISPLAY
           perform say
           read InpFile into temp-input
               at end move "Y" to WS-EOF
               not at end move function trim(temp-input) to profile-lastname
           end-read

           move "Enter University/College Attended:" to WS-DISPLAY
           perform say
           read InpFile into temp-input
               at end move "Y" to WS-EOF
               not at end move function trim(temp-input) to profile-university
           end-read

           move "Enter Major:" to WS-DISPLAY
           perform say
           read InpFile into temp-input
               at end move "Y" to WS-EOF
               not at end move function trim(temp-input) to profile-major
           end-read

           perform get-graduation-year

           move "Enter About Me (optional, max 200 chars, enter blank line to skip):" to WS-DISPLAY
           perform say
           read InpFile into temp-input
               at end move "Y" to WS-EOF
               not at end
                   if function trim(temp-input) = spaces
                       move spaces to profile-aboutme
                   else
                       move function trim(temp-input) to profile-aboutme
                   end-if
           end-read

           perform get-experience-entries

           perform get-education-entries

           perform save-profile

           move "Profile saved successfully!" to WS-DISPLAY
           perform say
           .

       get-graduation-year.
           move "n" to ws-year-valid
           perform until ws-year-valid = "y" or WS-EOF = "Y"
               move "Enter Graduation Year (YYYY):" to WS-DISPLAY
               perform say
               read InpFile into temp-input
                   at end
                       move "Y" to WS-EOF
                       exit perform
               end-read

               move function numval(function trim(temp-input)) to ws-year
               if ws-year >= 1950 and ws-year <= 2030
                   move ws-year to profile-gradyear
                   move "y" to ws-year-valid
               else
                   move "Invalid year. Please enter a year between 1950 and 2030." to WS-DISPLAY
                   perform say
               end-if
           end-perform
           .

       get-experience-entries.
           move 0 to profile-exp-count
           perform varying ws-exp-index from 1 by 1 until ws-exp-index > 3
               move spaces to exp-title(ws-exp-index)
               move spaces to exp-company(ws-exp-index)
               move spaces to exp-dates(ws-exp-index)
               move spaces to exp-description(ws-exp-index)
           end-perform

           perform until profile-exp-count >= 3 or WS-EOF = "Y"
               move "Add Experience (optional, max 3 entries. Enter 'DONE' to finish):" to WS-DISPLAY
               perform say

               read InpFile into temp-input
                   at end move "Y" to WS-EOF exit paragraph
               end-read

               if function upper-case(function trim(temp-input)) = "DONE"
                   exit perform
               end-if

               add 1 to profile-exp-count
               move profile-exp-count to ws-exp-index
               move ws-exp-index to ws-entry-number

               move spaces to WS-DISPLAY
               string "Experience #" ws-entry-number " - Title:" delimited by size into WS-DISPLAY
               perform say

               move function trim(temp-input) to exp-title(ws-exp-index)

               move spaces to WS-DISPLAY
               string "Experience #" ws-exp-index " - Company/Organization:" delimited by size into WS-DISPLAY
               perform say
               read InpFile into temp-input
                   at end move "Y" to WS-EOF exit paragraph
               end-read
               move function trim(temp-input) to exp-company(ws-exp-index)

               move spaces to WS-DISPLAY
               string "Experience #" ws-exp-index " - Dates (e.g., Summer 2024):" delimited by size into WS-DISPLAY
               perform say
               read InpFile into temp-input
                   at end move "Y" to WS-EOF exit paragraph
               end-read
               move function trim(temp-input) to exp-dates(ws-exp-index)

               move spaces to WS-DISPLAY
               string "Experience #" ws-exp-index " - Description (optional, max 100 chars, blank to skip):" delimited by size into WS-DISPLAY
               perform say

               read InpFile into temp-input
                   at end move "Y" to WS-EOF exit paragraph
               end-read

               if function length(function trim(temp-input)) > 0
                   if function length(function trim(temp-input)) <= 100
                       move function trim(temp-input) to exp-description(ws-exp-index)
                   else
                       move temp-input(1:100) to exp-description(ws-exp-index)
                   end-if
               else
                   move spaces to exp-description(ws-exp-index)
               end-if
           end-perform
           .

       get-education-entries.
           move 0 to profile-edu-count
           perform varying ws-edu-index from 1 by 1 until ws-edu-index > 3
               move spaces to edu-degree(ws-edu-index)
               move spaces to edu-university(ws-edu-index)
               move spaces to edu-years(ws-edu-index)
           end-perform

           perform until profile-edu-count >= 3 or WS-EOF = "Y"
               move "Add Education (optional, max 3 entries. Enter 'DONE' to finish):" to WS-DISPLAY
               perform say

               read InpFile into temp-input
                   at end move "Y" to WS-EOF exit paragraph
               end-read

               if function upper-case(function trim(temp-input)) = "DONE"
                   exit perform
               end-if

               add 1 to profile-edu-count
               move profile-edu-count to ws-edu-index
               move ws-edu-index to ws-entry-number

               move spaces to WS-DISPLAY
               string "Education #" ws-entry-number " - Degree:" delimited by size into WS-DISPLAY
               perform say

               move function trim(temp-input) to edu-degree(ws-edu-index)

               move spaces to WS-DISPLAY
               string "Education #" ws-edu-index " - University/College:" delimited by size into WS-DISPLAY
               perform say
               read InpFile into temp-input
                   at end move "Y" to WS-EOF exit paragraph
               end-read
               move function trim(temp-input) to edu-university(ws-edu-index)

               move spaces to WS-DISPLAY
               string "Education #" ws-edu-index " - Years Attended (e.g., 2023-2025):" delimited by size into WS-DISPLAY
               perform say
               read InpFile into temp-input
                   at end move "Y" to WS-EOF exit paragraph
               end-read
               move function trim(temp-input) to edu-years(ws-edu-index)
           end-perform
           .

       load-profile.
           move "n" to ws-profile-exists
           move current-user to profile-username

           open input profile-file
           if FILESTAT-PROFILE = "00"
               perform until 1 = 2
                   read profile-file into profile-line
                       at end exit perform
                   end-read

                   if profile-line(1:function length(function trim(current-user))) =
                      function trim(current-user)
                       and profile-line(function length(function trim(current-user)) + 1:1) = "|"
                       move "y" to ws-profile-exists
                       perform parse-profile-line-complete
                       exit perform
                   end-if
               end-perform
               close profile-file
           end-if

           if ws-profile-exists = "n"
               initialize profile-data
               move current-user to profile-username
           end-if
           .

       save-profile.
           move 0 to PROFILES-COUNT
           open input profile-file
           if FILESTAT-PROFILE = "00"
               perform until 1 = 2
                   read profile-file into TEMP-PROFILE-LINE
                      at end
                          exit perform
                   end-read
                   if not ( TEMP-PROFILE-LINE(1:function length(function trim(current-user))) = function trim(current-user)
                        and TEMP-PROFILE-LINE(function length(function trim(current-user)) + 1:1) = "|" )
                       add 1 to PROFILES-COUNT
                       move TEMP-PROFILE-LINE to PROFILE-TABLE-ENTRY(PROFILES-COUNT)
                   end-if
               end-perform
               close profile-file
           end-if

           move spaces to profile-line
           move 1 to ws-parse-pos

           string
               function trim(current-user) "|"
               function trim(profile-firstname) "|"
               function trim(profile-lastname) "|"
               function trim(profile-university) "|"
               function trim(profile-major) "|"
               profile-gradyear "|"
               function trim(profile-aboutme) "|"
               profile-exp-count "|"
               delimited by size
               into profile-line
               with pointer ws-parse-pos
           end-string

           perform varying ws-i from 1 by 1 until ws-i > profile-exp-count
               string
                   function trim(exp-title(ws-i)) "|"
                   function trim(exp-company(ws-i)) "|"
                   function trim(exp-dates(ws-i)) "|"
                   function trim(exp-description(ws-i)) "|"
                   delimited by size
                   into profile-line
                   with pointer ws-parse-pos
               end-string
           end-perform

           string
               profile-edu-count "|"
               delimited by size
               into profile-line
               with pointer ws-parse-pos
           end-string

           perform varying ws-i from 1 by 1 until ws-i > profile-edu-count
               string
                   function trim(edu-degree(ws-i)) "|"
                   function trim(edu-university(ws-i)) "|"
                   function trim(edu-years(ws-i)) "|"
                   delimited by size
                   into profile-line
                   with pointer ws-parse-pos
               end-string
           end-perform

           move profile-line to TEMP-PROFILE-LINE

           open output profile-file
           if FILESTAT-PROFILE not = "00"
               display "Error saving profile"
               close profile-file
               exit paragraph
           end-if

           perform varying ws-i from 1 by 1 until ws-i > PROFILES-COUNT
               move PROFILE-TABLE-ENTRY(ws-i) to profile-line
               write profile-line
           end-perform

           move TEMP-PROFILE-LINE to profile-line
           write profile-line
           close profile-file
           .

       view-profile.
      *>>    Epic #3: Enhanced profile display with easy-to-read format
      *>>    Shows all fields including optional ones in organized sections
           move "================================" to WS-DISPLAY
           perform say
           move "         YOUR PROFILE           " to WS-DISPLAY
           perform say
           move "================================" to WS-DISPLAY
           perform say

           perform load-profile-for-view

           if ws-profile-exists = "y"
               move "--- Personal Information ---" to WS-DISPLAY
               perform say

               move spaces to WS-DISPLAY
               string "Name: " function trim(profile-firstname) " "
                      function trim(profile-lastname) delimited by size into WS-DISPLAY
               perform say

               move spaces to WS-DISPLAY
               string "University: " function trim(profile-university) delimited by size into WS-DISPLAY
               perform say

               move spaces to WS-DISPLAY
               string "Major: " function trim(profile-major) delimited by size into WS-DISPLAY
               perform say

               move spaces to WS-DISPLAY
               string "Graduation Year: " profile-gradyear delimited by size into WS-DISPLAY
               perform say

      *>>        Epic #3: Display optional About Me section if provided
               if function trim(profile-aboutme) not = spaces
                   move " " to WS-DISPLAY
                   perform say
                   move "--- About Me ---" to WS-DISPLAY
                   perform say
                   move function trim(profile-aboutme) to WS-DISPLAY
                   perform say
               end-if

      *>>        Epic #3: Display all experience entries with proper formatting
               if profile-exp-count > 0
                   move " " to WS-DISPLAY
                   perform say
                   move "--- Professional Experience ---" to WS-DISPLAY
                   perform say
                   perform varying ws-i from 1 by 1 until ws-i > profile-exp-count
                       move " " to WS-DISPLAY
                       perform say
                       move spaces to WS-DISPLAY
                       string "Experience #" ws-i ":" delimited by size into WS-DISPLAY
                       perform say
                       move spaces to WS-DISPLAY
                       string "  Title: " function trim(exp-title(ws-i)) delimited by size into WS-DISPLAY
                       perform say
                       move spaces to WS-DISPLAY
                       string "  Company: " function trim(exp-company(ws-i)) delimited by size into WS-DISPLAY
                       perform say
                       move spaces to WS-DISPLAY
                       string "  Dates: " function trim(exp-dates(ws-i)) delimited by size into WS-DISPLAY
                       perform say
                       if function trim(exp-description(ws-i)) not = spaces
                           move spaces to WS-DISPLAY
                           string "  Description: " function trim(exp-description(ws-i)) delimited by size into WS-DISPLAY
                           perform say
                       end-if
                   end-perform
               else
      *>>            Epic #3: Show message when no experience entries exist
                   move " " to WS-DISPLAY
                   perform say
                   move "--- Professional Experience ---" to WS-DISPLAY
                   perform say
                   move "  No experience entries added." to WS-DISPLAY
                   perform say
               end-if

      *>>        Epic #3: Display all education entries with proper formatting
               if profile-edu-count > 0
                   move " " to WS-DISPLAY
                   perform say
                   move "--- Education ---" to WS-DISPLAY
                   perform say
                   perform varying ws-i from 1 by 1 until ws-i > profile-edu-count
                       move " " to WS-DISPLAY
                       perform say
                       move spaces to WS-DISPLAY
                       string "Education #" ws-i ":" delimited by size into WS-DISPLAY
                       perform say
                       move spaces to WS-DISPLAY
                       string "  Degree: " function trim(edu-degree(ws-i)) delimited by size into WS-DISPLAY
                       perform say
                       move spaces to WS-DISPLAY
                       string "  University: " function trim(edu-university(ws-i)) delimited by size into WS-DISPLAY
                       perform say
                       move spaces to WS-DISPLAY
                       string "  Years: " function trim(edu-years(ws-i)) delimited by size into WS-DISPLAY
                       perform say
                   end-perform
               else
                   move " " to WS-DISPLAY
                   perform say
                   move "--- Education ---" to WS-DISPLAY
                   perform say
                   move "  No education entries added." to WS-DISPLAY
                   perform say
               end-if

               move " " to WS-DISPLAY
               perform say
               move "================================" to WS-DISPLAY
               perform say
           else
               move "No profile found. Please create your profile first." to WS-DISPLAY
               perform say
           end-if
           .

       load-profile-for-view.
           move "n" to ws-profile-exists
           initialize profile-data
           perform varying ws-i from 1 by 1 until ws-i > 3
               move spaces to exp-title(ws-i)
               move spaces to exp-company(ws-i)
               move spaces to exp-dates(ws-i)
               move spaces to exp-description(ws-i)
               move spaces to edu-degree(ws-i)
               move spaces to edu-university(ws-i)
               move spaces to edu-years(ws-i)
           end-perform

           open input profile-file
           if FILESTAT-PROFILE = "00"
               perform until 1 = 2
                   read profile-file into profile-line
                       at end exit perform
                   end-read

                   if function length(function trim(current-user)) > 0
                       if profile-line(1:function length(function trim(current-user))) =
                          function trim(current-user)
                           and profile-line(function length(function trim(current-user)) + 1:1) = "|"
                           move "y" to ws-profile-exists
                           perform parse-profile-line-complete
                           exit perform
                       end-if
                   end-if
               end-perform
               close profile-file
           end-if
           .

       search-for-user.
      *>>    Epic #3: New feature - Search for users by full name
      *>>    Performs case-insensitive search across all profiles
      *>>    Displays matching users with their basic information
           move "--- Search for User ---" to WS-DISPLAY
           perform say
           move "Enter the first name of the person you're looking for:" to WS-DISPLAY
           perform say

           read InpFile into temp-input
               at end move "Y" to WS-EOF exit paragraph
           end-read
           move function trim(temp-input) to search-firstname

           move "Enter the last name of the person you're looking for:" to WS-DISPLAY
           perform say

           read InpFile into temp-input
               at end move "Y" to WS-EOF exit paragraph
           end-read
           move function trim(temp-input) to search-lastname

           move 0 to search-results-count

           move " " to WS-DISPLAY
           perform say
           move "Searching..." to WS-DISPLAY
           perform say
           move " " to WS-DISPLAY
           perform say

           open input profile-file
           if FILESTAT-PROFILE = "00"
               perform until 1 = 2
                   read profile-file into profile-line
                       at end exit perform
                   end-read

                   perform parse-search-profile

                   if function upper-case(function trim(temp-profile-firstname)) =
                      function upper-case(function trim(search-firstname))
                      and function upper-case(function trim(temp-profile-lastname)) =
                          function upper-case(function trim(search-lastname))
                       add 1 to search-results-count
                       perform display-search-result
                   end-if
               end-perform
               close profile-file
           end-if

           if search-results-count = 0
               move "No users found matching that name." to WS-DISPLAY
               perform say
           else
               move " " to WS-DISPLAY
               perform say
               move spaces to WS-DISPLAY
               string search-results-count " user(s) found." delimited by size into WS-DISPLAY
               perform say
           end-if

           move " " to WS-DISPLAY
           perform say
           .

       parse-search-profile.
      *>>    Epic #3: Helper procedure to parse profile data for search
      *>>    Extracts basic fields needed for search results display
           move spaces to PARSE-FIELD(1)
           move spaces to PARSE-FIELD(2)
           move spaces to PARSE-FIELD(3)
           move spaces to PARSE-FIELD(4)
           move spaces to PARSE-FIELD(5)
           move spaces to PARSE-FIELD(6)

           unstring profile-line delimited by "|" into
               PARSE-FIELD(1)
               PARSE-FIELD(2)
               PARSE-FIELD(3)
               PARSE-FIELD(4)
               PARSE-FIELD(5)
               PARSE-FIELD(6)
           end-unstring

           move function trim(PARSE-FIELD(1)) to temp-profile-username
           move function trim(PARSE-FIELD(2)) to temp-profile-firstname
           move function trim(PARSE-FIELD(3)) to temp-profile-lastname
           move function trim(PARSE-FIELD(4)) to temp-profile-university
           move function trim(PARSE-FIELD(5)) to temp-profile-major
           .

       display-search-result.
      *>>    Epic #3: Display formatted search result for a matching user
      *>>    Shows username, full name, university, and major
           move "================================" to WS-DISPLAY
           perform say
           move spaces to WS-DISPLAY
           string "Username: " function trim(temp-profile-username) delimited by size into WS-DISPLAY
           perform say
           move spaces to WS-DISPLAY
           string "Name: " function trim(temp-profile-firstname) " "
                  function trim(temp-profile-lastname) delimited by size into WS-DISPLAY
           perform say
           move spaces to WS-DISPLAY
           string "University: " function trim(temp-profile-university) delimited by size into WS-DISPLAY
           perform say
           move spaces to WS-DISPLAY
           string "Major: " function trim(temp-profile-major) delimited by size into WS-DISPLAY
           perform say
           move "================================" to WS-DISPLAY
           perform cr-offer-send-menu
           .

      view-my-network.
          perform until ws-conn-choice = 4 or WS-EOF = "Y"
              move "--- My Network ---" to WS-DISPLAY
              perform say
              move "1. Send Connection Request" to WS-DISPLAY
              perform say
              move "2. View Pending Connection Requests" to WS-DISPLAY
              perform say
              move "3. View My Connections" to WS-DISPLAY
              perform say
              move "4. Go Back" to WS-DISPLAY
              perform say
              move "Enter your choice:" to WS-DISPLAY
              perform say

              read InpFile into InpRecord
                  at end move "Y" to WS-EOF
                  not at end
                      move function numval(function trim(InpRecord))
                          to ws-conn-choice
              end-read

              if WS-EOF = "N"
                  evaluate ws-conn-choice
                      when 1
                          perform send-connection-request
                      when 2
                          perform view-pending-requests
                      when 3
                          perform view-my-connections
                      when 4
                          continue
                      when other
                          move "Invalid choice. Please enter 1, 2, 3, or 4." to WS-DISPLAY
                          perform say
                  end-evaluate
              end-if
          end-perform
          .

       send-connection-request.
           perform cr-begin-log
           move "--- Send Connection Request ---" to WS-DISPLAY
           perform say
           move "Enter username to send request to:" to WS-DISPLAY
           perform say

           read InpFile into temp-input
               at end move "Y" to WS-EOF exit paragraph
           end-read
           move function trim(temp-input) to target-username

      *>>    Check if target user exists
           move "n" to ws-found
           open input user-file
           if FILESTAT = "00"
               perform until 1 = 2
                   read user-file into user-line
                       at end exit perform
                   end-read
                   unstring user-line delimited by "," into f-user f-pass
                   end-unstring
                   if function trim(f-user) = target-username
                       move "y" to ws-found
                       exit perform
                   end-if
               end-perform
               close user-file
           end-if

           if ws-found = "n"
               move "User not found." to WS-DISPLAY
               perform say
               exit paragraph
           end-if

           if target-username = current-user
               move "You cannot send a connection request to yourself." to WS-DISPLAY
               perform say
               exit paragraph
           end-if

      *>>    Check if connection already exists or reverse connection exists
           perform check-existing-connections

           if ws-connection-exists = "y"
               move "Connection request already sent to this user." to WS-DISPLAY
               perform say
               exit paragraph
           end-if

           if ws-reverse-conn-exists = "y"
               move "This user has already sent you a connection request." to WS-DISPLAY
               perform say
               move "Please check your pending requests." to WS-DISPLAY
               perform say
               exit paragraph
           end-if

      *>>    Save the connection request
           open extend connection-file
           if FILESTAT-CONN not = "00"
               open output connection-file
               close connection-file
               open extend connection-file
           end-if

           move spaces to connection-line
           string function trim(current-user) delimited by size
                  "|" delimited by size
                  function trim(target-username) delimited by size
                  "|pending" delimited by size
               into connection-line
           end-string
           write connection-line
           close connection-file

           move "Connection request sent successfully!" to WS-DISPLAY
           perform say
           perform cr-end-log
           .

      view-pending-requests.
          perform cr-begin-log
          move "--- Pending Connection Requests ---" to WS-DISPLAY
          perform say

          move 0 to CONNECTIONS-COUNT
          move 0 to connection-count

          open input connection-file
          if FILESTAT-CONN = "00"
              perform until 1 = 2
                  read connection-file into connection-line
                      at end exit perform
                  end-read
                  add 1 to CONNECTIONS-COUNT
                  move connection-line to CONNECTION-ENTRY(CONNECTIONS-COUNT)
              end-perform
              close connection-file
          end-if

          perform varying ws-i from 1 by 1 until ws-i > CONNECTIONS-COUNT
              move CONNECTION-ENTRY(ws-i) to connection-line
              unstring connection-line delimited by "|" into
                  conn-from-user
                  conn-to-user
                  conn-status
              end-unstring

              if function trim(conn-to-user) = current-user
                  and function trim(conn-status) = "pending"
                  add 1 to connection-count

                  move spaces to WS-DISPLAY
                  string "Request from: " function trim(conn-from-user)
                         delimited by size into WS-DISPLAY
                  perform say
                  move "1. Accept" to WS-DISPLAY
                  perform say
                  move "2. Reject" to WS-DISPLAY
                  perform say

                  move spaces to WS-DISPLAY
                  string "Enter your choice for "
                         function trim(conn-from-user) ":"
                         delimited by size into WS-DISPLAY
                  perform say

                  read InpFile into temp-input
                      at end move "Y" to WS-EOF exit paragraph
                  end-read
                  move function numval(function trim(temp-input)) to ws-conn-choice

                  if ws-conn-choice = 1
                      move spaces to WS-DISPLAY
                      string "Connection request from "
                             function trim(conn-from-user)
                             " accepted!"
                             delimited by size into WS-DISPLAY
                      perform say

                      move spaces to connection-line
                      string function trim(conn-from-user) delimited by size
                             "|" delimited by size
                             function trim(conn-to-user) delimited by size
                             "|connected" delimited by size
                          into connection-line
                      end-string
                      move connection-line to CONNECTION-ENTRY(ws-i)
                  else
                      move spaces to WS-DISPLAY
                      string "Connection request from "
                             function trim(conn-from-user)
                             " rejected!"
                             delimited by size into WS-DISPLAY
                      perform say

                      move spaces to connection-line
                      string function trim(conn-from-user) delimited by size
                             "|" delimited by size
                             function trim(conn-to-user) delimited by size
                             "|rejected" delimited by size
                          into connection-line
                      end-string
                      move connection-line to CONNECTION-ENTRY(ws-i)
                  end-if
              end-if
          end-perform

          open output connection-file
          if FILESTAT-CONN = "00"
              perform varying ws-i from 1 by 1 until ws-i > CONNECTIONS-COUNT
                  move CONNECTION-ENTRY(ws-i) to connection-line
                  write connection-line
              end-perform
              close connection-file
          end-if

          if connection-count = 0
              move "No pending connection requests." to WS-DISPLAY
              perform say
          end-if

          move " " to WS-DISPLAY
          perform say
          perform cr-end-log
          .

      view-my-connections.
          move "--- My Connections ---" to WS-DISPLAY
          perform say
          move 0 to connection-count

          open input connection-file
          if FILESTAT-CONN = "00"
              perform until 1 = 2
                  read connection-file into connection-line
                      at end exit perform
                  end-read

                  unstring connection-line delimited by "|" into
                      conn-from-user
                      conn-to-user
                      conn-status
                  end-unstring

                  if function trim(conn-status) = "connected"
                      if function trim(conn-from-user) = current-user
                          add 1 to connection-count
                          move spaces to WS-DISPLAY
                          string connection-count ". "
                                 function trim(conn-to-user)
                                 delimited by size into WS-DISPLAY
                          perform say
                      end-if
                      if function trim(conn-to-user) = current-user
                          add 1 to connection-count
                          move spaces to WS-DISPLAY
                          string connection-count ". "
                                 function trim(conn-from-user)
                                 delimited by size into WS-DISPLAY
                          perform say
                      end-if
                  end-if
              end-perform
              close connection-file
          end-if

          if connection-count = 0
              move "You have no established connections yet." to WS-DISPLAY
              perform say
          else
              move " " to WS-DISPLAY
              perform say
              move spaces to WS-DISPLAY
              string "Total connections: " connection-count
                     delimited by size into WS-DISPLAY
              perform say
          end-if

          move " " to WS-DISPLAY
          perform say
          move "-----------------------------------" to WS-DISPLAY
          perform say
          .

parse-profile-line-complete.
           perform varying ws-parse-idx from 1 by 1 until ws-parse-idx > 50
               move spaces to PARSE-FIELD(ws-parse-idx)
           end-perform

           unstring profile-line delimited by "|" into
               PARSE-FIELD(1)
               PARSE-FIELD(2)
               PARSE-FIELD(3)
               PARSE-FIELD(4)
               PARSE-FIELD(5)
               PARSE-FIELD(6)
               PARSE-FIELD(7)
               PARSE-FIELD(8)
               PARSE-FIELD(9)
               PARSE-FIELD(10)
               PARSE-FIELD(11)
               PARSE-FIELD(12)
               PARSE-FIELD(13)
               PARSE-FIELD(14)
               PARSE-FIELD(15)
               PARSE-FIELD(16)
               PARSE-FIELD(17)
               PARSE-FIELD(18)
               PARSE-FIELD(19)
               PARSE-FIELD(20)
               PARSE-FIELD(21)
               PARSE-FIELD(22)
               PARSE-FIELD(23)
               PARSE-FIELD(24)
               PARSE-FIELD(25)
               PARSE-FIELD(26)
               PARSE-FIELD(27)
               PARSE-FIELD(28)
               PARSE-FIELD(29)
               PARSE-FIELD(30)
               PARSE-FIELD(31)
               PARSE-FIELD(32)
           end-unstring

           move function trim(PARSE-FIELD(2)) to profile-firstname
           move function trim(PARSE-FIELD(3)) to profile-lastname
           move function trim(PARSE-FIELD(4)) to profile-university
           move function trim(PARSE-FIELD(5)) to profile-major
           if function trim(PARSE-FIELD(6)) not = spaces
               move function numval(function trim(PARSE-FIELD(6))) to profile-gradyear
           else
               move 0 to profile-gradyear
           end-if
           move function trim(PARSE-FIELD(7)) to profile-aboutme

           if function trim(PARSE-FIELD(8)) not = spaces
               move function numval(function trim(PARSE-FIELD(8))) to profile-exp-count
           else
               move 0 to profile-exp-count
           end-if

           if profile-exp-count < 0 move 0 to profile-exp-count end-if
           if profile-exp-count > 3 move 3 to profile-exp-count end-if

      *>>    Epic #3 Fix: Corrected field indexing for proper data extraction
           move 9 to ws-field-num

           if profile-exp-count >= 1
               move function trim(PARSE-FIELD(ws-field-num)) to exp-title(1)
               move function trim(PARSE-FIELD(ws-field-num + 1)) to exp-company(1)
               move function trim(PARSE-FIELD(ws-field-num + 2)) to exp-dates(1)
               move function trim(PARSE-FIELD(ws-field-num + 3)) to exp-description(1)
               add 4 to ws-field-num
           end-if

           if profile-exp-count >= 2
               move function trim(PARSE-FIELD(ws-field-num)) to exp-title(2)
               move function trim(PARSE-FIELD(ws-field-num + 1)) to exp-company(2)
               move function trim(PARSE-FIELD(ws-field-num + 2)) to exp-dates(2)
               move function trim(PARSE-FIELD(ws-field-num + 3)) to exp-description(2)
               add 4 to ws-field-num
           end-if

           if profile-exp-count >= 3
               move function trim(PARSE-FIELD(ws-field-num)) to exp-title(3)
               move function trim(PARSE-FIELD(ws-field-num + 1)) to exp-company(3)
               move function trim(PARSE-FIELD(ws-field-num + 2)) to exp-dates(3)
               move function trim(PARSE-FIELD(ws-field-num + 3)) to exp-description(3)
               add 4 to ws-field-num
           end-if

           if function trim(PARSE-FIELD(ws-field-num)) not = spaces
               move function numval(function trim(PARSE-FIELD(ws-field-num))) to profile-edu-count
           else
               move 0 to profile-edu-count
           end-if
           add 1 to ws-field-num

           if profile-edu-count < 0 move 0 to profile-edu-count end-if
           if profile-edu-count > 3 move 3 to profile-edu-count end-if

           if profile-edu-count >= 1
               move function trim(PARSE-FIELD(ws-field-num)) to edu-degree(1)
               move function trim(PARSE-FIELD(ws-field-num + 1)) to edu-university(1)
               move function trim(PARSE-FIELD(ws-field-num + 2)) to edu-years(1)
               add 3 to ws-field-num
           end-if

           if profile-edu-count >= 2
               move function trim(PARSE-FIELD(ws-field-num)) to edu-degree(2)
               move function trim(PARSE-FIELD(ws-field-num + 1)) to edu-university(2)
               move function trim(PARSE-FIELD(ws-field-num + 2)) to edu-years(2)
               add 3 to ws-field-num
           end-if

           if profile-edu-count >= 3
               move function trim(PARSE-FIELD(ws-field-num)) to edu-degree(3)
               move function trim(PARSE-FIELD(ws-field-num + 1)) to edu-university(3)
               move function trim(PARSE-FIELD(ws-field-num + 2)) to edu-years(3)
               add 3 to ws-field-num
           end-if

           move "y" to ws-profile-exists
           .

       show-skill-menu.
           perform until WS-USER-CHOICE = 6 or WS-EOF = "Y"
               move "Learn a New Skill:" to WS-DISPLAY
               perform say

               move "1. Learn COBOL!" to WS-DISPLAY
               perform say

               move "2. Learn Jira!" to WS-DISPLAY
               perform say

               move "3. Learn Git!" to WS-DISPLAY
               perform say

               move "4. Learn Github!" to WS-DISPLAY
               perform say

               move "5. Learn Software Engineering!" to WS-DISPLAY
               perform say

               move "6. Go Back" to WS-DISPLAY
               perform say

               move "Enter your choice:" to WS-DISPLAY
               perform say

               read InpFile into InpRecord
                   at end move "Y" to WS-EOF
                   not at end
                       move function numval(function trim(InpRecord))
                           to WS-USER-CHOICE
               end-read

               if WS-EOF = "N"
                   if WS-USER-CHOICE >= 1 and WS-USER-CHOICE <= 5
                       move "This skill is under construction."
                           to WS-DISPLAY
                       perform say
                   end-if
               end-if
           end-perform
           .

       check-username.
           move "n" to ws-ok-user
           move function length(function trim(u)) to ws-len-u
           if ws-len-u = 0 or ws-len-u > 10
              exit paragraph
           end-if

           move 0 to ws-commas
           inspect u tallying ws-commas for all ","
           if ws-commas > 0
              exit paragraph
           end-if

           move "n" to ws-bad-char
           perform varying ws-i from 1 by 1
                   until ws-i > ws-len-u or ws-bad-char = "y"
              move u(ws-i:1) to ws-char
              if (ws-char < "0")
                 or (ws-char > "9" and ws-char < "A")
                 or (ws-char > "Z" and ws-char < "a")
                 or (ws-char > "z")
                 move "y" to ws-bad-char
              end-if
           end-perform

           if ws-bad-char = "y"
              exit paragraph
           end-if

           move "y" to ws-ok-user
           .

       check-password.
           move "n" to ws-ok-pass
           move function length(function trim(p)) to ws-len-p
           if ws-len-p < 8 or ws-len-p > 12
              exit paragraph
           end-if

           move 0 to ws-commas
           inspect p(1:ws-len-p) tallying ws-commas for all ","
           if ws-commas > 0
              exit paragraph
           end-if

           move 0 to ws-spaces
           inspect p(1:ws-len-p) tallying ws-spaces for all " "
           if ws-spaces > 0
              exit paragraph
           end-if

           move "n" to ws-has-upper ws-has-lower ws-has-digit ws-has-special
           perform varying ws-i from 1 by 1 until ws-i > ws-len-p
              move p(ws-i:1) to ws-char
              if ws-char >= "A" and ws-char <= "Z"
                 move "y" to ws-has-upper
              else
                 if ws-char >= "a" and ws-char <= "z"
                    move "y" to ws-has-lower
                 else
                    if ws-char >= "0" and ws-char <= "9"
                       move "y" to ws-has-digit
                    else
                       if ws-char not = " " and ws-char not = ","
                          move "y" to ws-has-special
                       end-if
                    end-if
                 end-if
              end-if
           end-perform

           if ws-has-upper = "y" and ws-has-lower = "y" and ws-has-digit = "y" and ws-has-special = "y"
              move "y" to ws-ok-pass
           end-if
           .

       say.
      *>>    Epic #3: All screen output is written to InCollege-Output.txt
      *>>    This includes profile viewing and search results for easy verification

           display WS-DISPLAY
           move WS-DISPLAY to OutRecord
           write OutRecord

           *> ITK-87: while in connection-requests context, also mirror to Connections-Output.txt
           if WS-CR-LOGGING = "Y"
              open extend ConnOutFile
              if FILESTAT-ConnOut not = "00"
                 open output ConnOutFile
                 close ConnOutFile
                 open extend ConnOutFile
              end-if
              move WS-DISPLAY to ConnOutRecord
              write ConnOutRecord
              close ConnOutFile
           end-if
           .

*>================ ITK-87 helpers: turn CR logging on/off ================
           cr-begin-log.
               move "Y" to WS-CR-LOGGING
               .

           cr-end-log.
               move "N" to WS-CR-LOGGING
           .




*> Integration: call 'perform cr-offer-send-menu' at the end of display-search-result.

       cr-offer-send-menu.
           perform cr-begin-log

           move "1. Send Connection Request" to WS-DISPLAY
           perform say
           move "2. Back to Main Menu" to WS-DISPLAY
           perform say
           move "Enter your choice:" to WS-DISPLAY
           perform say

           read InpFile into InpRecord
               at end exit paragraph
               not at end
                   move function numval(function trim(InpRecord))
                     to WS-USER-CHOICE
           end-read

           if WS-USER-CHOICE = 1
              perform send-connection-request-from-profile
           end-if
           perform cr-end-log

           .

       send-connection-request-from-profile.

           *> Begins to print ouput to a separate file
           perform cr-begin-log

           *> Target is the user whose card we just displayed
           move function trim(temp-profile-username) to target-username

           *> Self-guard
           if function upper-case(function trim(target-username)) =
              function upper-case(function trim(current-user))
              move "You cannot send a connection request to yourself." to WS-DISPLAY
              perform say
              exit paragraph
           end-if

           *> Reuse existing validation (sets ws-connection-exists / ws-reverse-conn-exists)
           perform check-existing-connections

           if ws-connection-exists = "y"
              move "You are already connected with this user." to WS-DISPLAY
              perform say
              exit paragraph
           end-if

           if ws-reverse-conn-exists = "y"
              move "This user has already sent you a connection request." to WS-DISPLAY
              perform say
              move "Please check your pending requests." to WS-DISPLAY
              perform say
              exit paragraph
           end-if

           *> Append current-user|target-username|pending to connections.dat
           open extend connection-file
           if FILESTAT-CONN not = "00"
              open output connection-file
              close connection-file
              open extend connection-file
           end-if

           move spaces to connection-line
           string function trim(current-user) delimited by size
                  "|"                     delimited by size
                  function trim(target-username) delimited by size
                  "|pending"              delimited by size
                  into connection-line
           end-string
           write connection-line
           close connection-file

           perform cr-notify-request-sent
           perform cr-end-log
           .

*> Called by send-connection-request-from-profile (ITK-79).

       cr-notify-request-sent.
           move spaces to WS-DISPLAY
           string "Connection request sent to "
                  function trim(temp-profile-firstname) " "
                  function trim(temp-profile-lastname) "."
                  delimited by size
                  into WS-DISPLAY
           perform say
           .

*> Sets ws-connection-exists = "y" if already connected (either direction)
*> Sets ws-reverse-conn-exists = "y" if target has a pending request to current-user
*> Expects:
*>   target-username, current-user
*>   connection-file, connection-line, FILESTAT-CONN
*>   ws-connection-exists, ws-reverse-conn-exists, conn-u1, conn-u2, conn-status

       check-existing-connections.
           move "n" to ws-connection-exists
           move "n" to ws-reverse-conn-exists

           open input connection-file
           if FILESTAT-CONN = "00"
              perform until 1 = 2
                 read connection-file into connection-line
                    at end exit perform
                 end-read

                 move spaces to conn-u1
                 move spaces to conn-u2
                 move spaces to conn-status
                 unstring connection-line delimited by "|"
                     into conn-u1 conn-u2 conn-status
                 end-unstring

                 *> already connected?
                 if function upper-case(function trim(conn-status)) = "CONNECTED"
                    and (
                        (function upper-case(function trim(conn-u1)) =
                          function upper-case(function trim(current-user)) and
                         function upper-case(function trim(conn-u2)) =
                          function upper-case(function trim(target-username)))
                        or
                        (function upper-case(function trim(conn-u2)) =
                          function upper-case(function trim(current-user)) and
                         function upper-case(function trim(conn-u1)) =
                          function upper-case(function trim(target-username)))
                       )
                    move "y" to ws-connection-exists
                 end-if

                 *> reverse pending (they already sent to me)
                 if function upper-case(function trim(conn-status)) = "PENDING"
                    and function upper-case(function trim(conn-u1)) =
                        function upper-case(function trim(target-username))
                    and function upper-case(function trim(conn-u2)) =
                        function upper-case(function trim(current-user))
                    move "y" to ws-reverse-conn-exists
                 end-if
              end-perform
           end-if
           close connection-file
           .

*> Lists all entries in connections.dat where conn-u2 = current-user and status=pending.
       cr-view-pending-requests.
           perform cr-begin-log
           move "--- Pending Connection Requests ---" to WS-DISPLAY
           perform say

           move 0 to search-results-count

           open input connection-file
           if FILESTAT-CONN = "00"
              perform until 1 = 2
                 read connection-file into connection-line
                    at end exit perform
                 end-read

                 move spaces to conn-u1
                 move spaces to conn-u2
                 move spaces to conn-status
                 unstring connection-line delimited by "|"
                     into conn-u1 conn-u2 conn-status
                 end-unstring

                 if function upper-case(function trim(conn-status)) = "PENDING"
                    and function upper-case(function trim(conn-u2)) =
                        function upper-case(function trim(current-user))
                    add 1 to search-results-count
                    move spaces to WS-DISPLAY
                    string "- " function trim(conn-u1)
                           " has sent you a connection request."
                           delimited by size into WS-DISPLAY
                    perform say
                 end-if
              end-perform
           end-if
           close connection-file

           if search-results-count = 0
              move "You have no pending connection requests at this time." to WS-DISPLAY
              perform say
           end-if

           move "-----------------------------------" to WS-DISPLAY
           perform say
           perform cr-end-log
           .

job-search-menu.
    perform until ws-job-choice = 4 or WS-EOF = "Y"
        move "--- Job Search/Internship Menu ---" to WS-DISPLAY
        perform say

        move "1. Post a Job/Internship" to WS-DISPLAY
        perform say

        move "2. Browse Jobs/Internships" to WS-DISPLAY
        perform say

        move "3. View My Applications" to WS-DISPLAY
        perform say

        move "4. Back to Main Menu" to WS-DISPLAY
        perform say

        move "Enter your choice:" to WS-DISPLAY
        perform say

        read InpFile into InpRecord
            at end move "Y" to WS-EOF
            not at end
                move function numval(function trim(InpRecord))
                    to ws-job-choice
        end-read

        if WS-EOF = "N"
            evaluate ws-job-choice
                when 1
                    perform post-job-internship
                when 2
                    perform browse-jobs-internships
                when 3
                    perform view-my-applications
                when 4
                    continue
                when other
                    move "Invalid choice. Please enter 1, 2, 3, or 4."
                        to WS-DISPLAY
                    perform say
            end-evaluate
        end-if
    end-perform

    *> Reset choice for next time
    move 0 to ws-job-choice
    .

post-job-internship.
    move "--- Post a New Job/Internship ---" to WS-DISPLAY
    perform say

    *> Initialize job data
    initialize job-data
    move function trim(current-user) to job-poster-username

    *> Capture job title (required)
    move "Enter Job Title:" to WS-DISPLAY
    perform say
    read InpFile into temp-input
        at end move "Y" to WS-EOF exit paragraph
    end-read
    move function trim(temp-input) to job-title

    *> Validate required field
    if function length(function trim(job-title)) = 0
        move "Job title is required." to WS-DISPLAY
        perform say
        exit paragraph
    end-if

    *> Capture description (required)
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

    *> Validate required field
    if function length(function trim(job-description)) = 0
        move "Job description is required." to WS-DISPLAY
        perform say
        exit paragraph
    end-if

    *> Capture employer (required)
    move "Enter Employer Name:" to WS-DISPLAY
    perform say
    read InpFile into temp-input
        at end move "Y" to WS-EOF exit paragraph
    end-read
    move function trim(temp-input) to job-employer

    *> Validate required field
    if function length(function trim(job-employer)) = 0
        move "Employer name is required." to WS-DISPLAY
        perform say
        exit paragraph
    end-if

    *> Capture location (required)
    move "Enter Location:" to WS-DISPLAY
    perform say
    read InpFile into temp-input
        at end move "Y" to WS-EOF exit paragraph
    end-read
    move function trim(temp-input) to job-location

    *> Validate required field
    if function length(function trim(job-location)) = 0
        move "Location is required." to WS-DISPLAY
        perform say
        exit paragraph
    end-if

    *> Capture salary (optional)
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

    *> Save the job posting
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

    *> Format: username|title|description|employer|location|salary
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

browse-jobs-internships.
    move "--- Available Job Listings ---" to WS-DISPLAY
    perform say

    move 0 to ws-job-count
    move 0 to ws-i

    *> Load all jobs into memory
    open input job-file
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

    *> Display job summaries
    if ws-job-count = 0
        move "No job listings available at this time." to WS-DISPLAY
        perform say
        move "-----------------------------" to WS-DISPLAY
        perform say
        exit paragraph
    else
        perform varying ws-i from 1 by 1 until ws-i > ws-job-count
            move JOB-TABLE-ENTRY(ws-i) to job-line
            perform parse-job-line
            perform display-job-summary
        end-perform
        move "-----------------------------" to WS-DISPLAY
        perform say
    end-if

    *> Allow user to view job details
    perform view-job-details-loop
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

parse-job-line.
    *> Format: username|title|description|employer|location|salary
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

view-job-details-loop.
    perform until WS-EOF = "Y"
        move "Enter job number to view details, or 0 to go back:" to WS-DISPLAY
        perform say

        read InpFile into temp-input
            at end move "Y" to WS-EOF exit paragraph
        end-read

        move function numval(function trim(temp-input)) to ws-job-selection

        if ws-job-selection = 0
            exit paragraph
        end-if

        if ws-job-selection < 1 or ws-job-selection > ws-job-count
            move "Invalid job number. Please try again." to WS-DISPLAY
            perform say
        else
            move JOB-TABLE-ENTRY(ws-job-selection) to job-line
            perform parse-job-line
            perform display-full-job-details
            perform show-apply-option
        end-if
    end-perform
    .

display-full-job-details.
    move "--- Job Details ---" to WS-DISPLAY
    perform say

    move spaces to WS-DISPLAY
    string "Title: " function trim(job-title)
           delimited by size into WS-DISPLAY
    perform say

    move spaces to WS-DISPLAY
    string "Description: " function trim(job-description)
           delimited by size into WS-DISPLAY
    perform say

    move spaces to WS-DISPLAY
    string "Employer: " function trim(job-employer)
           delimited by size into WS-DISPLAY
    perform say

    move spaces to WS-DISPLAY
    string "Location: " function trim(job-location)
           delimited by size into WS-DISPLAY
    perform say

    if function length(function trim(job-salary)) > 0
        move spaces to WS-DISPLAY
        string "Salary: " function trim(job-salary)
               delimited by size into WS-DISPLAY
        perform say
    end-if

    move "-------------------" to WS-DISPLAY
    perform say
    .

show-apply-option.
    move "1. Apply for this Job" to WS-DISPLAY
    perform say
    move "2. Back to Job List" to WS-DISPLAY
    perform say
    move "Enter your choice:" to WS-DISPLAY
    perform say

    read InpFile into temp-input
        at end move "Y" to WS-EOF exit paragraph
    end-read

    move function numval(function trim(temp-input)) to WS-USER-CHOICE

    if WS-USER-CHOICE = 1
        perform apply-for-job
    end-if
    .

apply-for-job.
    perform check-existing-application

    if ws-application-exists = "y"
        move "You have already applied for this job." to WS-DISPLAY
        perform say
        exit paragraph
    end-if

    perform save-job-application
    .

check-existing-application.
    move "n" to ws-application-exists

    open input application-file
    if FILESTAT-APP = "00"
        perform until 1 = 2
            read application-file into application-line
                at end exit perform
            end-read

            perform parse-application-line

            if function trim(app-username) = current-user
               and function trim(app-job-title) = function trim(job-title)
               and function trim(app-employer) = function trim(job-employer)
                move "y" to ws-application-exists
                exit perform
            end-if
        end-perform
        close application-file
    end-if
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

    *> Format: username|job-title|employer|location
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

    *> Display confirmation message
    move spaces to WS-DISPLAY
    string "Your application for "
           function trim(job-title) " at "
           function trim(job-employer)
           " has been submitted."
           delimited by size into WS-DISPLAY
    perform say
    .

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
    *> Format: username|job-title|employer|location
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

show-messages-menu.
    move 0 to ws-msg-choice
    perform until ws-msg-choice = 3 or WS-EOF = "Y"
        move "--- Messages Menu ---" to WS-DISPLAY
        perform say

        move "1. Send a New Message" to WS-DISPLAY
        perform say

        move "2. View My Messages" to WS-DISPLAY
        perform say

        move "3. Back to Main Menu" to WS-DISPLAY
        perform say

        move "Enter your choice:" to WS-DISPLAY
        perform say

        read InpFile into InpRecord
            at end move "Y" to WS-EOF
            not at end
                move function numval(function trim(InpRecord))
                    to ws-msg-choice
        end-read

        if WS-EOF = "N"
            evaluate ws-msg-choice
                when 1
                    perform send-new-message
                when 2
                    perform view-my-messages
                when 3
                    continue
                when other
                    move "Invalid choice. Please enter 1, 2, or 3."
                        to WS-DISPLAY
                    perform say
            end-evaluate
        end-if
    end-perform
    .

    send-new-message.
    move "--- Send a New Message ---" to WS-DISPLAY
    perform say

    move "Enter recipient's username (must be a connection):" to WS-DISPLAY
    perform say

    read InpFile into temp-input
        at end move "Y" to WS-EOF exit paragraph
    end-read
    move function trim(temp-input) to target-username

    *> Validate recipient exists and is connected
    perform validate-message-recipient

    if ws-is-connected = "n"
        exit paragraph
    end-if

    *> Get message content
    move "Enter your message (max 200 chars):" to WS-DISPLAY
    perform say

    read InpFile into temp-input
        at end move "Y" to WS-EOF exit paragraph
    end-read

    *> Truncates message if too long
    if function length(function trim(temp-input)) > 200
        move temp-input(1:200) to msg-content
    else
        move function trim(temp-input) to msg-content
    end-if

    *> Save the message
    perform save-message

    move spaces to WS-DISPLAY
    string "Message sent to " function trim(target-username)
           " successfully!" delimited by size into WS-DISPLAY
    perform say
    move "---------------------" to WS-DISPLAY
    perform say
    .

    validate-message-recipient.
    move "n" to ws-is-connected

    *> First check if user exists
    move "n" to ws-found
    open input user-file
    if FILESTAT = "00"
        perform until 1 = 2
            read user-file into user-line
                at end exit perform
            end-read
            unstring user-line delimited by "," into f-user f-pass
            end-unstring
            if function trim(f-user) = target-username
                move "y" to ws-found
                exit perform
            end-if
        end-perform
        close user-file
    end-if

    if ws-found = "n"
        move "User not found." to WS-DISPLAY
        perform say
        exit paragraph
    end-if

    *> Check if connected
    open input connection-file
    if FILESTAT-CONN = "00"
        perform until 1 = 2
            read connection-file into connection-line
                at end exit perform
            end-read

            unstring connection-line delimited by "|" into
                conn-from-user
                conn-to-user
                conn-status
            end-unstring

           *> Checks if connection exists either way in data file
            if function trim(conn-status) = "connected"
                if (function trim(conn-from-user) = current-user
                    and function trim(conn-to-user) = target-username)
                   or (function trim(conn-to-user) = current-user
                    and function trim(conn-from-user) = target-username)
                    move "y" to ws-is-connected
                    exit perform
                end-if
            end-if
        end-perform
        close connection-file
    end-if

    if ws-is-connected = "n"
        move "You must be connected with the user to send a message."
            to WS-DISPLAY
        perform say
    end-if
    .

    save-message.
    move function trim(current-user) to msg-sender
    move function trim(target-username) to msg-recipient
    move function current-date to msg-timestamp

    open extend message-file
    if FILESTAT-Msg not = "00"
        open output message-file
        close message-file
        open extend message-file
    end-if

    if FILESTAT-Msg not = "00"
        move "Error: Could not save message." to WS-DISPLAY
        perform say
        exit paragraph
    end-if

    *> Saves message in format: sender|recipient|content|timestamp
    move spaces to message-line
    string
        function trim(msg-sender) "|"
        function trim(msg-recipient) "|"
        function trim(msg-content) "|"
        function trim(msg-timestamp)
        delimited by size
        into message-line
    end-string

    write message-line
    close message-file
    .

    view-my-messages.
    move "View My Messages is under construction." to WS-DISPLAY
    perform say
    .