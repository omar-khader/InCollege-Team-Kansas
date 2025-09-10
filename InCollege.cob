identification division.
       program-id. InCollege.

       environment division.
       input-output section.
       file-control.
           select user-file assign to "users.dat"
               organization is line sequential
               file status is FILESTAT.
           select InpFile assign to "InCollege-Test.txt"
               organization is line sequential
               file status is FILESTAT.
           select OutFile assign to "InCollege-Output.txt"
               organization is line sequential
               file status is FILESTAT-Out.

       data division.
       file section.
       fd  user-file.
       01  user-line                 pic x(120).

       fd  InpFile.
       01  InpRecord                 pic x(80).

       fd  OutFile.
       01  OutRecord                 pic x(120).

       working-storage section.
       01  FILESTAT                  pic xx.
       01  FILESTAT-Out              pic xx.

       01  WS-EOF                    pic x value "N".
       01  WS-USER-CHOICE            pic 9 value 0.
       01  username-in               pic x(32).
       01  password-in               pic x(64).

       01  u                         pic x(32).
       01  p                         pic x(64).

       01  f-user                    pic x(32).
       01  f-pass                    pic x(64).

       01  ws-i                      pic 9(03) value 0.
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

       procedure division.
       main.
           *> Ensure users.dat exists
           open input user-file
           if FILESTAT = "35"  
              open output user-file
              close user-file
              *> reoopen for input now that file exists
              open input user-file
           end-if
           if FILESTAT not = "00"
              display "ERROR opening users.dat, file status: " FILESTAT
              stop run
           end-if
           close user-file

           
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
              *> read next token which is the user's menu choice
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
           move "3. Exit" to WS-DISPLAY
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
           perform until WS-EOF = "Y"
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
                 exit paragraph *> login successful
              else
                 *> allow unlimited login attempts
                 next sentence
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


           *> Count existing users (up to 5 allowed)
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

           *> Check duplicates
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
              string "rejected_duplicate: " u delimited by size into WS-DISPLAY
              perform say
              exit paragraph
           end-if

           *> new user to users.dat
           open extend user-file
           if FILESTAT not = "00"
              *> failsafe create file
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
              
              *> Add welcome message
              move spaces to WS-DISPLAY
              string "Welcome, " function trim(u) "!" delimited by size into WS-DISPLAY
              perform say
              
              *> post-login navigation
              perform post-login-menu
           else
              move "Incorrect username/password, please try again" to WS-DISPLAY
              perform say
           end-if
           .

       post-login-menu.
        perform until WS-EOF = "Y"
        move "1. Search for a job" to WS-DISPLAY
        perform say

        move "2. Find someone you know" to WS-DISPLAY
        perform say

        move "3. Learn a new skill" to WS-DISPLAY
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
                    move "Job search/internship is under construction."
                        to WS-DISPLAY
                    perform say
                when 2
                    move "Find someone you know is under construction."
                        to WS-DISPLAY
                    perform say
                when 3
                    perform show-skill-menu
            end-evaluate
        end-if
    end-perform
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
           display WS-DISPLAY
           move WS-DISPLAY to OutRecord
           write OutRecord
           .
