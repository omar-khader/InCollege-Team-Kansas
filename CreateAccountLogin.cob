       identification division.                              *> start of program
       program-id. CreateAccountLogin.                       *> program name

       environment division.                                 *> environment section
       input-output section.                                 *> file input and output
       file-control.                                         *> file control
           select user-file assign to "users.dat"            *> file that stores accounts
               organization is line sequential.              *> treat as text line by line
           select in-file assign to "input.txt"              *> batch commands file
               organization is line sequential.              *> treat as text line by line

       data division.                                        *> start of data
       file section.                                         *> file layouts
       fd  user-file.                                        *> descriptor for users.dat
       01  user-line                 pic x(120).             *> one line username comma password

       fd  in-file.                                           *> descriptor for input.txt
       01  in-line                   pic x(120).             *> one command line

       working-storage section.                               *> variables
       01  menu-choice               pic 9 value 0.          *> user menu choice
       01  cmd                       pic x(10).              *> command create or login for batch
       01  username-in               pic x(32).              *> raw username from input or batch
       01  password-in               pic x(64).              *> raw password from input or batch

       01  u                         pic x(32).              *> trimmed username
       01  p                         pic x(64).              *> trimmed password

       01  f-user                    pic x(32).              *> username from file
       01  f-pass                    pic x(64).              *> password from file

       01  ws-i                      pic 9(03) value 0.      *> loop index
       01  ws-len-u                  pic 9(03) value 0.      *> username length
       01  ws-len-p                  pic 9(03) value 0.      *> password length
       01  ws-char                   pic x value space.      *> single character holder

       01  ws-found                  pic x value "n".        *> found flag
       01  ws-ok-user                pic x value "n".        *> username ok
       01  ws-ok-pass                pic x value "n".        *> password ok

       01  ws-has-upper              pic x value "n".        *> password has upper
       01  ws-has-lower              pic x value "n".        *> password has lower
       01  ws-has-digit              pic x value "n".        *> password has digit
       01  ws-has-special            pic x value "n".        *> password has special

       01  ws-commas                 pic 9(04) value 0.      *> count commas
       01  ws-spaces                 pic 9(04) value 0.      *> count spaces
       01  ws-bad-char               pic x value "n".        *> bad char flag

       procedure division.                                     *> start of logic
       main.                                                   *> main paragraph

           open extend user-file                               *> ensure users.dat exists
           close user-file                                     *> close right away

           perform until menu-choice = 3                       *> loop until exit selected
              perform show-menu                                *> show menu
              accept menu-choice                               *> read user choice
              evaluate menu-choice                             *> route on choice
                 when 1
                    perform interactive-create                 *> interactive create flow
                 when 2
                    perform interactive-login                  *> interactive login flow
                 when 3
                    display "goodbye"                          *> exit message
                 when other
                    display "please enter 1 2 or 3"            *> invalid choice
              end-evaluate
           end-perform

           stop run.                                           *> end program

       show-menu.                                              *> draw menu
           display "welcome to incollege"                      *> header
           display "1 create new account"                      *> option 1
           display "2 login"                                   *> option 2
           display "3 run batch from input.txt and then exit"  *> option 3 batch mode
           display "enter choice"                              *> prompt
           .                                                   *> end paragraph

       interactive-create.                                     *> interactive create wrapper
           display "enter new username max 10 letters and digits only" *> prompt username
           accept username-in                                  *> read username
           display "enter new password 8 to 12 with upper lower digit special no spaces" *> prompt password
           accept password-in                                  *> read password
           move function trim(username-in) to u                *> trim username
           move function trim(password-in) to p                *> trim password
           perform do-create                                   *> run create using u and p
           .                                                   *> end paragraph

       interactive-login.                                      *> interactive login wrapper
           display "enter username"                            *> prompt username
           accept username-in                                  *> read username
           display "enter password"                            *> prompt password
           accept password-in                                  *> read password
           move function trim(username-in) to u                *> trim username
           move function trim(password-in) to p                *> trim password
           perform do-login                                    *> run login using u and p
           .                                                   *> end paragraph

       run-batch.                                              *> batch mode that processes input.txt
           open input in-file                                  *> open input file
           perform until 1 = 2                                 *> read until eof
              read in-file at end exit perform end-read        *> read a line or stop
              move spaces to cmd username-in password-in       *> clear fields
              unstring in-line delimited by all spaces         *> split into three parts
                  into cmd username-in password-in
              end-unstring
              move function trim(username-in) to u             *> trim username
              move function trim(password-in) to p             *> trim password
              evaluate cmd                                     *> route based on command
                 when "create" perform do-create
                 when "login"  perform do-login
                 when other
                    if cmd not = spaces
                       display "ignored: " in-line
                    end-if
              end-evaluate
           end-perform
           close in-file                                       *> close input file
           .                                                   *> end paragraph

       do-create.                                              *> create account
           perform check-username                              *> validate username
           if ws-ok-user not = "y"
              display "rejected_invalid_username: " u
              exit paragraph
           end-if

           perform check-password                              *> validate password
           if ws-ok-pass not = "y"
              display "rejected_invalid_password: " u
              exit paragraph
           end-if

           move "n" to ws-found                                *> reset found
           open input user-file                                *> open db to scan
           perform until 1 = 2
              read user-file at end exit perform end-read
              unstring user-line delimited by "," into f-user f-pass
              end-unstring
              if function trim(f-user) = u
                 move "y" to ws-found                          *> duplicate username
                 exit perform
              end-if
           end-perform
           close user-file

           if ws-found = "y"
              display "rejected_duplicate: " u                 *> already exists
              exit paragraph
           end-if

           open extend user-file                               *> append new record
           string function trim(u) delimited by size
                  ","              delimited by size
                  function trim(p) delimited by size
             into user-line
           end-string
           write user-line
           close user-file
           display "created: " u                               *> confirm created
           .                                                   *> end paragraph

       do-login.                                               *> login flow
           perform check-username                              *> validate username format
           if ws-ok-user not = "y"
              display "login_rejected_invalid_username: " u
              exit paragraph
           end-if

           perform check-password                              *> validate password format
           if ws-ok-pass not = "y"
              display "login_rejected_invalid_password: " u
              exit paragraph
           end-if

           move "n" to ws-found                                *> reset found
           open input user-file                                *> open db to scan
           perform until 1 = 2
              read user-file at end exit perform end-read
              unstring user-line delimited by "," into f-user f-pass
              end-unstring
              if function trim(f-user) = u
                 and function trim(f-pass) = p
                 move "y" to ws-found                          *> exact match found
                 exit perform
              end-if
           end-perform
           close user-file

           if ws-found = "y"
              display "login_success: " u
           else
              display "login_failed: " u
           end-if
           .                                                   *> end paragraph

       check-username.                                         *> username validation
           move "n" to ws-ok-user                              *> default invalid
           move function length(function trim(u)) to ws-len-u  *> compute length
           if ws-len-u = 0 or ws-len-u > 10                    *> must be 1 to 10
              exit paragraph
           end-if

           move 0 to ws-commas                                 *> count commas
           inspect u tallying ws-commas for all ","
           if ws-commas > 0                                    *> no commas allowed
              exit paragraph
           end-if

           move "n" to ws-bad-char                             *> assume ok chars
           perform varying ws-i from 1 by 1                    *> scan each char
                   until ws-i > ws-len-u or ws-bad-char = "y"
              move u(ws-i:1) to ws-char
              if (ws-char < "0")
                 or (ws-char > "9" and ws-char < "A")
                 or (ws-char > "Z" and ws-char < "a")
                 or (ws-char > "z")
                 move "y" to ws-bad-char                       *> mark bad if out of ranges
              end-if
           end-perform

           if ws-bad-char = "y"                                *> reject if any bad char
              exit paragraph
           end-if

           move "y" to ws-ok-user                              *> passed all checks
           .                                                   *> end paragraph

       check-password.                                         *> password validation
           move "n" to ws-ok-pass                              *> default invalid
           move function length(function trim(p)) to ws-len-p  *> compute length
           if ws-len-p < 8 or ws-len-p > 12                    *> must be 8 to 12
              exit paragraph
           end-if

           move 0 to ws-commas                                 *> check commas
           inspect p(1:ws-len-p) tallying ws-commas for all ","
           if ws-commas > 0
              exit paragraph
           end-if

           move 0 to ws-spaces                                 *> check spaces
           inspect p(1:ws-len-p) tallying ws-spaces for all " "
           if ws-spaces > 0
              exit paragraph
           end-if

           move "n" to ws-has-upper ws-has-lower ws-has-digit ws-has-special *> reset flags
           perform varying ws-i from 1 by 1 until ws-i > ws-len-p            *> scan password
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

           if ws-has-upper = "y" and ws-has-lower = "y"
              and ws-has-digit = "y" and ws-has-special = "y"
              move "y" to ws-ok-pass                            *> password ok
           end-if
           .                                                   *> end paragraph
