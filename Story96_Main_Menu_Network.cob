identification division.
       program-id. ITK96NetworkMenu.

       environment division.
       input-output section.
       file-control.
           select connection-file assign to "connections.dat"
               organization is line sequential
               file status is FILESTAT-CONN.
           select profile-file assign to "profiles.dat"
               organization is line sequential
               file status is FILESTAT-PROFILE.
           select InpFile assign to "InCollege-Input.txt"
               organization is line sequential
               file status is FILESTAT.
           select OutFile assign to "InCollege-Output.txt"
               organization is line sequential
               file status is FILESTAT-Out.

       data division.
       file section.
       fd  connection-file.
       01  connection-line           pic x(200).

       fd  profile-file.
       01  profile-line              pic x(1500).

       fd  InpFile.
       01  InpRecord                 pic x(200).

       fd  OutFile.
       01  OutRecord                 pic x(80).

       working-storage section.
       01  FILESTAT-CONN             pic xx.
       01  FILESTAT-PROFILE          pic xx.
       01  FILESTAT                  pic xx.
       01  FILESTAT-Out              pic xx.
       01  WS-EOF                    pic x value "N".
       01  WS-DISPLAY                pic x(80).
       01  WS-USER-CHOICE            pic 9 value 0.
       01  current-user              pic x(32).
       
       01  connection-data.
           05  conn-from-user        pic x(32).
           05  conn-to-user          pic x(32).
           05  conn-status           pic x(10).
       
       01  PARSE-FIELDS.
           05 PARSE-FIELD occurs 50 times pic x(200).
       
       01  connection-count          pic 9(03) value 0.
       01  other-user                pic x(32).
       01  profile-firstname         pic x(50).
       01  profile-lastname          pic x(50).
       01  ws-profile-found          pic x value "n".

       procedure division.
       main.
           open input InpFile
           if FILESTAT not = "00"
              display "ERROR opening input file"
              stop run
           end-if

           open output OutFile
           if FILESTAT-Out not = "00"
              display "ERROR opening output file"
              stop run
           end-if

           move "Enter your username:" to WS-DISPLAY
           perform say
           read InpFile into current-user
              at end move "Y" to WS-EOF
           end-read
           move function trim(current-user) to current-user

           perform view-my-network-menu

           close InpFile
           close OutFile
           stop run.

       view-my-network-menu.
           perform until WS-USER-CHOICE = 3 or WS-EOF = "Y"
               move "--- My Network ---" to WS-DISPLAY
               perform say
               move "1. View My Connections" to WS-DISPLAY
               perform say
               move "2. Disconnect from a Connection" to WS-DISPLAY
               perform say
               move "3. Go Back" to WS-DISPLAY
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
                           perform display-connections-with-names
                       when 2
                           perform disconnect-from-user
                       when 3
                           continue
                       when other
                           move "Invalid choice. Please enter 1, 2, or 3." to WS-DISPLAY
                           perform say
                   end-evaluate
               end-if
           end-perform
           .

       display-connections-with-names.
           move "--- My Connections ---" to WS-DISPLAY
           perform say
           move 0 to connection-count
           
           open input connection-file
           if FILESTAT-CONN = "00"
               perform until 1 = 2
                   read connection-file into connection-line
                       at end exit perform
                   end-read
                   
                   move spaces to conn-from-user
                   move spaces to conn-to-user
                   move spaces to conn-status
                   unstring connection-line delimited by "|" into
                       conn-from-user
                       conn-to-user
                       conn-status
                   end-unstring
                   
                   if function upper-case(function trim(conn-status)) = "CONNECTED"
                      if function upper-case(function trim(conn-from-user)) = 
                         function upper-case(function trim(current-user))
                         add 1 to connection-count
                         move function trim(conn-to-user) to other-user
                         perform get-user-name
                         perform display-connection-entry
                      else
                         if function upper-case(function trim(conn-to-user)) = 
                            function upper-case(function trim(current-user))
                            add 1 to connection-count
                            move function trim(conn-from-user) to other-user
                            perform get-user-name
                            perform display-connection-entry
                         end-if
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

       get-user-name.
           move "n" to ws-profile-found
           move spaces to profile-firstname
           move spaces to profile-lastname
           
           open input profile-file
           if FILESTAT-PROFILE = "00"
               perform until 1 = 2
                   read profile-file into profile-line
                       at end exit perform
                   end-read
                   
                   if profile-line(1:function length(function trim(other-user))) = 
                      function trim(other-user)
                       and profile-line(function length(function trim(other-user)) + 1:1) = "|"
                       move "y" to ws-profile-found
                       perform parse-profile-for-name
                       exit perform
                   end-if
               end-perform
               close profile-file
           end-if
           .

       parse-profile-for-name.
           move spaces to PARSE-FIELD(1)
           move spaces to PARSE-FIELD(2)
           move spaces to PARSE-FIELD(3)
           
           unstring profile-line delimited by "|" into
               PARSE-FIELD(1)
               PARSE-FIELD(2)
               PARSE-FIELD(3)
           end-unstring
           
           move function trim(PARSE-FIELD(2)) to profile-firstname
           move function trim(PARSE-FIELD(3)) to profile-lastname
           .

       display-connection-entry.
           move "================================" to WS-DISPLAY
           perform say
           move spaces to WS-DISPLAY
           string connection-count ". Username: " function trim(other-user)
                  delimited by size into WS-DISPLAY
           perform say
           
           if ws-profile-found = "y"
               move spaces to WS-DISPLAY
               string "   Name: " function trim(profile-firstname) " " 
                      function trim(profile-lastname) delimited by size into WS-DISPLAY
               perform say
           else
               move "   Name: (No profile available)" to WS-DISPLAY
               perform say
           end-if
           .

       disconnect-from-user.
           move "--- Disconnect from Connection ---" to WS-DISPLAY
           perform say
           move "This feature will be implemented in a future update." to WS-DISPLAY
           perform say
           move " " to WS-DISPLAY
           perform say
           .

       say.
           display WS-DISPLAY
           move WS-DISPLAY to OutRecord
           write OutRecord
           .