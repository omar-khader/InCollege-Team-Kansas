identification division.
       program-id. ITK98OutputTest.

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
       01  temp-input                pic x(200).
       
       01  connection-data.
           05  conn-from-user        pic x(32).
           05  conn-to-user          pic x(32).
           05  conn-status           pic x(10).
       
       01  CONNECTIONS-TABLE.
           05 CONNECTION-ENTRY occurs 100 times pic x(200).
       01  CONNECTIONS-COUNT         pic 9(03) value 0.
       
       01  PARSE-FIELDS.
           05 PARSE-FIELD occurs 50 times pic x(200).
       
       01  connection-count          pic 9(03) value 0.
       01  pending-count             pic 9(03) value 0.
       01  ws-i                      pic 9(03) value 0.
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

           move "=== Connection Management Test ===" to WS-DISPLAY
           perform say
           
           move "Enter your username:" to WS-DISPLAY
           perform say
           read InpFile into current-user
              at end move "Y" to WS-EOF
           end-read
           move function trim(current-user) to current-user

           perform test-all-connection-features

           close InpFile
           close OutFile
           stop run.

       test-all-connection-features.
           move " " to WS-DISPLAY
           perform say
           move "TEST 1: View Pending Requests" to WS-DISPLAY
           perform say
           move "================================" to WS-DISPLAY
           perform say
           perform view-pending-requests
           
           move " " to WS-DISPLAY
           perform say
           move "TEST 2: View All Connections" to WS-DISPLAY
           perform say
           move "================================" to WS-DISPLAY
           perform say
           perform view-all-connections
           
           move " " to WS-DISPLAY
           perform say
           move "TEST 3: Remove Accepted/Rejected Requests" to WS-DISPLAY
           perform say
           move "================================" to WS-DISPLAY
           perform say
           perform remove-processed-requests
           
           move " " to WS-DISPLAY
           perform say
           move "All connection-related output has been written to InCollege-Output.txt" to WS-DISPLAY
           perform say
           .

       view-pending-requests.
           move "--- Pending Connection Requests ---" to WS-DISPLAY
           perform say
           move 0 to pending-count
           
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
                   
                   if function upper-case(function trim(conn-to-user)) = 
                      function upper-case(function trim(current-user))
                       and function upper-case(function trim(conn-status)) = "PENDING"
                       add 1 to pending-count
                       move spaces to WS-DISPLAY
                       string "Request from: " function trim(conn-from-user) 
                              delimited by size into WS-DISPLAY
                       perform say
                   end-if
               end-perform
               close connection-file
           end-if
           
           if pending-count = 0
               move "You have no pending connection requests at this time." to WS-DISPLAY
               perform say
           else
               move " " to WS-DISPLAY
               perform say
               move spaces to WS-DISPLAY
               string "Total pending requests: " pending-count 
                      delimited by size into WS-DISPLAY
               perform say
           end-if
           
           move " " to WS-DISPLAY
           perform say
           .

       view-all-connections.
           move "--- My Connections ---" to WS-DISPLAY
           perform say
           move 0 to connection-count
           
           open input connection-file
           if FILESTAT-CONN = "00"
               perform until 1 = 2
                   read connection-line into connection-line
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
           .

       remove-processed-requests.
           move 0 to CONNECTIONS-COUNT
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
                   
                   if function upper-case(function trim(conn-to-user)) = 
                      function upper-case(function trim(current-user))
                      if function upper-case(function trim(conn-status)) = "ACCEPTED"
                         or function upper-case(function trim(conn-status)) = "REJECTED"
                         continue
                      else
                         add 1 to CONNECTIONS-COUNT
                         move connection-line to CONNECTION-ENTRY(CONNECTIONS-COUNT)
                      end-if
                   else
                      add 1 to CONNECTIONS-COUNT
                      move connection-line to CONNECTION-ENTRY(CONNECTIONS-COUNT)
                   end-if
               end-perform
               close connection-file
           end-if

           open output connection-file
           if FILESTAT-CONN not = "00"
               move "Error updating connection file" to WS-DISPLAY
               perform say
               close connection-file
               exit paragraph
           end-if

           perform varying ws-i from 1 by 1 until ws-i > CONNECTIONS-COUNT
               move CONNECTION-ENTRY(ws-i) to connection-line
               write connection-line
           end-perform
           close connection-file

           move "Accepted/rejected requests removed successfully." to WS-DISPLAY
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

       say.
           display WS-DISPLAY
           move WS-DISPLAY to OutRecord
           write OutRecord
           .