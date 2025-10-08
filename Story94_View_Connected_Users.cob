identification division.
       program-id. ITK94ViewConnections.

       environment division.
       input-output section.
       file-control.
           select connection-file assign to "connections.dat"
               organization is line sequential
               file status is FILESTAT-CONN.
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

       fd  InpFile.
       01  InpRecord                 pic x(200).

       fd  OutFile.
       01  OutRecord                 pic x(80).

       working-storage section.
       01  FILESTAT-CONN             pic xx.
       01  FILESTAT                  pic xx.
       01  FILESTAT-Out              pic xx.
       01  WS-EOF                    pic x value "N".
       01  WS-DISPLAY                pic x(80).
       01  current-user              pic x(32).
       
       01  connection-data.
           05  conn-from-user        pic x(32).
           05  conn-to-user          pic x(32).
           05  conn-status           pic x(10).
       
       01  connection-count          pic 9(03) value 0.
       01  other-user                pic x(32).

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

           perform view-all-connections

           close InpFile
           close OutFile
           stop run.

       view-all-connections.
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
                         move spaces to WS-DISPLAY
                         string connection-count ". " function trim(other-user)
                                delimited by size into WS-DISPLAY
                         perform say
                      else
                         if function upper-case(function trim(conn-to-user)) = 
                            function upper-case(function trim(current-user))
                            add 1 to connection-count
                            move function trim(conn-from-user) to other-user
                            move spaces to WS-DISPLAY
                            string connection-count ". " function trim(other-user)
                                   delimited by size into WS-DISPLAY
                            perform say
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

       say.
           display WS-DISPLAY
           move WS-DISPLAY to OutRecord
           write OutRecord
           .