identification division.
       program-id. ITK93RemoveRequests.

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
       01  ws-choice                 pic 9 value 0.
       
       01  connection-data.
           05  conn-from-user        pic x(32).
           05  conn-to-user          pic x(32).
           05  conn-status           pic x(10).
       
       01  CONNECTIONS-TABLE.
           05 CONNECTION-ENTRY occurs 100 times pic x(200).
       01  CONNECTIONS-COUNT         pic 9(03) value 0.
       01  ws-i                      pic 9(03) value 0.
       01  request-username          pic x(32).
       01  requests-found            pic 9(02) value 0.

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

           perform remove-processed-requests

           close InpFile
           close OutFile
           stop run.

       remove-processed-requests.
           move "--- Remove Accepted/Rejected Requests ---" to WS-DISPLAY
           perform say
           
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

           move "Accepted/rejected requests have been removed from your pending list." to WS-DISPLAY
           perform say
           .

       say.
           display WS-DISPLAY
           move WS-DISPLAY to OutRecord
           write OutRecord
           .