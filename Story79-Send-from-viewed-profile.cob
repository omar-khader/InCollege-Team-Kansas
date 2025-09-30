*> Integration: call 'perform cr-offer-send-menu' at the end of display-search-result.
*> Relies on existing variables/routines in InCollege (1).cob:
*>   current-user, temp-profile-username, temp-profile-firstname/lastname
*>   connection-file, connection-line, FILESTAT-CONN, check-existing-connections
*>   WS-DISPLAY, say, InpFile, InpRecord, WS-USER-CHOICE

       cr-offer-send-menu.
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
           .

       send-connection-request-from-profile.
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
           .
