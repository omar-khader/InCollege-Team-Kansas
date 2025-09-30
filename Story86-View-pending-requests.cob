*> Lists all entries in connections.dat where conn-u2 = current-user and status=pending.
       cr-view-pending-requests.
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
           .
