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
