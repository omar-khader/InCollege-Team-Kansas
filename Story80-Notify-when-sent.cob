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
