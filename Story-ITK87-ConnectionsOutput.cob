*=====================================================================
*  STORY: ITK-87 — Write all connection-requests output to a file
*  FILE:  Story-ITK87-ConnectionsOutput.cob
*
*  HOW TO MERGE:
*    (A) ENVIRONMENT DIVISION / file-control  -> paste SELECT block
*    (B) DATA DIVISION / FILE SECTION         -> paste FD block
*    (C) WORKING-STORAGE SECTION              -> paste WS items
*    (D) PROCEDURE DIVISION                   -> replace 'say.' with this one
*                                               and append the two helpers.
*    (E) In CR flows, wrap with:
*          perform cr-begin-log
*              ... (your existing prints via 'say')
*          perform cr-end-log
*        Suggested places: 
*          - cr-offer-send-menu.
*          - send-connection-request-from-profile.
*          - send-connection-request.
*          - view-pending-requests. / cr-view-pending-requests.
*=====================================================================

*---------------- (A) ENVIRONMENT DIVISION / file-control -------------
* Paste inside 'file-control.' after the OutFile SELECT
           select ConnOutFile assign to "Connections-Output.txt"
               organization is line sequential
               file status is FILESTAT-ConnOut.

*---------------- (B) DATA DIVISION / FILE SECTION --------------------
* Paste with the other FDs (after OutFile FD is a good spot)
       fd  ConnOutFile.
       01  ConnOutRecord            pic x(80).

*---------------- (C) WORKING-STORAGE SECTION -------------------------
* Add these with your other file-status and flags

       01  FILESTAT-ConnOut         pic xx.
       01  WS-CR-LOGGING            pic x value "N".

*---------------- (D) PROCEDURE DIVISION: replace 'say.' --------------
* Replace your entire existing 'say.' paragraph with this one:

       say.
      *>>  Mirrors every display line to InCollege-Output.txt (existing),
      *>>  and, when WS-CR-LOGGING = "Y", also to Connections-Output.txt.
           display WS-DISPLAY
           move WS-DISPLAY to OutRecord
           write OutRecord

           if WS-CR-LOGGING = "Y"
              open extend ConnOutFile
              if FILESTAT-ConnOut not = "00"
                 open output ConnOutFile
                 close ConnOutFile
                 open extend ConnOutFile
              end-if
              move WS-DISPLAY to ConnOutRecord
              write ConnOutRecord
              close ConnOutFile
           end-if
           .

*---------------- (E) PROCEDURE DIVISION: helpers (append) ------------
* Append these two paragraphs anywhere convenient (e.g., near 'say.')

       cr-begin-log.
           move "Y" to WS-CR-LOGGING
           .

       cr-end-log.
           move "N" to WS-CR-LOGGING
           .
