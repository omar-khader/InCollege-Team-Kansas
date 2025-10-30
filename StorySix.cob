identification division.
program-id. StorySix.

environment division.
input-output section.
file-control.
    select InpFile assign to "StorySix-Input.txt"
        organization is line sequential
        file status is FILESTAT-In.

    select OutFile assign to "StorySix-Output.txt"
        organization is line sequential
        file status is FILESTAT-Out.

data division.
file section.
fd InpFile.
01 InpRecord pic x(80).

fd OutFile.
01 OutRecord pic x(80).

working-storage section.
01 FILESTAT-In pic xx.
01 FILESTAT-Out pic xx.
01 WS-EOF pic x value "N".
01 WS-DISPLAY pic x(80).
01 WS-USER-CHOICE pic 9.

procedure division.
main-logic.
    open input InpFile
    open output OutFile
    perform post-login-menu
    close InpFile
    close OutFile
    stop run
    .

say.
    display WS-DISPLAY
    move WS-DISPLAY to OutRecord
    write OutRecord
    .

post-login-menu.
    perform until WS-EOF = "Y"
        move "1. Create/Edit My Profile" to WS-DISPLAY
        perform say

        move "2. Search for a job" to WS-DISPLAY
        perform say

        move "3. View My Profile" to WS-DISPLAY
        perform say

        move "4. Find someone you know" to WS-DISPLAY
        perform say

        move "5. View My Network" to WS-DISPLAY
        perform say

        move "6. Learn a new skill" to WS-DISPLAY
        perform say

        move "7. View My Pending Connection Requests" to WS-DISPLAY
        perform say

        move "8. Messages" to WS-DISPLAY
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
                    perform create-edit-profile
                when 2
                    perform job-search-menu
                when 3
                    perform view-profile
                when 4
                    perform search-for-user
                when 5
                    perform view-my-network
                when 6
                    perform show-skill-menu
                when 7
                    perform cr-view-pending-requests
                when 8
                    perform open-messages-menu
                when other
                    exit perform
            end-evaluate
        end-if
    end-perform
    .

open-messages-menu.
    move "You have chosen Messages. Any further actions are not implemented in this story." to WS-DISPLAY
    perform say
    .

create-edit-profile.
    move "Create/Edit Profile - Not implemented" to WS-DISPLAY
    perform say
    .

job-search-menu.
    move "Job Search - Not implemented" to WS-DISPLAY
    perform say
    .

view-profile.
    move "View Profile - Not implemented" to WS-DISPLAY
    perform say
    .

search-for-user.
    move "Search for User - Not implemented" to WS-DISPLAY
    perform say
    .

view-my-network.
    move "View Network - Not implemented" to WS-DISPLAY
    perform say
    .

show-skill-menu.
    move "Learn Skill - Not implemented" to WS-DISPLAY
    perform say
    .

cr-view-pending-requests.
    move "Pending Requests - Not implemented" to WS-DISPLAY
    perform say
    .
