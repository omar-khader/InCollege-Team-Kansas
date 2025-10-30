identification division.
program-id. Story35.

environment division.
input-output section.
file-control.
    select InpFile assign to "Story35-Input.txt"
        organization is line sequential
        file status is FILESTAT-In.

    select OutFile assign to "Story35-Output.txt"
        organization is line sequential
        file status is FILESTAT-Out.

    select user-file assign to "users.dat"
        organization is line sequential
        file status is FILESTAT-Users.

    select conn-file assign to "connections.dat"
        organization is line sequential
        file status is FILESTAT-Conn.

    select message-file assign to "messages.dat"
        organization is line sequential
        file status is FILESTAT-Msg.

data division.
file section.
fd InpFile.
01 InpRecord pic x(200).

fd OutFile.
01 OutRecord pic x(80).

fd message-file.
01 message-record pic x(500).

fd user-file.
01 user-record pic x(100).

fd conn-file.
01 conn-record pic x(100).

working-storage section.
01 FILESTAT-In pic xx.
01 FILESTAT-Out pic xx.
01 FILESTAT-Users pic xx.
01 FILESTAT-Conn pic xx.
01 FILESTAT-Msg pic xx.

01 WS-EOF pic x value "N".
01 WS-DISPLAY pic x(80).
01 WS-USER-CHOICE pic 9.

01 current-user pic x(32) value "jsmith123".
01 target-username pic x(32) value "mjones456".
01 temp-input pic x(200).

01 ws-msg-choice pic 9 value 0.
01 ws-recipient-valid pic x value "n".
01 ws-is-connected pic x value "n".
01 ws-found pic x value "n".

01 f-user pic x(32).
01 f-pass pic x(64).

01 conn-from-user pic x(32).
01 conn-to-user pic x(32).
01 conn-status pic x(10).

01 message-data.
    05 msg-sender pic x(32).
    05 msg-recipient pic x(32).
    05 msg-content pic x(200).
    05 msg-timestamp pic x(20).


procedure division.
main-logic.
    open input InpFile
    if FILESTAT-In not = "00"
        display "ERROR opening Story124-Input.txt"
        stop run
    end-if

    open output OutFile
    if FILESTAT-Out not = "00"
        display "ERROR opening Story124-Output.txt"
        stop run
    end-if

     open input message-file
    if FILESTAT-Msg = "35"
        open output message-file
        close message-file
    else
        close message-file
    end-if

    *> Simulate login - read username from input
    move "Logged in as jsmith123. Testing sending messages to mjones456." to WS-DISPLAY
    perform say

    *> Go to send-new=message
    perform send-new-message

    close InpFile
    close OutFile
    stop run
    .

say.
    display WS-DISPLAY
    move WS-DISPLAY to OutRecord
    write OutRecord
    .



send-new-message.
    move "--- Send a messaging to mjones456 ---" to WS-DISPLAY
    perform say

    *> Validate recipient exists and is connected
    perform validate-message-recipient

    if ws-is-connected = "n"
        exit paragraph
    end-if

    *> Get message content
    move "Enter your message (max 200 chars):" to WS-DISPLAY
    perform say

    read InpFile into temp-input
        at end move "Y" to WS-EOF exit paragraph
    end-read

    *> Truncate if too long
    if function length(function trim(temp-input)) > 200
        move temp-input(1:200) to msg-content
    else
        move function trim(temp-input) to msg-content
    end-if

    *> Save the message
    perform save-message

    move spaces to WS-DISPLAY
    string "Message sent to " function trim(target-username)
           " successfully!" delimited by size into WS-DISPLAY
    perform say
    move "---------------------" to WS-DISPLAY
    perform say
    .

validate-message-recipient.
    move "n" to ws-is-connected

    *> First check if user exists
    move "n" to ws-found
    open input user-file
    if FILESTAT-Users = "00"
        perform until 1 = 2
            read user-file into user-record
                at end exit perform
            end-read
            unstring user-record delimited by ","
                into f-user f-pass
            end-unstring
            if function trim(f-user) = target-username
                move "y" to ws-found
                exit perform
            end-if
        end-perform
        close user-file
    end-if

    if ws-found = "n"
        move "User not found." to WS-DISPLAY
        perform say
        exit paragraph
    end-if

    *> Check if it's the same user
    if function trim(target-username) = function trim(current-user)
        move "You cannot send a message to yourself." to WS-DISPLAY
        perform say
        exit paragraph
    end-if

    *> Check if connected
    open input conn-file
    if FILESTAT-Conn = "00"
        perform until 1 = 2
            read conn-file into conn-record
                at end exit perform
            end-read

            unstring conn-record delimited by "|" into
                conn-from-user
                conn-to-user
                conn-status
            end-unstring

            if function trim(conn-status) = "connected"
                if (function trim(conn-from-user) = current-user
                    and function trim(conn-to-user) = target-username)
                   or (function trim(conn-to-user) = current-user
                    and function trim(conn-from-user) = target-username)
                    move "y" to ws-is-connected
                    exit perform
                end-if
            end-if
        end-perform
        close conn-file
    end-if

    if ws-is-connected = "n"
        move "You can only message users you are connected with."
            to WS-DISPLAY
        perform say
    .

save-message.
    move function trim(current-user) to msg-sender
    move function trim(target-username) to msg-recipient

    *> Create simple timestamp (optional)
    move function current-date to msg-timestamp

    open extend message-file
    if FILESTAT-Msg not = "00"
        open output message-file
        close message-file
        open extend message-file
    end-if

    if FILESTAT-Msg not = "00"
        move "Error: Could not save message." to WS-DISPLAY
        perform say
        exit paragraph
    end-if

    *> Format: sender|recipient|content|timestamp
    move spaces to message-record
    string
        function trim(msg-sender) "|"
        function trim(msg-recipient) "|"
        function trim(msg-content) "|"
        function trim(msg-timestamp)
        delimited by size
        into message-record
    end-string

    write message-record
    close message-file
    .
