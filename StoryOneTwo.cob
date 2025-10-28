IDENTIFICATION DIVISION.
PROGRAM-ID. StoryOneTwo.

ENVIRONMENT DIVISION.
INPUT-OUTPUT SECTION.
FILE-CONTROL.
    SELECT InpFile ASSIGN TO "input.txt"
        ORGANIZATION IS LINE SEQUENTIAL.
    SELECT OutFile ASSIGN TO "output.txt"
        ORGANIZATION IS LINE SEQUENTIAL.
    SELECT JobsFile ASSIGN TO "jobs.dat"
        ORGANIZATION IS LINE SEQUENTIAL.

DATA DIVISION.
FILE SECTION.
FD  InpFile.
01  InpRecord               PIC X(100).

FD  OutFile.
01  OutRecord               PIC X(100).

FD  JobsFile.
01  JobRecord               PIC X(500).

WORKING-STORAGE SECTION.
01  WS-DISPLAY              PIC X(100).
01  WS-EOF                  PIC X VALUE "N".
01  WS-JOB-EOF              PIC X VALUE "N".
01  ws-job-choice           PIC 9 VALUE 0.
01  ws-job-counter          PIC 99 VALUE 0.
01  ws-job-detail-choice    PIC 99 VALUE 0.
01  ws-current-job          PIC 99 VALUE 0.

01  JOB-FIELDS.
    05  JOB-USERNAME        PIC X(50).
    05  JOB-TITLE           PIC X(50).
    05  JOB-DESCRIPTION     PIC X(100).
    05  JOB-EMPLOYER        PIC X(50).
    05  JOB-LOCATION        PIC X(50).
    05  JOB-SALARY          PIC X(30).

PROCEDURE DIVISION.
main-procedure.
    open input InpFile
    open output OutFile

    perform job-search-menu

    close InpFile
    close OutFile
    stop run.

job-search-menu.
    perform until ws-job-choice = 3 or WS-EOF = "Y"
        move "--- Job Search/Internship Menu ---" to WS-DISPLAY
        perform say

        move "1. Post a Job/Internship" to WS-DISPLAY
        perform say

        move "2. Browse Jobs/Internships" to WS-DISPLAY
        perform say

        move "3. Back to Main Menu" to WS-DISPLAY
        perform say

        move "Enter your choice:" to WS-DISPLAY
        perform say

        read InpFile into InpRecord
            at end move "Y" to WS-EOF
            not at end
                move function numval(function trim(InpRecord))
                    to ws-job-choice
        end-read

        if WS-EOF = "N"
            evaluate ws-job-choice
                when 1
                    perform post-job-internship
                when 2
                    perform display-job-listings
                when 3
                    continue
                when other
                    move "Invalid choice. Please enter 1, 2, or 3."
                        to WS-DISPLAY
                    perform say
            end-evaluate
        end-if
    end-perform

    *> Reset choice for next time
    move 0 to ws-job-choice
    .

post-job-internship.
    move "Post job functionality not yet implemented." to WS-DISPLAY
    perform say
    .

display-job-listings.
    move "--- Available Job Listings ---" to WS-DISPLAY
    perform say

    *> Open jobs file
    open input JobsFile

    move 0 to ws-job-counter
    move "N" to WS-JOB-EOF

    *> Read and display all jobs
    perform until WS-JOB-EOF = "Y"
        read JobsFile into JobRecord
            at end move "Y" to WS-JOB-EOF
            not at end
                perform parse-job-record
                add 1 to ws-job-counter
                move spaces to WS-DISPLAY
                string ws-job-counter delimited by size
                    ". " delimited by size
                    function trim(JOB-TITLE) delimited by size
                    " at " delimited by size
                    function trim(JOB-EMPLOYER) delimited by size
                    " (" delimited by size
                    function trim(JOB-LOCATION) delimited by size
                    ")" delimited by size
                    into WS-DISPLAY
                end-string
                perform say
        end-read
    end-perform

    close JobsFile

    move "-----------------------------" to WS-DISPLAY
    perform say

    move "Enter job number to view details, or 0 to go back:"
        to WS-DISPLAY
    perform say

    read InpFile into InpRecord
        at end move "Y" to WS-EOF
        not at end
            move function numval(function trim(InpRecord))
                to ws-job-detail-choice
    end-read

    if WS-EOF = "N" and ws-job-detail-choice > 0
        if ws-job-detail-choice <= ws-job-counter
            move "That feature is not yet implemented." to WS-DISPLAY
            perform say
        else
            move "Invalid job number. Please try again."
                to WS-DISPLAY
            perform say
        end-if
    end-if
    .

parse-job-record.
    *> Parse record into individual fields
    unstring JobRecord delimited by "|"
        into JOB-USERNAME
             JOB-TITLE
             JOB-DESCRIPTION
             JOB-EMPLOYER
             JOB-LOCATION
             JOB-SALARY
    end-unstring
    .

say.
    display WS-DISPLAY
    move WS-DISPLAY to OutRecord
    write OutRecord
    .
