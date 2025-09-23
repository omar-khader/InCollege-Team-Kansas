identification division.
       program-id. Story4-modified-from-story-3.

       environment division.
       input-output section.
       file-control.
           select profile-file assign to "profiles.dat"
               organization is line sequential
               file status is FILESTAT-PROFILE.
           select InpFile assign to "InCollege-Input.txt"
               organization is line sequential
               file status is FILESTAT.
           select OutFile assign to "InCollege-Output.txt"
               organization is line sequential
               file status is FILESTAT-Out.

       data division.
       file section.
       fd  profile-file.
       01  profile-line              pic x(1500).

       fd  InpFile.
       01  InpRecord                 pic x(200).

       fd  OutFile.
       01  OutRecord                 pic x(80).

       working-storage section.
       01  FILESTAT-PROFILE          pic xx.
       01  FILESTAT                  pic xx.
       01  FILESTAT-Out              pic xx.

       01  WS-EOF                    pic x value "N".
       01  WS-DISPLAY                pic x(200).

       01  search-firstname          pic x(50).
       01  search-lastname           pic x(50).
       01  search-results-count      pic 9(02) value 0.
      *>    Temporary profile data structure for search results
       01  temp-profile-data.
           05  temp-profile-username      pic x(32).
           05  temp-profile-firstname     pic x(50).
           05  temp-profile-lastname      pic x(50).
           05  temp-profile-university    pic x(100).
           05  temp-profile-major         pic x(50).
           05  temp-profile-grad-year      pic 9(4).
           05  temp-profile-about-me       pic x(200).
           05  temp-profile-exp-count     pic 9.
           05  temp-profile-experiences   occurs 3 times.
               10  temp-exp-title         pic x(50).
               10  temp-exp-company       pic x(50).
               10  temp-exp-dates         pic x(30).
               10  temp-exp-description   pic x(100).
           05  temp-profile-edu-count     pic 9.
           05  temp-profile-educations    occurs 3 times.
               10  temp-edu-degree        pic x(50).
               10  temp-edu-university    pic x(100).
               10  temp-edu-years         pic x(30).

       01  PARSE-FIELDS.
           05 PARSE-FIELD occurs 50 times pic x(200).
       01  ws-parse-idx              pic 9(02) value 0.
       01  ws-field-num              pic 9(02) value 0.
       01  ws-i                      pic 9(03) value 0.


       procedure division.

      *> STORY 4 and 5.
      *> Modified from Omar's story three code.
      *> Consists of a formatted search profile view based on
      *> provided example output

       main.
           open input profile-file
           if FILESTAT-PROFILE = "35"
              open output profile-file
              close profile-file
           end-if
           close profile-file

           open input InpFile
           if FILESTAT not = "00"
              display "ERROR opening InCollege-Input.txt, status: " FILESTAT
              stop run
           end-if

           open output OutFile
           if FILESTAT-Out not = "00"
              display "ERROR opening InCollege-Output.txt, status: " FILESTAT-Out
              stop run
           end-if

           perform search-for-user

           close InpFile
           close OutFile
           stop run.

       search-for-user.
      *>    STORY 3: New feature - Search for users by full name
      *>   Performs case-insensitive search across all profiles
      *>    Displays matching users with their basic information
           move "--- Search for User ---" to WS-DISPLAY
           perform say
           move "Enter the first name of the person you're looking for:" to WS-DISPLAY
           perform say

           read InpFile into InpRecord
               at end move "Y" to WS-EOF exit paragraph
           end-read
           move function trim(InpRecord) to search-firstname

           move "Enter the last name of the person you're looking for:" to WS-DISPLAY
           perform say

           read InpFile into InpRecord
               at end move "Y" to WS-EOF exit paragraph
           end-read
           move function trim(InpRecord) to search-lastname

           move 0 to search-results-count

           move " " to WS-DISPLAY
           perform say
           move "Searching..." to WS-DISPLAY
           perform say
           move " " to WS-DISPLAY
           perform say

           open input profile-file
           if FILESTAT-PROFILE = "00"
               perform until 1 = 2
                   read profile-file into profile-line
                       at end exit perform
                   end-read

                   perform parse-searched-profile-line-complete

      *>            STORY 3: Case-insensitive comparison for both names
                   if function upper-case(function trim(temp-profile-firstname)) =
                      function upper-case(function trim(search-firstname))
                      and function upper-case(function trim(temp-profile-lastname)) =
                          function upper-case(function trim(search-lastname))
                       add 1 to search-results-count
                       perform display-search-result
                   end-if
               end-perform
               close profile-file
           end-if

      *>        STORY 3: Handle no results case
           if search-results-count = 0
               move "No one by that name could be found." to WS-DISPLAY
               perform say
           else
               move " " to WS-DISPLAY
               perform say
               move spaces to WS-DISPLAY
               string search-results-count " user(s) found." delimited by size into WS-DISPLAY
               perform say
           end-if

           move " " to WS-DISPLAY
           perform say
           .

       parse-searched-profile-line-complete.
           perform varying ws-parse-idx from 1 by 1 until ws-parse-idx > 50
               move spaces to PARSE-FIELD(ws-parse-idx)
           end-perform

           unstring profile-line delimited by "|" into
               PARSE-FIELD(1)
               PARSE-FIELD(2)
               PARSE-FIELD(3)
               PARSE-FIELD(4)
               PARSE-FIELD(5)
               PARSE-FIELD(6)
               PARSE-FIELD(7)
               PARSE-FIELD(8)
               PARSE-FIELD(9)
               PARSE-FIELD(10)
               PARSE-FIELD(11)
               PARSE-FIELD(12)
               PARSE-FIELD(13)
               PARSE-FIELD(14)
               PARSE-FIELD(15)
               PARSE-FIELD(16)
               PARSE-FIELD(17)
               PARSE-FIELD(18)
               PARSE-FIELD(19)
               PARSE-FIELD(20)
               PARSE-FIELD(21)
               PARSE-FIELD(22)
               PARSE-FIELD(23)
               PARSE-FIELD(24)
               PARSE-FIELD(25)
               PARSE-FIELD(26)
               PARSE-FIELD(27)
               PARSE-FIELD(28)
               PARSE-FIELD(29)
               PARSE-FIELD(30)
               PARSE-FIELD(31)
               PARSE-FIELD(32)
           end-unstring

           move function trim(PARSE-FIELD(2)) to temp-profile-firstname
           move function trim(PARSE-FIELD(3)) to temp-profile-lastname
           move function trim(PARSE-FIELD(4)) to temp-profile-university
           move function trim(PARSE-FIELD(5)) to temp-profile-major
           if function trim(PARSE-FIELD(6)) not = spaces
               move function numval(function trim(PARSE-FIELD(6))) to temp-profile-grad-year
           else
               move 0 to temp-profile-grad-year
           end-if
           move function trim(PARSE-FIELD(7)) to temp-profile-about-me

           if function trim(PARSE-FIELD(8)) not = spaces
               move function numval(function trim(PARSE-FIELD(8))) to temp-profile-exp-count
           else
               move 0 to temp-profile-exp-count
           end-if

           if temp-profile-exp-count < 0 move 0 to temp-profile-exp-count end-if
           if temp-profile-exp-count > 3 move 3 to temp-profile-exp-count end-if

      *>    STORY 1 Fix: Corrected field indexing for proper data extraction
           move 9 to ws-field-num

           if temp-profile-exp-count >= 1
               move function trim(PARSE-FIELD(ws-field-num)) to temp-exp-title(1)
               move function trim(PARSE-FIELD(ws-field-num + 1)) to temp-exp-company(1)
               move function trim(PARSE-FIELD(ws-field-num + 2)) to temp-exp-dates(1)
               move function trim(PARSE-FIELD(ws-field-num + 3)) to temp-exp-description(1)
               add 4 to ws-field-num
           end-if

           if temp-profile-exp-count >= 2
               move function trim(PARSE-FIELD(ws-field-num)) to temp-exp-title(2)
               move function trim(PARSE-FIELD(ws-field-num + 1)) to temp-exp-company(2)
               move function trim(PARSE-FIELD(ws-field-num + 2)) to temp-exp-dates(2)
               move function trim(PARSE-FIELD(ws-field-num + 3)) to temp-exp-description(2)
               add 4 to ws-field-num
           end-if

           if temp-profile-exp-count >= 3
               move function trim(PARSE-FIELD(ws-field-num)) to temp-exp-title(3)
               move function trim(PARSE-FIELD(ws-field-num + 1)) to temp-exp-company(3)
               move function trim(PARSE-FIELD(ws-field-num + 2)) to temp-exp-dates(3)
               move function trim(PARSE-FIELD(ws-field-num + 3)) to temp-exp-description(3)
               add 4 to ws-field-num
           end-if

           if function trim(PARSE-FIELD(ws-field-num)) not = spaces
               move function numval(function trim(PARSE-FIELD(ws-field-num))) to temp-profile-edu-count
           else
               move 0 to temp-profile-edu-count
           end-if
           add 1 to ws-field-num

           if temp-profile-edu-count < 0 move 0 to temp-profile-edu-count end-if
           if temp-profile-edu-count > 3 move 3 to temp-profile-edu-count end-if

           if temp-profile-edu-count >= 1
               move function trim(PARSE-FIELD(ws-field-num)) to temp-edu-degree(1)
               move function trim(PARSE-FIELD(ws-field-num + 1)) to temp-edu-university(1)
               move function trim(PARSE-FIELD(ws-field-num + 2)) to temp-edu-years(1)
               add 3 to ws-field-num
           end-if

           if temp-profile-edu-count >= 2
               move function trim(PARSE-FIELD(ws-field-num)) to temp-edu-degree(2)
               move function trim(PARSE-FIELD(ws-field-num + 1)) to temp-edu-university(2)
               move function trim(PARSE-FIELD(ws-field-num + 2)) to temp-edu-years(2)
               add 3 to ws-field-num
           end-if

           if temp-profile-edu-count >= 3
               move function trim(PARSE-FIELD(ws-field-num)) to temp-edu-degree(3)
               move function trim(PARSE-FIELD(ws-field-num + 1)) to temp-edu-university(3)
               move function trim(PARSE-FIELD(ws-field-num + 2)) to temp-edu-years(3)
               add 3 to ws-field-num
           end-if
           .



       display-search-result.
      *>    STORY 4: Display formatted search result for a matching user
      *>    Based off provided example output
           move "========Found User Profile========" to WS-DISPLAY
           perform say
           move spaces to WS-DISPLAY
           string "Name: " function trim(temp-profile-firstname) " "
                  function trim(temp-profile-lastname) delimited by size into WS-DISPLAY
           perform say
           move spaces to WS-DISPLAY
           string "University: " function trim(temp-profile-university) delimited by size into WS-DISPLAY
           perform say
           move spaces to WS-DISPLAY
           string "Major: " function trim(temp-profile-major) delimited by size into WS-DISPLAY
           perform say
           move spaces to WS-DISPLAY
           string "Graduation Year: " function trim(temp-profile-grad-year) delimited by size into WS-DISPLAY
           perform say
           move spaces to WS-DISPLAY

           *> Display optional About Me section if provided
           if function trim(temp-profile-about-me) not = spaces
               move spaces to WS-DISPLAY
               string "About me: " function trim(temp-profile-about-me)
                delimited by size into WS-DISPLAY
               PERFORM SAY
           end-if
           move "==================================" to WS-DISPLAY
           perform say

           *> Display all experience entries with proper formatting
           if temp-profile-exp-count > 0
               move " " to WS-DISPLAY
               perform say
               move "--- Professional Experience ---" to WS-DISPLAY
               perform say
               perform varying ws-i from 1 by 1 until ws-i > temp-profile-exp-count
                   move " " to WS-DISPLAY
                   perform say
                   move spaces to WS-DISPLAY
                   string "Experience #" ws-i ":" delimited by size into WS-DISPLAY
                   perform say
                   move spaces to WS-DISPLAY
                   string "  Title: " function trim(temp-exp-title(ws-i)) delimited by size into WS-DISPLAY
                   perform say
                   move spaces to WS-DISPLAY
                   string "  Company: " function trim(temp-exp-company(ws-i)) delimited by size into WS-DISPLAY
                   perform say
                   move spaces to WS-DISPLAY
                   string "  Dates: " function trim(temp-exp-dates(ws-i)) delimited by size into WS-DISPLAY
                   perform say
                   if function trim(temp-exp-description(ws-i)) not = spaces
                   move spaces to WS-DISPLAY
                   string "  Description: " function trim(temp-exp-description(ws-i)) delimited by size into WS-DISPLAY
                   perform say
                   end-if
               end-perform
           else
      *>           Show message when no experience entries exist
               move " " to WS-DISPLAY
               perform say
               move "--- Professional Experience ---" to WS-DISPLAY
               perform say
               move "  No experience entries added." to WS-DISPLAY
               perform say
           end-if

      *>   STORY 1: Display all education entries with proper formatting
           if temp-profile-edu-count > 0
               move " " to WS-DISPLAY
               perform say
               move "--- Education ---" to WS-DISPLAY
               perform say
               perform varying ws-i from 1 by 1 until ws-i > temp-profile-edu-count
                   move " " to WS-DISPLAY
                   perform say
                   move spaces to WS-DISPLAY
                   string "Education #" ws-i ":" delimited by size into WS-DISPLAY
                   perform say
                   move spaces to WS-DISPLAY
                   string "  Degree: " function trim(temp-edu-degree(ws-i)) delimited by size into WS-DISPLAY
                   perform say
                   move spaces to WS-DISPLAY
                   string "  University: " function trim(temp-edu-university(ws-i)) delimited by size into WS-DISPLAY
                   perform say
                   move spaces to WS-DISPLAY
                   string "  Years: " function trim(temp-edu-years(ws-i)) delimited by size into WS-DISPLAY
                   perform say
               end-perform
           else
               move " " to WS-DISPLAY
               perform say
               move "--- Education ---" to WS-DISPLAY
               perform say
               move "  No education entries added." to WS-DISPLAY
               perform say
           end-if
           .

       say.
      *>    STORY 3: All screen output is written to InCollege-Output.txt
      *>    This includes search results for easy verification
           display function trim(WS-DISPLAY)
           move WS-DISPLAY to OutRecord
           write OutRecord
           .
