identification division.
       program-id. Story1-ViewCompleteProfile.

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
       01  WS-DISPLAY                pic x(80).

       01  current-user              pic x(32).
       01  ws-profile-exists         pic x value "n".
       
       01  profile-data.
           05  profile-username      pic x(32).
           05  profile-firstname     pic x(50).
           05  profile-lastname      pic x(50).
           05  profile-university    pic x(100).
           05  profile-major         pic x(50).
           05  profile-gradyear      pic 9(4).
           05  profile-aboutme       pic x(200).
           05  profile-exp-count     pic 9.
           05  profile-experiences   occurs 3 times.
               10  exp-title         pic x(50).
               10  exp-company       pic x(50).
               10  exp-dates         pic x(30).
               10  exp-description   pic x(100).
           05  profile-edu-count     pic 9.
           05  profile-educations    occurs 3 times.
               10  edu-degree        pic x(50).
               10  edu-university    pic x(100).
               10  edu-years         pic x(30).

       01  PARSE-FIELDS.
           05 PARSE-FIELD occurs 50 times pic x(200).
       01  ws-parse-idx              pic 9(02) value 0.
       01  ws-field-num              pic 9(02) value 0.
       01  ws-i                      pic 9(03) value 0.

       procedure division.
      ******************************************************************
      * STORY 1: View Complete Profile with All Optional Fields
      * 
      * As a logged-in user, I want to view my complete saved profile,
      * including all optional fields, so I can see all my information
      * in one place.
      * 
      * Features:
      * - Enhanced profile display with formatted sections
      * - Shows all optional fields (About Me, Experience, Education)
      * - Displays empty sections with appropriate messages
      * - Easy-to-read format with clear section headers
      ******************************************************************
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

           move "TestUser" to current-user
           perform view-profile

           close InpFile
           close OutFile
           stop run.

       view-profile.
      *    STORY 1: Enhanced profile display with easy-to-read format
      *    Shows all fields including optional ones in organized sections
           move "================================" to WS-DISPLAY
           perform say
           move "         YOUR PROFILE           " to WS-DISPLAY
           perform say
           move "================================" to WS-DISPLAY
           perform say

           perform load-profile-for-view

           if ws-profile-exists = "y"
               move "--- Personal Information ---" to WS-DISPLAY
               perform say
               
               move spaces to WS-DISPLAY
               string "Name: " function trim(profile-firstname) " " 
                      function trim(profile-lastname) delimited by size into WS-DISPLAY
               perform say

               move spaces to WS-DISPLAY
               string "University: " function trim(profile-university) delimited by size into WS-DISPLAY
               perform say

               move spaces to WS-DISPLAY
               string "Major: " function trim(profile-major) delimited by size into WS-DISPLAY
               perform say

               move spaces to WS-DISPLAY
               string "Graduation Year: " profile-gradyear delimited by size into WS-DISPLAY
               perform say

      *        STORY 1: Display optional About Me section if provided
               if function trim(profile-aboutme) not = spaces
                   move " " to WS-DISPLAY
                   perform say
                   move "--- About Me ---" to WS-DISPLAY
                   perform say
                   move function trim(profile-aboutme) to WS-DISPLAY
                   perform say
               end-if

      *        STORY 1: Display all experience entries with proper formatting
               if profile-exp-count > 0
                   move " " to WS-DISPLAY
                   perform say
                   move "--- Professional Experience ---" to WS-DISPLAY
                   perform say
                   perform varying ws-i from 1 by 1 until ws-i > profile-exp-count
                       move " " to WS-DISPLAY
                       perform say
                       move spaces to WS-DISPLAY
                       string "Experience #" ws-i ":" delimited by size into WS-DISPLAY
                       perform say
                       move spaces to WS-DISPLAY
                       string "  Title: " function trim(exp-title(ws-i)) delimited by size into WS-DISPLAY
                       perform say
                       move spaces to WS-DISPLAY
                       string "  Company: " function trim(exp-company(ws-i)) delimited by size into WS-DISPLAY
                       perform say
                       move spaces to WS-DISPLAY
                       string "  Dates: " function trim(exp-dates(ws-i)) delimited by size into WS-DISPLAY
                       perform say
                       if function trim(exp-description(ws-i)) not = spaces
                           move spaces to WS-DISPLAY
                           string "  Description: " function trim(exp-description(ws-i)) delimited by size into WS-DISPLAY
                           perform say
                       end-if
                   end-perform
               else
      *            STORY 1: Show message when no experience entries exist
                   move " " to WS-DISPLAY
                   perform say
                   move "--- Professional Experience ---" to WS-DISPLAY
                   perform say
                   move "  No experience entries added." to WS-DISPLAY
                   perform say
               end-if

      *        STORY 1: Display all education entries with proper formatting
               if profile-edu-count > 0
                   move " " to WS-DISPLAY
                   perform say
                   move "--- Education ---" to WS-DISPLAY
                   perform say
                   perform varying ws-i from 1 by 1 until ws-i > profile-edu-count
                       move " " to WS-DISPLAY
                       perform say
                       move spaces to WS-DISPLAY
                       string "Education #" ws-i ":" delimited by size into WS-DISPLAY
                       perform say
                       move spaces to WS-DISPLAY
                       string "  Degree: " function trim(edu-degree(ws-i)) delimited by size into WS-DISPLAY
                       perform say
                       move spaces to WS-DISPLAY
                       string "  University: " function trim(edu-university(ws-i)) delimited by size into WS-DISPLAY
                       perform say
                       move spaces to WS-DISPLAY
                       string "  Years: " function trim(edu-years(ws-i)) delimited by size into WS-DISPLAY
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

               move " " to WS-DISPLAY
               perform say
               move "================================" to WS-DISPLAY
               perform say
           else
               move "No profile found. Please create your profile first." to WS-DISPLAY
               perform say
           end-if
           .

       load-profile-for-view.
           move "n" to ws-profile-exists
           initialize profile-data
           perform varying ws-i from 1 by 1 until ws-i > 3
               move spaces to exp-title(ws-i)
               move spaces to exp-company(ws-i)
               move spaces to exp-dates(ws-i)
               move spaces to exp-description(ws-i)
               move spaces to edu-degree(ws-i)
               move spaces to edu-university(ws-i)
               move spaces to edu-years(ws-i)
           end-perform
           
           open input profile-file
           if FILESTAT-PROFILE = "00"
               perform until 1 = 2
                   read profile-file into profile-line
                       at end exit perform
                   end-read
                   
                   if function length(function trim(current-user)) > 0
                       if profile-line(1:function length(function trim(current-user))) = 
                          function trim(current-user)
                           and profile-line(function length(function trim(current-user)) + 1:1) = "|"
                           move "y" to ws-profile-exists
                           perform parse-profile-line-complete
                           exit perform
                       end-if
                   end-if
               end-perform
               close profile-file
           end-if
           .

parse-profile-line-complete.
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

           move function trim(PARSE-FIELD(2)) to profile-firstname
           move function trim(PARSE-FIELD(3)) to profile-lastname
           move function trim(PARSE-FIELD(4)) to profile-university
           move function trim(PARSE-FIELD(5)) to profile-major
           if function trim(PARSE-FIELD(6)) not = spaces
               move function numval(function trim(PARSE-FIELD(6))) to profile-gradyear
           else
               move 0 to profile-gradyear
           end-if
           move function trim(PARSE-FIELD(7)) to profile-aboutme

           if function trim(PARSE-FIELD(8)) not = spaces
               move function numval(function trim(PARSE-FIELD(8))) to profile-exp-count
           else
               move 0 to profile-exp-count
           end-if

           if profile-exp-count < 0 move 0 to profile-exp-count end-if
           if profile-exp-count > 3 move 3 to profile-exp-count end-if

      *    STORY 1 Fix: Corrected field indexing for proper data extraction
           move 9 to ws-field-num
           
           if profile-exp-count >= 1
               move function trim(PARSE-FIELD(ws-field-num)) to exp-title(1)
               move function trim(PARSE-FIELD(ws-field-num + 1)) to exp-company(1)
               move function trim(PARSE-FIELD(ws-field-num + 2)) to exp-dates(1)
               move function trim(PARSE-FIELD(ws-field-num + 3)) to exp-description(1)
               add 4 to ws-field-num
           end-if
           
           if profile-exp-count >= 2
               move function trim(PARSE-FIELD(ws-field-num)) to exp-title(2)
               move function trim(PARSE-FIELD(ws-field-num + 1)) to exp-company(2)
               move function trim(PARSE-FIELD(ws-field-num + 2)) to exp-dates(2)
               move function trim(PARSE-FIELD(ws-field-num + 3)) to exp-description(2)
               add 4 to ws-field-num
           end-if
           
           if profile-exp-count >= 3
               move function trim(PARSE-FIELD(ws-field-num)) to exp-title(3)
               move function trim(PARSE-FIELD(ws-field-num + 1)) to exp-company(3)
               move function trim(PARSE-FIELD(ws-field-num + 2)) to exp-dates(3)
               move function trim(PARSE-FIELD(ws-field-num + 3)) to exp-description(3)
               add 4 to ws-field-num
           end-if

           if function trim(PARSE-FIELD(ws-field-num)) not = spaces
               move function numval(function trim(PARSE-FIELD(ws-field-num))) to profile-edu-count
           else
               move 0 to profile-edu-count
           end-if
           add 1 to ws-field-num

           if profile-edu-count < 0 move 0 to profile-edu-count end-if
           if profile-edu-count > 3 move 3 to profile-edu-count end-if

           if profile-edu-count >= 1
               move function trim(PARSE-FIELD(ws-field-num)) to edu-degree(1)
               move function trim(PARSE-FIELD(ws-field-num + 1)) to edu-university(1)
               move function trim(PARSE-FIELD(ws-field-num + 2)) to edu-years(1)
               add 3 to ws-field-num
           end-if
           
           if profile-edu-count >= 2
               move function trim(PARSE-FIELD(ws-field-num)) to edu-degree(2)
               move function trim(PARSE-FIELD(ws-field-num + 1)) to edu-university(2)
               move function trim(PARSE-FIELD(ws-field-num + 2)) to edu-years(2)
               add 3 to ws-field-num
           end-if
           
           if profile-edu-count >= 3
               move function trim(PARSE-FIELD(ws-field-num)) to edu-degree(3)
               move function trim(PARSE-FIELD(ws-field-num + 1)) to edu-university(3)
               move function trim(PARSE-FIELD(ws-field-num + 2)) to edu-years(3)
               add 3 to ws-field-num
           end-if

           move "y" to ws-profile-exists
           .

       say.
      *    STORY 1: All screen output is written to InCollege-Output.txt
      *    This includes profile viewing for easy verification
           display WS-DISPLAY
           move WS-DISPLAY to OutRecord
           write OutRecord
           .
