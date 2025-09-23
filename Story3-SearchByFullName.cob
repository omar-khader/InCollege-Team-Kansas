identification division.
       program-id. Story3-SearchByFullName.

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

      *    STORY 3: Variables for search functionality
       01  search-firstname          pic x(50).
       01  search-lastname           pic x(50).
       01  search-results-count      pic 9(02) value 0.
      *    Temporary profile data structure for search results
       01  temp-profile-data.
           05  temp-profile-username      pic x(32).
           05  temp-profile-firstname     pic x(50).
           05  temp-profile-lastname      pic x(50).
           05  temp-profile-university    pic x(100).
           05  temp-profile-major         pic x(50).

       01  PARSE-FIELDS.
           05 PARSE-FIELD occurs 50 times pic x(200).
       01  ws-parse-idx              pic 9(02) value 0.

       procedure division.
      ******************************************************************
      * STORY 3: Search for Users by Full Name
      * 
      * As a logged-in user, I want to search for other users by their
      * full name, so I can find and connect with people I know.
      * 
      * Features:
      * - Case-insensitive search by first and last name
      * - Displays matching users with basic information
      * - Shows count of users found
      * - Clear search result formatting
      * - Handles no results gracefully
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

           perform search-for-user

           close InpFile
           close OutFile
           stop run.

       search-for-user.
      *    STORY 3: New feature - Search for users by full name
      *    Performs case-insensitive search across all profiles
      *    Displays matching users with their basic information
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
                   
                   perform parse-search-profile
                   
      *            STORY 3: Case-insensitive comparison for both names
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
           
      *        STORY 3: Handle no results case
           if search-results-count = 0
               move "No users found matching that name." to WS-DISPLAY
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

       parse-search-profile.
      *    STORY 3: Helper procedure to parse profile data for search
      *    Extracts basic fields needed for search results display
           move spaces to PARSE-FIELD(1)
           move spaces to PARSE-FIELD(2)
           move spaces to PARSE-FIELD(3)
           move spaces to PARSE-FIELD(4)
           move spaces to PARSE-FIELD(5)
           move spaces to PARSE-FIELD(6)
           
           unstring profile-line delimited by "|" into
               PARSE-FIELD(1)
               PARSE-FIELD(2)
               PARSE-FIELD(3)
               PARSE-FIELD(4)
               PARSE-FIELD(5)
               PARSE-FIELD(6)
           end-unstring
           
           move function trim(PARSE-FIELD(1)) to temp-profile-username
           move function trim(PARSE-FIELD(2)) to temp-profile-firstname
           move function trim(PARSE-FIELD(3)) to temp-profile-lastname
           move function trim(PARSE-FIELD(4)) to temp-profile-university
           move function trim(PARSE-FIELD(5)) to temp-profile-major
           .
           
       display-search-result.
      *    STORY 3: Display formatted search result for a matching user
      *    Shows username, full name, university, and major
           move "================================" to WS-DISPLAY
           perform say
           move spaces to WS-DISPLAY
           string "Username: " function trim(temp-profile-username) delimited by size into WS-DISPLAY
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
           move "================================" to WS-DISPLAY
           perform say
           .

       say.
      *    STORY 3: All screen output is written to InCollege-Output.txt
      *    This includes search results for easy verification
           display WS-DISPLAY
           move WS-DISPLAY to OutRecord
           write OutRecord
           .
