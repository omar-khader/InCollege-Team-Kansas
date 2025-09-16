IDENTIFICATION DIVISION.
       PROGRAM-ID. PROFILE-SAVING-TEST.
       AUTHOR. InCollege-Team-Kansas.

       ENVIRONMENT DIVISION.
       INPUT-OUTPUT SECTION.
       FILE-CONTROL.

           *> Define the output and data files for the test
           SELECT OutFile ASSIGN TO 'StoryEight-Output.txt'
               ORGANIZATION IS LINE SEQUENTIAL
               ACCESS MODE IS SEQUENTIAL.

           SELECT USER-FILE ASSIGN TO "USERPROF.DAT"
           ORGANIZATION IS SEQUENTIAL
           ACCESS MODE IS SEQUENTIAL
           FILE STATUS IS WS-USERFILE-STATUS.

       DATA DIVISION.
       FILE SECTION.

       FD  OutFile.
       01  Out-Record-Area           PIC X(80).

       FD USER-FILE.
       01 USER-RECORD.
           05 USER-USERNAME        PIC X(32).
           05 USER-FIRST-NAME      PIC X(50).
           05 USER-LAST-NAME       PIC X(50).
           05 USER-UNIVERSITY      PIC X(50).
           05 USER-MAJOR           PIC X(30).
           05 USER-GRAD-YEAR       PIC 9(4).
           05 USER-ABOUT-ME        PIC X(200).

           05  prof-exp-count      PIC 9.
           05  prof-experience     occurs 3 times.
               10  exp-title       pic x(50).
               10  exp-company     pic x(50).
               10  exp-dates       pic x(30).
               10  exp-description pic x(100).

           05  prof-edu-count      PIC 9.
           05  prof-education      occurs 3 times.
               10  edu-degree      pic x(50).
               10  edu-university  pic x(100).
               10  edu-years       pic x(30).

       WORKING-STORAGE SECTION.
       01  WS-DISPLAY              PIC X(80).
       01 WS-USERFILE-STATUS          PIC XX.



       PROCEDURE DIVISION.
       MAIN.
           OPEN OUTPUT OutFile.

           perform sample-profile-creation.
           perform save-current-user-profile.

           close OutFile.
           STOP RUN.

       say.
           *> This paragraph handles displaying output to the screen and the file
           DISPLAY WS-DISPLAY.
           WRITE Out-Record-Area FROM WS-DISPLAY.

       sample-profile-creation.
           *> This paragraph handles creating a sample profile for testing
           move "Beginning creation of sample profile." to WS-DISPLAY
           perform say

           initialize USER-RECORD

           move "Username: i.hate.cobol" to WS-DISPLAY
           perform say
           move "i.hate.cobol" to USER-USERNAME

           move "User First Name: John" to WS-DISPLAY
           perform say
           move "John" to USER-FIRST-NAME

           move "User Last Name: Bovi" to WS-DISPLAY
           perform say
           move "Bovi" to USER-LAST-NAME

           move "University: University of South Florida" to WS-DISPLAY
           perform say
           move "University of South Florida" to USER-UNIVERSITY

           move "Major: Being bad at programming" to WS-DISPLAY
           perform say
           move "Being bad at programming" to USER-MAJOR

           move "Grad Year: 2040" to WS-DISPLAY
           perform say
           move 2040 to USER-GRAD-YEAR

           move "About me: I'm really bad at programming. :)" to WS-DISPLAY
           perform say
           move "I'm really bd at programming. :)" to USER-ABOUT-ME

           move "Setting prof-exp-count to 1." to WS-DISPLAY
           perform say
           move 1 to prof-exp-count

           move "Defining professional experience (1)." to WS-DISPLAY
           perform say

           move "Setting exp-title to: Programmer" to WS-DISPLAY
           perform say
           move "Programmer" to exp-title(1)

           move "Setting exp-company to: Intel" to WS-DISPLAY
           perform say
           move "Intel" to exp-company(1)

           move "Setting exp-dates to: August 1st to Now" to WS-DISPLAY
           perform say
           move "August 1st to Now" to exp-dates(1)

           move "Setting exp-description to: A job" to WS-DISPLAY
           perform say
           move "A job" to exp-description(1)

           move "Setting prof-edu-count to 1" to WS-DISPLAY
           perform say
           move 1 to prof-edu-count

           move "Defining prof-edu-count (1)" to WS-DISPLAY
           perform say

           move "Setting edu-degree to: Bachelors of Science in Computer Science"
           to WS-DISPLAY
           perform say
           move "Bachelors of Science in Computer Science" to edu-degree(1)

           move "Setting edu-university to: University of South Florida" to WS-DISPLAY
           perform say
           move "University of South FLorida" to edu-university(1)

           move "Setting edu-years to: 2020 to 2024" to WS-DISPLAY
           perform say
           move "2020 to 2024" to edu-years(1)
           .

       save-current-user-profile.
           *> This paragraph handles saving user data into userprof.dat

           *> First try to open in EXTEND mode (file exists)
           open extend USER-FILE

           evaluate WS-USERFILE-STATUS
               when "00"
                   *> File exists and opened successfully for append
                   write USER-RECORD
                   if WS-USERFILE-STATUS = "00"
                       move "User Profile Successfully Saved (Appended)" to WS-DISPLAY
                       perform say
                   else
                       string "Error writing to file: " WS-USERFILE-STATUS
                       delimited by size into WS-DISPLAY
                       perform say
                   end-if
                   close USER-FILE

               when "35"
                   *> File doesn't exist, so create it with OUTPUT mode
                   close USER-FILE  *> Close the failed EXTEND attempt
                   open output USER-FILE
                   if WS-USERFILE-STATUS = "00"
                       write USER-RECORD
                       if WS-USERFILE-STATUS = "00"
                           move "User Profile Successfully Saved (New File Created)" to WS-DISPLAY
                           perform say
                       else
                           string "Error writing to new file: " WS-USERFILE-STATUS
                           delimited by size into WS-DISPLAY
                           perform say
                       end-if
                       close USER-FILE
                   else
                       string "Error creating file: " WS-USERFILE-STATUS
                       delimited by size into WS-DISPLAY
                       perform say
                   end-if

               when other
                   *> Some other error occurred
                   string "Error opening file: " WS-USERFILE-STATUS
                   delimited by size into WS-DISPLAY
                   perform say
                   close USER-FILE
           end-evaluate.

