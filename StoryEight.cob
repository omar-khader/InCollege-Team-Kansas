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

       FD  InpFile.
       01  Inp-Record-Area           PIC X(80).

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

       WORKING-STORAGE SECTION.
       01  WS-DISPLAY              PIC X(80).
       01 WS-USERFILE-STATUS          PIC XX.
       01 WS-CHOICE               PIC 9.
       01 WS-CONTINUE             PIC X VALUE 'Y'.
       01 WS-RECORD-COUNT         PIC 9(3) VALUE 0.

       01 WS-USER-DATA.
           05 WS-USER-USERNAME     PIC X(32).
           05 WS-USER-FIRST-NAME   PIC X(50).
           05 WS-USER-LAST-NAME    PIC X(50).
           05 WS-USER-UNIVERSITY   PIC X(50).
           05 WS-USER-MAJOR        PIC X(30).
           05 WS-GRAD-YEAR         PIC 9(4).
           05 WS-ABOUT-ME          PIC X(200).

       01 WS-DISPLAY-RECORD.
           05 DISP-USER-USERNAME   PIC X(32).
           05 DISP-USER-FIRST-NAME PIC X(50).
           05 DISP-USER-LAST-NAME  PIC X(50).
           05 DISP-USER-UNIVERSITY PIC X(50).
           05 DISP-USER-MAJOR      PIC X(30).
           05 DISP-GRAD-YEAR       PIC 9(4).
           05 DISP-ABOUT-ME        PIC X(200).


       PROCEDURE DIVISION.
       MAIN.
           OPEN INPUT InpFile, OUTPUT OutFile.

           perform sample-profile-creation.
           perform save-current-user-profile.

           close InpFile, OutFile.
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

