IDENTIFICATION DIVISION.
       PROGRAM-ID. FILE-INPUT-TEST.
       AUTHOR. InCollege Team.

       ENVIRONMENT DIVISION.
       INPUT-OUTPUT SECTION.
       FILE-CONTROL.
           SELECT InpFile ASSIGN TO 'InCollege-Input.txt'
               ORGANIZATION IS LINE SEQUENTIAL
               FILE STATUS IS FILESTAT.
           SELECT OutFile ASSIGN TO 'InCollege-Output.txt'
               ORGANIZATION IS LINE SEQUENTIAL
               FILE STATUS IS FILESTAT-Out.

       DATA DIVISION.
       FILE SECTION.
       FD  InpFile.
       01  Inp-Record-Area           PIC X(200).

       FD  OutFile.
       01  Out-Record-Area           PIC X(200).

       WORKING-STORAGE SECTION.
       01  WS-DISPLAY              PIC X(200).
       01  WS-EOF                  PIC X(1) VALUE 'N'.
           88  END-OF-FILE                VALUE 'Y'.
       01  FILESTAT-Out            PIC X(2).
       01  FILESTAT                PIC X(2).

       01  file-input-data.
           05  input-file-name     PIC X(50) VALUE "InCollege-Input.txt".
           05  output-file-name    PIC X(50) VALUE "InCollege-Output.txt".
           05  current-input-line  PIC X(200).
           05  input-line-number   PIC 9(4) VALUE 0.
           05  end-of-input        PIC X VALUE "N".

       01  input-validation.
           05  expected-input-type PIC X(20).
           05  actual-input-value  PIC X(200).
           05  is-valid-input      PIC X VALUE "Y".

       01  profile-data.
           05  prof-first-name     PIC X(50).
           05  prof-last-name      PIC X(50).
           05  prof-university     PIC X(100).
           05  prof-major          PIC X(50).
           05  prof-grad-year      PIC 9(4).
           05  prof-about-me       PIC X(200).

       PROCEDURE DIVISION.
       MAIN-LOGIC-SECTION.
           PERFORM setup-file-input-system.

           IF FILESTAT = "00" AND FILESTAT-Out = "00"
               PERFORM read-profile-input-with-validation
               PERFORM display-profile-data
           END-IF.
           
           PERFORM close-file-input-system.
           
           STOP RUN.

       setup-file-input-system.
           MOVE 0 TO input-line-number
           MOVE "N" TO end-of-input

           OPEN INPUT InpFile
           DISPLAY "Input file status after open: " FILESTAT
           IF FILESTAT NOT = "00"
               MOVE SPACES TO WS-DISPLAY
               STRING "ERROR: Cannot open input file " input-file-name
                      ", file status: " FILESTAT DELIMITED BY SIZE INTO WS-DISPLAY
               DISPLAY WS-DISPLAY
               STOP RUN
           END-IF

           OPEN OUTPUT OutFile
           DISPLAY "Output file status after open: " FILESTAT-Out
           IF FILESTAT-Out NOT = "00"
               MOVE SPACES TO WS-DISPLAY
               STRING "ERROR: Cannot open output file " output-file-name
                      ", file status: " FILESTAT-Out DELIMITED BY SIZE INTO WS-DISPLAY
               DISPLAY WS-DISPLAY
               CLOSE InpFile
               STOP RUN
           END-IF

           MOVE "File input system initialized successfully." TO WS-DISPLAY
           PERFORM say.

       read-next-input-line.
           READ InpFile INTO current-input-line
               AT END
                   MOVE "Y" TO end-of-input
                   MOVE "Y" TO WS-EOF
               NOT AT END
                   ADD 1 TO input-line-number
                   MOVE FUNCTION TRIM(current-input-line) TO actual-input-value
           END-READ

           IF end-of-input = "N"
               MOVE SPACES TO WS-DISPLAY
               STRING "Reading input line " input-line-number ": "
                   actual-input-value DELIMITED BY SIZE INTO WS-DISPLAY
               PERFORM say
           END-IF.

       validate-file-input.
           MOVE "Y" TO is-valid-input

           IF expected-input-type NOT = "OPTIONAL" AND
              FUNCTION LENGTH(FUNCTION TRIM(actual-input-value)) = 0
               MOVE "N" TO is-valid-input
               MOVE SPACES TO WS-DISPLAY
               STRING "ERROR: Expected " expected-input-type
                      " but got empty input at line " input-line-number
                      DELIMITED BY SIZE INTO WS-DISPLAY
               PERFORM say
           END-IF

           EVALUATE expected-input-type
               WHEN "YEAR"
                   IF FUNCTION TEST-NUMVAL(actual-input-value) NOT = 0
                       MOVE "N" TO is-valid-input
                       MOVE "ERROR: Expected numeric year but got non-numeric input" TO WS-DISPLAY
                       PERFORM say
                   END-IF
               WHEN "MENU-CHOICE"
                   IF FUNCTION TEST-NUMVAL(actual-input-value) NOT = 0
                       MOVE "N" TO is-valid-input
                       MOVE "ERROR: Expected numeric menu choice but got non-numeric input" TO WS-DISPLAY
                       PERFORM say
                   END-IF
               WHEN "DONE-OR-TEXT"
                   CONTINUE
               WHEN "TEXT"
                   CONTINUE
               WHEN "OPTIONAL"
                   CONTINUE
           END-EVALUATE.

       read-profile-input-with-validation.
           MOVE "Reading profile data with validation:" TO WS-DISPLAY
           PERFORM say

           MOVE "TEXT" TO expected-input-type
           PERFORM read-next-input-line
           PERFORM validate-file-input
           IF is-valid-input = "Y"
               MOVE actual-input-value TO prof-first-name
           END-IF

           MOVE "TEXT" TO expected-input-type
           PERFORM read-next-input-line
           PERFORM validate-file-input
           IF is-valid-input = "Y"
               MOVE actual-input-value TO prof-last-name
           END-IF

           MOVE "TEXT" TO expected-input-type
           PERFORM read-next-input-line
           PERFORM validate-file-input
           IF is-valid-input = "Y"
               MOVE actual-input-value TO prof-university
           END-IF

           MOVE "TEXT" TO expected-input-type
           PERFORM read-next-input-line
           PERFORM validate-file-input
           IF is-valid-input = "Y"
               MOVE actual-input-value TO prof-major
           END-IF

           MOVE "YEAR" TO expected-input-type
           PERFORM read-next-input-line
           PERFORM validate-file-input
           IF is-valid-input = "Y"
               IF FUNCTION TEST-NUMVAL(actual-input-value) = 0
                   MOVE FUNCTION NUMVAL(actual-input-value) TO prof-grad-year
               END-IF
           END-IF

           MOVE "OPTIONAL" TO expected-input-type
           PERFORM read-next-input-line
           PERFORM validate-file-input
           IF FUNCTION LENGTH(FUNCTION TRIM(actual-input-value)) > 0
               MOVE actual-input-value TO prof-about-me
           END-IF.

       display-profile-data.
           MOVE "=== COLLECTED PROFILE DATA ===" TO WS-DISPLAY
           PERFORM say
           
           MOVE SPACES TO WS-DISPLAY
           STRING "First Name: " FUNCTION TRIM(prof-first-name) 
               DELIMITED BY SIZE INTO WS-DISPLAY
           PERFORM say
           
           MOVE SPACES TO WS-DISPLAY
           STRING "Last Name: " FUNCTION TRIM(prof-last-name)
               DELIMITED BY SIZE INTO WS-DISPLAY
           PERFORM say
           
           MOVE SPACES TO WS-DISPLAY
           STRING "University: " FUNCTION TRIM(prof-university)
               DELIMITED BY SIZE INTO WS-DISPLAY
           PERFORM say
           
           MOVE SPACES TO WS-DISPLAY
           STRING "Major: " FUNCTION TRIM(prof-major)
               DELIMITED BY SIZE INTO WS-DISPLAY
           PERFORM say
           
           MOVE SPACES TO WS-DISPLAY
           STRING "Graduation Year: " prof-grad-year
               DELIMITED BY SIZE INTO WS-DISPLAY
           PERFORM say
           
           IF FUNCTION LENGTH(FUNCTION TRIM(prof-about-me)) > 0
               MOVE SPACES TO WS-DISPLAY
               STRING "About Me: " FUNCTION TRIM(prof-about-me)
                   DELIMITED BY SIZE INTO WS-DISPLAY
               PERFORM say
           END-IF.

       close-file-input-system.
           CLOSE InpFile
           CLOSE OutFile
           
           MOVE SPACES TO WS-DISPLAY
           STRING "Processed " input-line-number " input lines total."
                  DELIMITED BY SIZE INTO WS-DISPLAY
           DISPLAY WS-DISPLAY.

       say.
           DISPLAY WS-DISPLAY.
           WRITE Out-Record-Area FROM WS-DISPLAY.
