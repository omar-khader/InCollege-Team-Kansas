IDENTIFICATION DIVISION.
       PROGRAM-ID. InCollegeEpic1HW1.
       AUTHOR. Vivek.
       DATE-WRITTEN. 2025-09-06.

ENVIRONMENT DIVISION.
       INPUT-OUTPUT SECTION.
       FILE-CONTROL.
           SELECT InpFile ASSIGN TO "InCollege-Input.txt"
           ORGANIZATION IS LINE SEQUENTIAL  *> reads line-by-line
           FILE STATUS IS FILESTAT.         *> should give 00

DATA DIVISION.
FILE SECTION.
       FD  InpFile.
       01  InpRecord               PIC X(80).

WORKING-STORAGE SECTION.
       01  FILESTAT                PIC X(2).
       01  WS-EOF                  PIC X VALUE 'N'.  *> default file not reached end (NO)
       01  WS-DISPLAY              PIC X(80).

PROCEDURE DIVISION.
       MAIN.
           OPEN INPUT InpFile.
           PERFORM UNTIL WS-EOF = 'Y'
            READ InpFile
              AT END MOVE 'Y' TO WS-EOF
              NOT AT END
                MOVE InpRecord TO WS-DISPLAY
                DISPLAY WS-DISPLAY
            END-READ
           END-PERFORM.
           CLOSE InpFile.
       STOP RUN.

