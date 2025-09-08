IDENTIFICATION DIVISION.
       PROGRAM-ID. InCollegeOutput.

ENVIRONMENT DIVISION.
       INPUT-OUTPUT SECTION.
       FILE-CONTROL.
           SELECT InpFile ASSIGN TO 'InCollege-Input.txt'
               ORGANIZATION IS LINE SEQUENTIAL
               FILE STATUS IS FILESTAT.

           SELECT OutFile ASSIGN TO 'InCollege-Output.txt'
               ORGANIZATION IS LINE SEQUENTIAL
               FILE STATUS IS FILESTAT.

DATA DIVISION.
FILE SECTION.
       FD  InpFile.
       01  InpRecord                      PIC X(80).

       FD  OutFile.
       01  OutRecord                      PIC X(80).

WORKING-STORAGE SECTION.
       01  FILESTAT                       PIC X(2).
       01  WS-EOF                         PIC X VALUE 'N'.
       01  WS-DISPLAY                     PIC X(80).

PROCEDURE DIVISION.
       MAIN.
           OPEN INPUT InpFile
           OPEN OUTPUT OutFile.
           PERFORM UNTIL WS-EOF = 'Y'
               READ InpFile
                   AT END MOVE 'Y' TO WS-EOF
                   NOT AT END
                       MOVE InpRecord TO WS-DISPLAY
                       DISPLAY WS-DISPLAY
                       MOVE WS-DISPLAY TO OutRecord
                       WRITE OutRecord
               END-READ
           END-PERFORM.
           CLOSE InpFile
                 OutFile.
       STOP RUN.

