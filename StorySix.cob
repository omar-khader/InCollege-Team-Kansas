IDENTIFICATION DIVISION.
       PROGRAM-ID. StorySix.

ENVIRONMENT DIVISION.
       INPUT-OUTPUT SECTION.
       FILE-CONTROL.
           SELECT InpFile ASSIGN TO "InCollege-Input.txt"
               ORGANIZATION IS LINE SEQUENTIAL
               FILE STATUS IS FILESTAT.

           SELECT OutFile ASSIGN TO "InCollege-Output.txt"
               ORGANIZATION IS LINE SEQUENTIAL
               FILE STATUS IS FILESTAT.

DATA DIVISION.
       FILE SECTION.
       FD  InpFile.
       01  InpRecord           PIC X(80).

       FD  OutFile.
       01  OutRecord           PIC X(80).
       
       WORKING-STORAGE SECTION.
       01  FILESTAT               PIC X(2).
       01  WS-EOF           PIC X VALUE 'N'.
       01  WS-USER-CHOICE         PIC X(1).

PROCEDURE DIVISION.
       MAIN-PROCEDURE.
           OPEN INPUT InpFile
           OPEN OUTPUT OutFile
           PERFORM POST-LOGIN-NAVIGATION
           CLOSE InpFile
           CLOSE OutFile
           STOP RUN.

       POST-LOGIN-NAVIGATION.
           PERFORM UNTIL WS-USER-CHOICE = '4' OR WS-EOF = 'Y'
               MOVE "Search for a job" TO OutRecord
               DISPLAY OutRecord
               WRITE OutRecord

               MOVE "Find someone you know" TO OutRecord
               DISPLAY OutRecord
               WRITE OutRecord

               MOVE "Learn a new skill" TO OutRecord
               DISPLAY OutRecord
               WRITE OutRecord

               MOVE "Enter your choice:" TO OutRecord
               DISPLAY OutRecord
               WRITE OutRecord

               READ InpFile
                   AT END MOVE 'Y' TO WS-EOF
                   NOT AT END MOVE InpRecord TO WS-USER-CHOICE
               END-READ

               IF WS-EOF = 'N'
                   EVALUATE WS-USER-CHOICE
                       WHEN '1'
                           MOVE "Job search/internship is under construction." TO OutRecord
                           DISPLAY OutRecord
                           WRITE OutRecord
                       WHEN '2'
                           MOVE "Find someone you know is under construction." TO OutRecord
                           DISPLAY OutRecord
                           WRITE OutRecord
                       WHEN '3'
                           MOVE "Skill menu would go here." TO OutRecord
                           DISPLAY OutRecord
                           WRITE OutRecord
                   END-EVALUATE
               END-IF
           END-PERFORM.


