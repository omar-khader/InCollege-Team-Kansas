       IDENTIFICATION DIVISION.
       PROGRAM-ID. SkillMenu.

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
       01  FILESTAT            PIC X(2).
       01  WS-EOF              PIC X VALUE 'N'.
       01  WS-USER-CHOICE      PIC X(1).
       01  WS-DISPLAY          PIC X(80).

       PROCEDURE DIVISION.
       MAIN-PROCEDURE.
           OPEN INPUT InpFile
           OPEN OUTPUT OutFile
           PERFORM SKILL-MENU
           CLOSE InpFile
           CLOSE OutFile
           STOP RUN.

       SKILL-MENU.
           PERFORM UNTIL WS-USER-CHOICE = '6' OR WS-EOF = 'Y'
               MOVE "Learn a New Skill:" TO WS-DISPLAY
               DISPLAY FUNCTION TRIM(WS-DISPLAY)
               MOVE WS-DISPLAY TO OutRecord
               WRITE OutRecord

               MOVE "1. Learn COBOL!" TO WS-DISPLAY
               DISPLAY FUNCTION TRIM(WS-DISPLAY)
               MOVE WS-DISPLAY TO OutRecord
               WRITE OutRecord

               MOVE "2. Learn Jira!" TO WS-DISPLAY
               DISPLAY FUNCTION TRIM(WS-DISPLAY)
               MOVE WS-DISPLAY TO OutRecord
               WRITE OutRecord

               MOVE "3. Learn Git!" TO WS-DISPLAY
               DISPLAY FUNCTION TRIM(WS-DISPLAY)
               MOVE WS-DISPLAY TO OutRecord
               WRITE OutRecord

               MOVE "4. Learn Github!" TO WS-DISPLAY
               DISPLAY FUNCTION TRIM(WS-DISPLAY)
               MOVE WS-DISPLAY TO OutRecord
               WRITE OutRecord

               MOVE "5. Learn Software Engineering!" TO WS-DISPLAY
               DISPLAY FUNCTION TRIM(WS-DISPLAY)
               MOVE WS-DISPLAY TO OutRecord
               WRITE OutRecord

               MOVE "6. Return" TO WS-DISPLAY
               DISPLAY FUNCTION TRIM(WS-DISPLAY)
               MOVE WS-DISPLAY TO OutRecord
               WRITE OutRecord

               MOVE "Enter your choice:" TO WS-DISPLAY
               DISPLAY FUNCTION TRIM(WS-DISPLAY)
               MOVE WS-DISPLAY TO OutRecord
               WRITE OutRecord

               READ InpFile
                   AT END MOVE 'Y' TO WS-EOF
                   NOT AT END MOVE InpRecord TO WS-USER-CHOICE
               END-READ

               IF WS-EOF = 'N'
                   IF WS-USER-CHOICE >= '1' AND WS-USER-CHOICE <= '5'
                       MOVE "under construction" TO WS-DISPLAY
                       DISPLAY FUNCTION TRIM(WS-DISPLAY)  *> function removes the trailing whitespaces
                       MOVE WS-DISPLAY TO OutRecord
                       WRITE OutRecord
                   END-IF
               END-IF
           END-PERFORM.

 

