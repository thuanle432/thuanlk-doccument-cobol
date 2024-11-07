       IDENTIFICATION DIVISION.
       PROGRAM-ID. P0001.
      ******************************************************************
      *                                            AUTHOR              *
      *                                            LE KIM THUAN        *
      ******************************************************************
       DATA DIVISION.
       WORKING-STORAGE SECTION.
       01  WS-OPTION                       PIC X(01).
       01  WS-EMPLOYEE.
           05  WS-ID                       PIC 9(02).
           05  WS-NAME                     PIC X(20).
           05  WS-SALARY                   PIC 9(04)V99.
       01  GEETING                         PIC X(30).


       SCREEN SECTION.
       01  MENU-SCREEN.
           05  LINE NUMBER 1 COLUMN 02 VALUE 'PLEASE OPTION'.
           05  LINE NUMBER 2 COLUMN 02 VALUE 'A: CREATE NAME'.
           05  LINE NUMBER 3 COLUMN 02 VALUE 'B: VIEW NAME'.
           05  LINE NUMBER 4 COLUMN 02 VALUE 'C: MODIIFY NAME'.
           05  LINE NUMBER 5 COLUMN 02 VALUE 'C: DELETE NAME'.
           05  LINE NUMBER 6 COLUMN 02 VALUE 'ACTION: '.
           05  LINE NUMBER 6 COLUMN 10 PIC X(01) TO WS-OPTION.

       01  CREATE-SCREEN.
           05  BLANK SCREEN.
           05  LINE NUMBER 1 COLUMN 35 
                                   VALUE 'PLEASE ENTER YOUR EMPLOYEE'.
           05  LINE NUMBER 2 COLUMN 35 VALUE 'ID:  '.
           05  LINE NUMBER 2 COLUMN 45 PIC 9(02) TO WS-ID.
           05  LINE NUMBER 3 COLUMN 35 VALUE 'NAME:  '.
           05  LINE NUMBER 3 COLUMN 45 PIC X(20) TO WS-NAME.
           05  LINE NUMBER 4 COLUMN 35 VALUE 'SALARY:  '.
           05  LINE NUMBER 4 COLUMN 45 PIC 9(04)V99 TO WS-SALARY.

       01  VIEW-SCREEN.
           05  BLANK SCREEN.
           05  LINE NUMBER 1 COLUMN 35 
                                   VALUE 'INFORMATION EMPLOYEE'.
           05  LINE NUMBER 2 COLUMN 35 VALUE 'ID:  '.
           05  LINE NUMBER 2 COLUMN 45 PIC 9(02) FROM WS-ID.
           05  LINE NUMBER 3 COLUMN 35 VALUE 'NAME:  '.
           05  LINE NUMBER 3 COLUMN 45 PIC X(20) FROM WS-NAME.
           05  LINE NUMBER 4 COLUMN 35 VALUE 'SALARY:  '.
           05  LINE NUMBER 4 COLUMN 45 PIC 9(04)V99 FROM WS-SALARY.
       01  MESSAGE-SCREEN.
           05  BLANK SCREEN.
           05  LINE NUMBER 20 COLUMN 15 VALUE 'PLEASE INPUT OPTION'.

       PROCEDURE DIVISION.

       MAIN SECTION.
       MAIN-START.
           PERFORM VIEW-MAIN.
       MAIN-EXIT.
           STOP RUN.
      /
       VIEW-MAIN SECTION.
       VIEW-MAIN-START.
           DISPLAY MENU-SCREEN.
           DISPLAY VIEW-SCREEN.
           PERFORM UNTIL WS-OPTION         = 'E'
               ACCEPT MENU-SCREEN
               EVALUATE WS-OPTION
                   WHEN 'A'
                       PERFORM DISPLAY-CREATE
                   WHEN 'B'
                       PERFORM DISPLAY-VIEW     
                   WHEN OTHER
                       PERFORM DISPLAY-MESSAGE   
               END-EVALUATE
           END-PERFORM.
           ACCEPT VIEW-SCREEN.
       VIEW-MAIN-EXIT.
           EXIT.
      /
      /
       DISPLAY-CREATE SECTION.
       DISPLAY-CREATE-START.
           DISPLAY CREATE-SCREEN.
           ACCEPT CREATE-SCREEN.
       DISPLAY-CREATE-EXIT.
           EXIT.
      /
       DISPLAY-VIEW SECTION.
       DISPLAY-VIEW-START.
           DISPLAY VIEW-SCREEN.
           ACCEPT VIEW-SCREEN.
       DISPLAY-VIEW-EXIT.
           EXIT.
      /
       DISPLAY-MESSAGE SECTION.
       DISPLAY-MESSAGE-START.
           DISPLAY MESSAGE-SCREEN.
           ACCEPT MESSAGE-SCREEN.
       DISPLAY-MESSAGE-EXIT.
           EXIT.

