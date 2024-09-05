       IDENTIFICATION DIVISION.
       PROGRAM-ID. main-program.
       DATA DIVISION.
       WORKING-STORAGE SECTION.
       01  WS-STAFF.
           03  WS-ID                   PIC     9(02).
           03  WS-NAME                 PIC     X(14).
           03  WS-SALARY               PIC     9(7)v9(02).
           03  WS-BONUS-SALARY         PIC     9(7)V9(02).
       PROCEDURE DIVISION.
           MOVE 01                             TO WS-ID.
           MOVE "THUAN"                        TO WS-NAME.
           MOVE 4000.00                        TO WS-SALARY.
           DISPLAY "SALARY IS: " WS-SALARY.
           CALL "bonus"                        USING WS-STAFF.
           DISPLAY "SALARY AFFTER BONUS: " WS-SALARY.
           DISPLAY "BONUS: " WS-BONUS-SALARY.
           STOP RUN.
       