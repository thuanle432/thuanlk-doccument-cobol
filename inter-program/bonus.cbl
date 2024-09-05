       IDENTIFICATION DIVISION.
       PROGRAM-ID. bonus.
       DATA DIVISION.
       LINKAGE SECTION.
       01  WS-STAFF.
           03  WS-ID                   PIC     9(02).
           03  WS-NAME                 PIC     X(14).
           03  WS-SALARY               PIC     9(7)v9(02).
           03  WS-BONUS-SALARY         PIC     9(7)V9(02).
       PROCEDURE DIVISION USING WS-STAFF.
           COMPUTE  WS-BONUS-SALARY =          WS-SALARY * 0.1.
           ADD WS-BONUS-SALARY                 TO WS-SALARY.
           EXIT PROGRAM.
       