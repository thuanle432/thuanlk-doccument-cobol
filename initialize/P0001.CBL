       IDENTIFICATION DIVISION.
       PROGRAM-ID. P0001.
      ******************************************************************
      *                                            AUTHOR              *
      *                                            LE KIM THUAN        *
      ******************************************************************
       DATA DIVISION.
       WORKING-STORAGE SECTION.
       01  WS-EMPLOYEE.
           03  WS-NAME                 PIC X(25).
           03  WS-AGE                  PIC 9(03).
           03  WS-SALARY               PIC 9(07)V99.
       PROCEDURE DIVISION.
           INITIALIZE WS-EMPLOYEE  REPLACE
                                   ALPHABETIC BY SPACES 
                                   NUMERIC BY ZEROS.
           DISPLAY WS-EMPLOYEE.
           STOP RUN.
