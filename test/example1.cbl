       IDENTIFICATION DIVISION.
       PROGRAM-ID. example1.

       DATA DIVISION.
       WORKING-STORAGE SECTION.
       01  WS-Id       PIC 9(5).
       01  WS-Name     PIC X(20).

       PROCEDURE DIVISION.
           DISPLAY "Input name: " ACCEPT WS-Name.
           MOVE 00001 TO WS-Id.
           DISPLAY "Output Id: " WS-Id.
           DISPLAY "Output Name: " WS-Name.
           STOP RUN.
      
       