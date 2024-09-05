       IDENTIFICATION DIVISION.
       PROGRAM-ID. example7.
       DATA DIVISION.
       WORKING-STORAGE SECTION.
       01  WS-TABLE.
           03  WS-LENGTH-TABLE OCCURS 10 TIMES INDEXED BY IND.
               07  WS-NUMBER           PIC X(12).
               07  WS-NAME             PIC X(12).
       01  WS-LENGTH           PIC 9(02) VALUE 0.
       01  I                   PIC 9(02).
       PROCEDURE DIVISION.
           DISPLAY "WS-NUMER: " WITH NO ADVANCING ACCEPT WS-NUMBER(IND).
           PERFORM VARYING I FROM 1 BY 1 UNTIL I > LENGTH 
           OF WS-NUMBER
               IF WS-NUMBER(IND)(I:1) NOT = SPACE
                   ADD 1 TO WS-LENGTH
               END-IF
           END-PERFORM.
           DISPLAY "WS-NAME: " ACCEPT WS-NAME.
           STOP RUN.
       