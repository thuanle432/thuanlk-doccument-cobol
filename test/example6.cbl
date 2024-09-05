       IDENTIFICATION DIVISION.
       PROGRAM-ID. example6.
       DATA DIVISION.
       WORKING-STORAGE SECTION.
       01  WS-OPTION       PIC     X(01).
       PROCEDURE DIVISION.
           PERFORM UNTIL WS-OPTION = "A"
               DISPLAY "INPUT OPTION: " WITH NO ADVANCING 
                                           ACCEPT WS-OPTION
               IF WS-OPTION NOT = "A" THEN
                   DISPLAY "INPUT PLEASE"
               END-IF
           END-PERFORM.
           DISPLAY "NEXT PROGRAM".
           STOP RUN.
       