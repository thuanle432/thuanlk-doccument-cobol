       IDENTIFICATION DIVISION.
       PROGRAM-ID. verbs.

       DATA DIVISION.
       WORKING-STORAGE SECTION.
       01  Choice      PIC     X(01).

       PROCEDURE DIVISION.
       MAIN-START.
           PERFORM CHOOSE.
           IF Choice = "Y" THEN
               GO TO MESSANGER
           END-IF.
           DISPLAY "ERROR!!!!!!!".
       MAIN-EXIT.       
           STOP RUN.
       CHOOSE.
           DISPLAY "Input choice: " ACCEPT Choice.
       MESSANGER.
           DISPLAY "OK!!!!!!".
       
