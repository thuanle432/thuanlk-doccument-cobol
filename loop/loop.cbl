       IDENTIFICATION DIVISION.
       PROGRAM-ID. loop.
       DATA DIVISION.
       WORKING-STORAGE SECTION.
      *01  Couter      PIC     9(01) VALUE 5. 
       01  OPTION      PIC     9(01).
       PROCEDURE DIVISION.
      *    PERFORM TEST-B VARYING Couter FROM 1 BY 1 UNTIL Couter > 5
       MAIN SECTION.
       MAIN-START.
           DISPLAY "INPUT OPTION: " WITH NO ADVANCING ACCEPT OPTION.
           PERFORM UNTIL OPTION = 0
               EVALUATE OPTION
                   WHEN 1
                       PERFORM HELLO
                       GO TO MAIN
                   WHEN 2
                       PERFORM FULLNAME
                       GO TO MAIN
                   WHEN 0
                       GO TO MAIN
                   WHEN OTHER 
                       DISPLAY "INPUT OPTION, PLEASE! "
                       GO TO MAIN
               END-EVALUATE
           END-PERFORM.
           
       MAIN-EXIT.
           EXIT.
           DISPLAY "END PROGRAMME".
           STOP RUN.
      *TEST-B.
      *    DISPLAY "So: " Couter.

       HELLO SECTION.
           DISPLAY "HELLO".
           EXIT.
       FULLNAME SECTION.
           DISPLAY "LE KIM THUAN".
           EXIT.
