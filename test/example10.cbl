       IDENTIFICATION DIVISION.
       PROGRAM-ID. LeapYearCheck.
       DATA DIVISION.
       WORKING-STORAGE SECTION.
       01 YEAR PIC 9(4) VALUE 400.
       01 DIV-4 PIC 9(4).
       01 MULT-4 PIC 9(4).
       01 DIFF-4 PIC 9(4).
       01 DIV-100 PIC 9(4).
       01 MULT-100 PIC 9(4).
       01 DIFF-100 PIC 9(4).
       01 DIV-400 PIC 9(4).
       01 MULT-400 PIC 9(4).
       01 DIFF-400 PIC 9(4).

       PROCEDURE DIVISION.
           COMPUTE DIV-4 = YEAR / 4
           COMPUTE MULT-4 = DIV-4 * 4
           COMPUTE DIFF-4 = YEAR - MULT-4
           DISPLAY "DIV-4: " DIV-4.
           DISPLAY "MULT-4: " MULT-4.
           DISPLAY "DIFF-4: " DIFF-4.

           COMPUTE DIV-100 = YEAR / 100
           COMPUTE MULT-100 = DIV-100 * 100
           COMPUTE DIFF-100 = YEAR - MULT-100
           DISPLAY "DIV-100: " DIV-100.
           DISPLAY "MULT-100: " MULT-100.
           DISPLAY "DIFF-100: " DIFF-100.

           COMPUTE DIV-400 = YEAR / 400
           COMPUTE MULT-400 = DIV-400 * 400
           COMPUTE DIFF-400 = YEAR - MULT-400
           DISPLAY "DIV-400: " DIV-400.
           DISPLAY "MULT-400: " MULT-400.
           DISPLAY "DIFF-400: " DIFF-400.

           EVALUATE TRUE
               WHEN DIFF-4 = 0 AND DIFF-100 > 0
                   DISPLAY YEAR " LEAP YEAR."
               WHEN DIFF-400 = 0
                   DISPLAY YEAR " LEAP YEAR."
               WHEN OTHER
                   DISPLAY YEAR " NOT LEAP YEAR."
           END-EVALUATE
           STOP RUN.
