       IDENTIFICATION DIVISION.
       PROGRAM-ID. erithmetic.
       DATA DIVISION.
       WORKING-STORAGE SECTION.
       01  Num1    PIC 9(04).
       01  Num2    PIC 9(04).
       01  Result  PIC 9(04) VALUE ZEROS.
       01  SoDu    PIC 9(04) VALUE ZEROS.

       
       PROCEDURE DIVISION.
           DISPLAY "Num1: " ACCEPT Num1.
           DISPLAY "Num2: " ACCEPT Num2.
      *    ADD Num1 TO Num2 GIVING Result.
      *    COMPUTE Result = Num1 + Num2.
      *    SUBTRACT Num1 FROM Num2 GIVING Result.
      *    COMPUTE Result = Num1 - Num2.
      *    MULTIPLY Num1 BY Num2 GIVING Result.
      *    COMPUTE Result = Num1 * Num2.
      *    DIVIDE Num1 BY Num2 GIVING Result REMAINDER SoDu.
           COMPUTE Result = Num1 / Num2.
           COMPUTE SoDu = Num1 - (Num2 * Result).
           DISPLAY "Result: " Result.
           DISPLAY "So du: " SoDu.
           STOP RUN.

       