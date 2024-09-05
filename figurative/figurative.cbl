       IDENTIFICATION DIVISION.
       PROGRAM-ID. figurative.
       DATA DIVISION.
       WORKING-STORAGE SECTION.   
       01  Number1         PIC          S9(5)V99.
       PROCEDURE DIVISION.
           DISPLAY "Number: " ACCEPT Number1.
           DISPLAY "Result number 1: " Number1.
           STOP RUN.
