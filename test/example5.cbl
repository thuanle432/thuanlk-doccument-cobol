       IDENTIFICATION DIVISION.
       PROGRAM-ID. example5.
       DATA DIVISION.
       WORKING-STORAGE SECTION.

       01  Id-Stu      PIC     9(02).
       01  FILLER      PIC     X(5).    
       01  Name-Stu    PIC     X(25).
       01  FILLER      PIC     X(5). 
       01  Address-Stu PIC     X(50).
       
       PROCEDURE DIVISION.
           MOVE 01 TO Id-Stu.
           DISPLAY Id-Stu.
           MOVE "Le Kim Thuan" TO Name-Stu.
           DISPLAY Name-Stu.
           MOVE "Thanh Hoa" TO Address-Stu.
           DISPLAY Address-Stu.
           STOP RUN.