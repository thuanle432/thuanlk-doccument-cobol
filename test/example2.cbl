       IDENTIFICATION DIVISION.
       PROGRAM-ID. example2.

       DATA DIVISION.
       WORKING-STORAGE SECTION.
       01  Student.
           03  Id-Student      PIC     9(04).
           03  Name-Student.
               05  Last-Name   PIC     X(25).
               05  First-Name  PIC     X(25).
           03  Custom-Address.
               05  Num-Home    PIC     9(04).
               05  Commune     PIC     X(25).
               05  District    PIC     X(25).
               05  City        PIC     X(25).
       01  Full-Name           PIC     X(50).

       PROCEDURE DIVISION.
       BEGIN.
           MOVE 0001 TO Id-Student.
           DISPLAY "Enter Last Name: ".
           ACCEPT Last-Name.
           DISPLAY "Enter First Name: ".
           ACCEPT First-Name.
           MOVE SPACE TO FULL-NAME
           STRING First-Name DELIMITED BY SPACE
           "" Last-Name DELIMITED BY SIZE INTO Full-Name.
           DISPLAY "Enter Number home: ".
           ACCEPT Num-Home.
           DISPLAY "Enter Commune: ".
           ACCEPT Commune.
           DISPLAY "Enter District: ".
           ACCEPT District.
           DISPLAY "Enter City: ".
           ACCEPT City.
           PERFORM DISPLAY-CUSTOMER.

       DISPLAY-CUSTOMER.
           DISPLAY "Name: "    Full-Name
           DISPLAY "Address: " Num-Home 
                             "" Commune 
                             "" District 
                             "" City.
           STOP RUN.
