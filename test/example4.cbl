       IDENTIFICATION DIVISION.
       PROGRAM-ID. example4.
       DATA DIVISION.
       WORKING-STORAGE SECTION.
       01    WS-Test1      PIC     X(10).
       01    WS-Test2      PIC     X(10).
       01    WS-Test3      PIC     X(10).
       PROCEDURE DIVISION.
       MAIN-SRART.
           PERFORM Custom-test1 THRU Custom-test3.
           EXIT.
       
       Custom-test1.
           MOVE "TEST 1" TO WS-Test1.
           DISPLAY WS-Test1.
       Custom-test2.
           MOVE "TEST 2" TO WS-Test2.
           DISPLAY WS-Test2.
       Custom-test3.
           MOVE "TEST 3" TO WS-Test3.
           DISPLAY WS-Test3.
           
           STOP RUN.
        