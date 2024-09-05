       IDENTIFICATION DIVISION.
       PROGRAM-ID. contional.

       DATA DIVISION.
       WORKING-STORAGE SECTION.
       01  A       PIC     9(02)V9.
      *01  B       PIC     9(01).

       PROCEDURE DIVISION.
           DISPLAY "Num1: " WITH NO ADVANCING ACCEPT A.
      *    DISPLAY "Num2: " ACCEPT B.

      *    IF A > B
      *        DISPLAY "True"
      *    ELSE
      *        DISPLAY "False"
      *    END-IF.


      *    EVALUATE TRUE
      *        WHEN A >= 8.5
      *            DISPLAY "Diem A"
      *        WHEN A >= 7.0 AND A < 8.5
      *            DISPLAY "Diem B"
      *        WHEN A >= 5.5 AND A < 7
      *            DISPLAY "Diem C"
      *        WHEN A < 5.5 AND A >= 4.0
      *            DISPLAY "Diem D"
      *        WHEN A < 4.0
      *            DISPLAY "Diem F"
      *        WHEN A > 10 AND A < 0
      *            DISPLAY "Nhap khon hop le"
      *    END-EVALUATE.
      
           EVALUATE A
               WHEN 1
                   DISPLAY "Thang 1"
               WHEN 2
                   DISPLAY "Thang 2"
               WHEN 3
                   DISPLAY "Thang 3"
               WHEN 4
                   DISPLAY "Thang 4"
               WHEN 5
                   DISPLAY "Thang 5"
               WHEN 6
                   DISPLAY "Thang 6"
               WHEN OTHER
                   DISPLAY "Nhap sai"
           END-EVALUATE.
           STOP RUN.
