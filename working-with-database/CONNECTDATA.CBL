       IDENTIFICATION DIVISION.
       PROGRAM-ID. CONNECTDATA.
       ENVIRONMENT DIVISION.
       CONFIGURATION SECTION.
       SOURCE-COMPUTER. IBM-PC.
       OBJECT-COMPUTER. IBM-PC.

       DATA DIVISION.
       WORKING-STORAGE SECTION.
       EXEC SQL INCLUDE SQLCA END-EXEC.
       01 Server-Name PIC X(20) VALUE 'localhost'.
       01 Database-Name PIC X(20) VALUE 'thuanlktest'.
       01 User-ID PIC X(20) VALUE 'postgres'.
       01 Password PIC X(20) VALUE SPACE.

       PROCEDURE DIVISION.
           EXEC SQL
               CONNECT TO :Database-Name USER :User-ID USING :Password
           END-EXEC.

           IF SQLCODE = 0
               DISPLAY "Kết nối thành công."
           ELSE
               DISPLAY "Lỗi kết nối, SQLCODE:" SQLCODE
           END-IF.

           EXEC SQL
               DISCONNECT :Database-Name
           END-EXEC.

           STOP RUN.
