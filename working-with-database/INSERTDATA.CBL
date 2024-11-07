       IDENTIFICATION DIVISION.
       PROGRAM-ID. INSERTDATA.

       ENVIRONMENT DIVISION.
       CONFIGURATION SECTION.
       DATA DIVISION.
       WORKING-STORAGE SECTION.
       01  SQLCODE                     PIC S9(9) COMP.
       01  SQLERRMC                    PIC X(70).
       01  USER-ID                     PIC 9(4).
       01  USER-NAME                   PIC X(30).
       01  USER-AGE                    PIC 9(3).

       EXEC SQL INCLUDE SQLCA END-EXEC.

       PROCEDURE DIVISION.
       MAIN-LOGIC.
           DISPLAY "Connecting to the database...".
           EXEC SQL
               CONNECT TO 'thuanlktest' 
               USER 'postgres' 
               USING ''
           END-EXEC.
           IF SQLCODE NOT EQUAL ZERO
               DISPLAY "Error connecting to DB: ", SQLERRMC
               GO TO END-PROGRAM
           END-IF.

           EXEC SQL
               CREATE TABLE IF NOT EXISTS users (
                   id SERIAL PRIMARY KEY,
                   name VARCHAR(100),
                   age INTEGER
               )
           END-EXEC.

           DISPLAY "Enter User Name:"              WITH NO ADVANCING
                                                   ACCEPT USER-NAME.
           DISPLAY "Enter User Age:"               WITH NO ADVANCING
                                                   ACCEPT USER-AGE.

           EXEC SQL
               INSERT INTO users (id, name, age)
               VALUES (:USER-ID, :USER-NAME, :USER-AGE)
           END-EXEC
           IF SQLCODE NOT EQUAL ZERO
               DISPLAY "Error inserting data: ", SQLERRMC
           ELSE
               DISPLAY "Data inserted successfully."
           END-IF.

           EXEC SQL
               DISCONNECT CURRENT
           END-EXEC    

       END-PROGRAM.
           DISPLAY "Program finished."
           STOP RUN.
