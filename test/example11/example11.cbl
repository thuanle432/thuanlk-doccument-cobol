       IDENTIFICATION DIVISION.
       PROGRAM-ID. example11.
       DATA DIVISION.
       WORKING-STORAGE SECTION.
       01  WS-ERROR-CODE           PIC X(04)  VALUE SPACES.
       01  WS-ERROR-MESSAGE        PIC X(25)  VALUE SPACES.
       01  WS-FLAGERROR            PIC 9(01).
       PROCEDURE DIVISION.
           DISPLAY "HELLO WORLD "
           PERFORM E001
           PERFORM E002
           STOP RUN.
       E001.
           MOVE 1 TO WS-FLAGERROR.
           PERFORM E003.
       E002.
           MOVE 2 TO WS-FLAGERROR.
           PERFORM E003.
       E003.
           EVALUATE TRUE
               WHEN WS-FLAGERROR = 1
                   MOVE "E001"  TO WS-ERROR-CODE
                   PERFORM E004
                   DISPLAY WS-ERROR-MESSAGE
               WHEN WS-FLAGERROR = 2
                   MOVE "E002"  TO WS-ERROR-CODE
                   PERFORM E004
                   DISPLAY WS-ERROR-MESSAGE.
       E004.
           CALL "error-message"            USING  
                                           WS-ERROR-CODE 
                                           WS-ERROR-MESSAGE.        
       