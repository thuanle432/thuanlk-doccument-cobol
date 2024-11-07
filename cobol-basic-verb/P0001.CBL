       IDENTIFICATION DIVISION.
       PROGRAM-ID. P0001.
      ******************************************************************
      *                                            AUTHOR              *
      *                                            LE KIM THUAN        *
      ******************************************************************
       DATA DIVISION.
       WORKING-STORAGE SECTION.
       01  WS-BTECH-1ST-YEAR.
           03  SUBJECT-MARKS           PIC 9(03) OCCURS 6 TIMES 
                                       INDEXED BY MARKS-INDEX.
       01  I                           PIC 9(01) VALUE 1.
       PROCEDURE DIVISION.
           PERFORM UNTIL I > 6
               MOVE I                  TO SUBJECT-MARKS(I)
               SET I                   UP BY 1
           END-PERFORM.
           SET MARKS-INDEX             TO 1.
           PERFORM UNTIL MARKS-INDEX > 6
               DISPLAY 'SUBJECT '      MARKS-INDEX ' : ' 
                                       SUBJECT-MARKS(MARKS-INDEX)
               SET MARKS-INDEX         UP BY 1
           END-PERFORM.
           STOP RUN.
