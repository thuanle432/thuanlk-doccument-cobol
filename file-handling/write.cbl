       IDENTIFICATION DIVISION.
       PROGRAM-ID. write1.
       ENVIRONMENT DIVISION.
       INPUT-OUTPUT SECTION.
       FILE-CONTROL.
           SELECT STUDENT ASSIGN TO "STUDENT.DAT"
           ORGANIZATION IS LINE SEQUENTIAL
           ACCESS IS SEQUENTIAL
           FILE STATUS IS WS-STATUS.
               
           SELECT TEMP-STUDENT ASSIGN TO "TEMP.DAT"
           ORGANIZATION IS LINE SEQUENTIAL
           ACCESS MODE IS SEQUENTIAL
           FILE STATUS IS WS-STATUS.
       DATA DIVISION.
       FILE SECTION.
       FD  STUDENT.
       01  STUDENT-RECORD.
           03  FS-STUDENT-ID           PIC 9(03).
           03  FS-STUDENT-NAME         PIC X(15).
           03  FS-STUDENT-AGE          PIC 9(02).
       FD  TEMP-STUDENT.
       01  TEMP-STUDENT-RECORD.
           03  TEMP-STUDENT-ID         PIC 9(03).
           03  TEMP-STUDENT-NAME       PIC X(15).
           03  TEMP-STUDENT-AGE         PIC 9(02).
       WORKING-STORAGE SECTION.
       01  WS-MENU-OPTION              PIC X(01).
           88  FC-CREATE               VALUE "A".
           88  FC-VIEW                 VALUE "B".
           88  FC-DELETE               VALUE "D".
           88  FC-EXIT                 VALUE "E".
       01  WS-VARIABLE.
           03  WS-STUDENT.
               05  WS-STUDENT-ID       PIC 9(03).
               05  WS-STUDENT-NAME     PIC X(15).
               05  WS-STUDENT-AGE      PIC X(02).
           03  WS-OEF                  PIC X(01) VALUE "N".
           03  WS-CHOICE               PIC 9(01).
           03  WS-STATUS               PIC X(02).
           03  WS-SEARCH-ID            PIC 9(03).
           03  WS-DELETED              PIC X(01).
       PROCEDURE DIVISION.
       MAIN SECTION.
       MAIN-START.
           PERFORM UNTIL WS-MENU-OPTION        = "E"
               DISPLAY "INPUT OPTION: "    WITH NO ADVANCING
                                           ACCEPT WS-MENU-OPTION
               EVALUATE WS-MENU-OPTION
                   WHEN "A"
                       PERFORM CREATE-STUDENT
                   WHEN "B"
                       PERFORM VIEW-STUDENT
                   WHEN 'C'
                       PERFORM MODIFY-STUDENT
                   WHEN "D"
                       PERFORM DELETE-STUDENT
               END-EVALUATE
           END-PERFORM.
       MAIN-EXIT.
           EXIT.
           STOP RUN.
      /
       CREATE-STUDENT SECTION.
       CREATE-STUDENT-START.
           OPEN EXTEND STUDENT.
           DISPLAY "INPUT ID: "            WITH NO ADVANCING
                                           ACCEPT FS-STUDENT-ID.
           DISPLAY "INPUT NAME: "          WITH NO ADVANCING
                                           ACCEPT FS-STUDENT-NAME.
           DISPLAY "INPUT AGE: "           WITH NO ADVANCING
                                           ACCEPT FS-STUDENT-AGE.
           WRITE STUDENT-RECORD
           END-WRITE.
           CLOSE STUDENT.
       CREATE-STUDENT-EXIT.
           EXIT.
      /
       VIEW-STUDENT SECTION.
       VIEW-STUDENT-START.
           OPEN INPUT STUDENT.
           PERFORM UNTIL WS-OEF                = "Y"
               READ STUDENT INTO WS-STUDENT
                   AT END
                       MOVE "Y"            TO WS-OEF
                   NOT AT END
                       DISPLAY "ID: " WS-STUDENT-ID
                       DISPLAY "NAME: " WS-STUDENT-NAME
                       DISPLAY "AGE: " WS-STUDENT-AGE
               END-READ
           END-PERFORM.
           CLOSE STUDENT.
       VIEW-STUDENT-EXIT.
           EXIT.
      /
       MODIFY-STUDENT SECTION.
       MODIFY-STUDENT-START.
           DISPLAY 'INPUT OPTION MODIFY: '     WITH NO ADVANCING
                                               ACCEPT WS-CHOICE.
           CALL 'modify-client'                USING WS-CHOICE
                                               WS-VARIABLE.
       MODIFY-STUDENT-EXIT.
           EXIT.
      /
       DELETE-STUDENT SECTION.
       DELETE-STUDENT-START.
           DISPLAY "INPUT ID TO DELETE: " WITH NO ADVANCING
           ACCEPT WS-SEARCH-ID.
           OPEN INPUT STUDENT.
           IF WS-STATUS NOT = "00"
               DISPLAY "ERROR OPENING STUDENT FILE!"
               GO TO DELETE-STUDENT-EXIT
           END-IF.
           OPEN OUTPUT TEMP-STUDENT
           IF WS-STATUS NOT = "00"
               DISPLAY "ERROR OPENING TEMP FILE!"
               GO TO DELETE-STUDENT-EXIT
           END-IF.
           MOVE "N" TO WS-OEF.
           MOVE "N" TO WS-DELETED.
           PERFORM UNTIL WS-OEF = "Y"
               READ STUDENT INTO WS-STUDENT
               AT END
                   MOVE "Y" TO WS-OEF
               NOT AT END
                   IF WS-SEARCH-ID = WS-STUDENT-ID
                       MOVE "Y" TO WS-DELETED
                   ELSE
                       MOVE WS-STUDENT-ID TO TEMP-STUDENT-ID
                       MOVE WS-STUDENT-NAME TO TEMP-STUDENT-NAME
                       MOVE WS-STUDENT-AGE TO TEMP-STUDENT-AGE
                       WRITE TEMP-STUDENT-RECORD
                   END-IF
               END-READ
           END-PERFORM.
           CLOSE STUDENT.
           CLOSE TEMP-STUDENT.
           IF WS-DELETED = "N"
               DISPLAY "STUDENT ID NOT FOUND"
               CALL "SYSTEM" USING "rm TEMP.DAT"
           ELSE
               CALL "SYSTEM" USING "rm STUDENT.DAT"
               CALL "SYSTEM" USING "mv TEMP.DAT STUDENT.DAT"
               DISPLAY "RECORD DELETED SUCCESSFULLY"
           END-IF.
       DELETE-STUDENT-EXIT.
           EXIT.
