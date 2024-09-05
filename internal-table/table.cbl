       IDENTIFICATION DIVISION.
       PROGRAM-ID. table.
       DATA DIVISION.
       WORKING-STORAGE SECTION.
       01  OPTION                  PIC     9(01).
       01  WS-COUNT                PIC     9(03)   VALUE 0.
       01  TABLE-STUDENT.
           03  STUDENT-RECORD OCCURS 999 TIMES INDEXED BY Ind.
               05  ID-STUDENT      PIC     9(03).
               05  NAME-STUDENT    PIC     X(11).
               05  BOD-STUDENT     PIC     X(10).
           03  TOTAL-RECORD        PIC     9(03) VALUE 0.

       01  Search-ID               PIC     9(03).
       01  ID-POSITION             PIC     9(03).

       PROCEDURE DIVISION.
       MAIN SECTION.
       MAIN-START.
           DISPLAY "***************************************".
           DISPLAY "*****************MENU******************".
           DISPLAY "*1. INPUT DATA                         ".
           DISPLAY "*2. OUTPUT DATA                        ".
           DISPLAY "*3. SEARCH DATA                        ".
           DISPLAY "*4. UPDATE DATA                        ".
           DISPLAY "***************************************".
           DISPLAY "INPUT OPTION: " WITH NO ADVANCING ACCEPT OPTION.
           PERFORM UNTIL OPTION = 0
               EVALUATE OPTION
                   WHEN 1
                       PERFORM INPUT-DATA
                       GO TO MAIN
                   WHEN 2
                       PERFORM OUTPUT-DATA
                       GO TO MAIN
                   WHEN 3
                       PERFORM SEARCH-DATA
                       GO TO MAIN
                   WHEN 4
                       PERFORM UPDATE-DATA
                       GO TO MAIN
      *            WHEN 5 
      *                PERFORM DELETE-DATA
      *                GO TO MAIN
                   WHEN 0
                       GO TO MAIN
                   WHEN OTHER 
                       DISPLAY "INPUT ERROR! INPUT, PLEASE"
                       GO TO MAIN
               END-EVALUATE
           END-PERFORM.
       MAIN-EXIT.
           EXIT.
           DISPLAY "END PROGRAMME!".
           STOP RUN.
       INPUT-DATA.
           IF WS-COUNT < 1000
               ADD 1 TO WS-COUNT
               SET Ind TO WS-COUNT
               DISPLAY "INPUT ID: " WITH NO ADVANCING ACCEPT
               ID-STUDENT (Ind)
               DISPLAY "INPUT NAME: " WITH NO ADVANCING ACCEPT
               NAME-STUDENT  (Ind)
               DISPLAY "INPUT BOD: " WITH NO ADVANCING ACCEPT
               BOD-STUDENT (Ind)
           ELSE
               DISPLAY "LIST IS FULL"
           END-IF.
           
       OUTPUT-DATA.
           IF WS-COUNT > 0
           PERFORM VARYING Ind FROM 1 BY 1 UNTIL Ind > WS-COUNT
               DISPLAY "***************************************"
               DISPLAY "*STUDENT-ID: " 
               ID-STUDENT (Ind) "*" 
               NAME-STUDENT (Ind) "*"
               BOD-STUDENT (Ind) "*"
               DISPLAY "***************************************"
           END-PERFORM.
       SEARCH-DATA.
           DISPLAY "INPUT ID WANT TO SEARCH: " WITH NO ADVANCING
           ACCEPT Search-ID.
           PERFORM VARYING Ind FROM 1 BY 1 UNTIL Ind > WS-COUNT
               IF ID-STUDENT (Ind) = Search-ID
                   DISPLAY "***************************************"
                   DISPLAY "DATA : "
                   DISPLAY "ID: " ID-STUDENT (Ind)
                   DISPLAY "Name: " NAME-STUDENT (Ind)
                   DISPLAY "Birthdate: " BOD-STUDENT (Ind)
                   DISPLAY "***************************************"
                   EXIT PERFORM
               END-IF
           END-PERFORM.
       UPDATE-DATA.
           DISPLAY "INPUT ID WANT TO UPDATE: " WITH NO ADVANCING
           ACCEPT Search-ID.
           PERFORM VARYING Ind FROM 1 BY 1 UNTIL Ind > WS-COUNT
               IF ID-STUDENT (Ind) = Search-ID
                   DISPLAY "***************************************"
                   DISPLAY "*STUDENT-ID: " 
                   ID-STUDENT (Ind) "*" 
                   NAME-STUDENT (Ind) "*"
                   BOD-STUDENT (Ind) "*"
                   DISPLAY "***************************************"
                   DISPLAY "INPUT ID: " WITH NO ADVANCING ACCEPT
                   ID-STUDENT (Ind)
                   DISPLAY "INPUT NAME: " WITH NO ADVANCING ACCEPT
                   NAME-STUDENT  (Ind)
                   DISPLAY "INPUT BOD: " WITH NO ADVANCING ACCEPT
                   BOD-STUDENT (Ind)
                   INSPECT ID-STUDENT (Ind) REPLACING ALL 
                   ID-STUDENT (Ind) BY ID-STUDENT (Ind)
                   INSPECT NAME-STUDENT (Ind) REPLACING ALL 
                   NAME-STUDENT (Ind) BY NAME-STUDENT (Ind) 
                   INSPECT BOD-STUDENT (Ind) REPLACING ALL 
                   BOD-STUDENT (Ind) BY BOD-STUDENT (Ind)
               END-IF
           END-PERFORM.
      *DELETE-DATA.
      *    DISPLAY "INPUT ID WANT TO DELETE: " WITH NO ADVANCING
      *    ACCEPT Search-ID.
      *    PERFORM VARYING Ind FROM 1 BY 1 UNTIL Ind > TOTAL-RECORD
      *        IF ID-STUDENT (Ind) = Search-ID
      *            PERFORM VARYING Ind FROM Ind BY 1 UNTIL Ind =
      *            TOTAL-RECORD - 1
      *                MOVE ID-STUDENT (Ind + 1) TO ID-STUDENT (Ind)
      *                MOVE NAME-STUDENT (Ind + 1) TO NAME-STUDENT (Ind)
      *                MOVE BOD-STUDENT (Ind + 1) TO BOD-STUDENT (Ind)
      *            END-PERFORM
      *            SUBTRACT 1 FROM TOTAL-RECORD
      *        END-IF
      *    END-PERFORM.
      *    DISPLAY "DELETE SUSSUFULLY!".

