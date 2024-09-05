       IDENTIFICATION DIVISION.
       PROGRAM-ID. example8.
       DATA DIVISION.
       WORKING-STORAGE SECTION.
       01 PERSON-TABLE.
           05 PERSON OCCURS 100 TIMES ASCENDING KEY IS PERSON-AGE 
           INDEXED BY PERSON-INDX.
               10 PERSON-ID        PIC 9(04).
               10 PERSON-NAME      PIC X(20).
               10 PERSON-AGE       PIC 9(03).
       01 TEMP-PERSON.
           05 TEMP-PERSON-ID       PIC 9(04).
           05 TEMP-PERSON-NAME     PIC X(20).
           05 TEMP-PERSON-AGE      PIC 9(03).
       01  SEARCH-AGE              PIC 9(03).

       01  I                       PIC 9(03) VALUE 1.
       01  J                       PIC 9(03) VALUE 1.
       01  WS-COUNT                PIC 9(03).
       01  LOW                     PIC 9(03) VALUE 1.
       01  HIGH                    PIC 9(03).
       01  CHOICE                  PIC 9(01).
       01 NUM-OF-PERSONS         PIC 9(03) VALUE 0.
       PROCEDURE DIVISION.
       MAIN SECTION.
       MAIN-START.
           DISPLAY "CHOICE: " ACCEPT CHOICE
           EVALUATE CHOICE
               WHEN 1
                   PERFORM INPUT-DATA
                   GO TO MAIN
               WHEN 2 
                   PERFORM OUTPUT-DATA
                   GO TO MAIN
               WHEN 3
                   PERFORM 200-SEARCH-BY-AGE
                   GO TO MAIN
               WHEN OTHER
                   GO TO MAIN
           END-EVALUATE.
       MAIN-EXIT.
           EXIT.
           PERFORM 300-EXIT-PROGRAM.
           STOP RUN.

       INPUT-DATA.
           IF WS-COUNT < 101
               ADD 1 TO WS-COUNT.
               SET PERSON-INDX TO WS-COUNT
               DISPLAY "ID: " ACCEPT PERSON-ID(PERSON-INDX).
               DISPLAY "NAME: " ACCEPT PERSON-NAME(PERSON-INDX).
               DISPLAY "AGE: " ACCEPT PERSON-AGE(PERSON-INDX).

       OUTPUT-DATA.
           PERFORM 100-SORT-AGE
           IF WS-COUNT > 0
               PERFORM VARYING PERSON-INDX FROM 1 BY 1 
               UNTIL PERSON-INDX > WS-COUNT
                    DISPLAY " ID=" PERSON-ID (PERSON-INDX)
                           ", Name=" PERSON-NAME (PERSON-INDX)
                           ", Age=" PERSON-AGE (PERSON-INDX)
               END-PERFORM.
       100-SORT-AGE.
           PERFORM VARYING I FROM 1 BY 1 UNTIL I > WS-COUNT - 1
               PERFORM VARYING J FROM 1 BY 1 UNTIL J > WS-COUNT - I
                   IF PERSON-AGE (J) > PERSON-AGE (J + 1)
                       MOVE PERSON (J) TO TEMP-PERSON
                       MOVE PERSON (J + 1) TO PERSON (J)
                       MOVE TEMP-PERSON TO PERSON (J + 1)
                   END-IF
               END-PERFORM
           END-PERFORM.
       200-SEARCH-BY-AGE.
           SORT PERSON ON ASCENDING KEY PERSON-AGE
           DISPLAY "Enter age to search (three digits, e.g., 010): "
           ACCEPT SEARCH-AGE.

           SET LOW TO 1.
           SET HIGH TO 100
           SEARCH ALL PERSON
               AT END
               DISPLAY "No record found for age: ", SEARCH-AGE
               WHEN PERSON-AGE (PERSON-INDX) = SEARCH-AGE
               DISPLAY "Found: ID=" PERSON-ID (PERSON-INDX)
                    ", Name=" PERSON-NAME (PERSON-INDX)
                    ", Age=" PERSON-AGE (PERSON-INDX).

       300-EXIT-PROGRAM.
           DISPLAY "End of program.".
