       IDENTIFICATION DIVISION.
       PROGRAM-ID. EmployeeManagement.

       DATA DIVISION.
       WORKING-STORAGE SECTION.
       01  WS-COUNT                    PIC 9(05) VALUE 0.
       01  MENU-OPTION                 PIC 9(01).
       01  WS-TARGET-ID                PIC 9(08).
       01  WS-CHOICE-MODIFY            PIC 9(02).
       01  WS-TEMP-TYPE                PIC X(01).

       01  WS-TABLE.
           03 WS-ENTRY OCCURS 9999 TIMES ASCENDING KEY IS WS-ID
           INDEXED BY IND.
               05  WS-ID               PIC 9(04).
               05  WS-TYPE             PIC X(01).  
               05  WS-NAME             PIC X(30).
               05  WS-POSITION         PIC X(20).
               05  WS-CODE             PIC X(10).
       01  WS-TEMP-ENTRY.
           03  WS-TEMP-ID              PIC 9(08).
           03  WS-TEMP-TYPE                 PIC X(01).  
           03  WSTEMP--NAME                 PIC X(30).
           03  WS-TEMP-POSITION             PIC X(20).
           03  WS-TEMP-CODE                 PIC X(10).
       01  I                           PIC 9(02).
       01  J                           PIC 9(02).
       01  WS-CURRENT-DATE             PIC 9(08).
       01  WS-CREATE-DATE              PIC 9(08).
       01  WS-MODIFY-DATE              PIC 9(08).
       01  WS-DELETE-DATE              PIC 9(08).
       PROCEDURE DIVISION.
       MAIN SECTION.
       MAIN-START.
           DISPLAY "**************************************************".
           DISPLAY "*                MENU                           *".
           DISPLAY "* 1. ADD NEW ENTRY                              *".
           DISPLAY "* 2. MODIFY ENTRY                               *".
           DISPLAY "* 3. VIEW ALL ENTRIES                           *".
           DISPLAY "* 4. SORT ENTRIES BY ID                         *".
           DISPLAY "* 5. SEARCH ENTRY BY ID                         *".
           DISPLAY "* 6. DELETE                                    *".
           DISPLAY "* 0. EXIT                                       *".
           DISPLAY "**************************************************".

           PERFORM UNTIL MENU-OPTION = 10
               DISPLAY "SELECT AN OPTION: " WITH NO ADVANCING
               ACCEPT MENU-OPTION
               DISPLAY "NUMBER: "MENU-OPTION
               EVALUATE MENU-OPTION
                   WHEN 1
                       PERFORM ADD-NEW-ENTRY
                       GO TO MAIN
                   WHEN 2
                       PERFORM MODIFY-ENTRY
                   WHEN 3
                       PERFORM DISPLAY-ALL-ENTRIES
                   WHEN 4
                       PERFORM SORT-ENTRIES
                   WHEN 5
                       PERFORM SEARCH-ENTRY
                    WHEN 6
                       PERFORM DELETE-ENTRY
                   WHEN OTHER
                       DISPLAY "INVALID OPTION."
               END-EVALUATE
           END-PERFORM.

       MAIN-EXIT.
           DISPLAY "EXIT PROGRAM."
           STOP RUN.
       DATE-CURRENT SECTION.
           MOVE FUNCTION CURRENT-DATE(1:8) TO WS-CURRENT-DATE.
       DATE-CREATE SECTION.
           MOVE WS-CURRENT-DATE TO WS-CREATE-DATE.
           DISPLAY "CREATE DATE: "WS-CREATE-DATE.
       ADD-NEW-ENTRY SECTION.
           IF WS-COUNT < 9999
               ADD 1 TO WS-COUNT
               SET IND TO WS-COUNT
               DISPLAY "ENTER ID: " WITH NO ADVANCING
               ACCEPT WS-ID(IND)
               DISPLAY "ENTER TYPE (D for Director, E for Employee): "
               WITH NO ADVANCING
               ACCEPT WS-TYPE(IND)
               DISPLAY "ENTER NAME: " WITH NO ADVANCING
               ACCEPT WS-NAME(IND)
               DISPLAY "ENTER POSITION: " WITH NO ADVANCING
               ACCEPT WS-POSITION(IND)
               DISPLAY "ENTER CODE: " WITH NO ADVANCING
               ACCEPT WS-CODE(IND)
           ELSE
               DISPLAY "TABLE IS FULL."
           END-IF.

       MODIFY-ENTRY SECTION.
           DISPLAY "ENTER ID TO MODIFY: " WITH NO ADVANCING
           ACCEPT WS-TARGET-ID
           SEARCH ALL WS-ENTRY
               AT END
                   DISPLAY "ID NOT FOUND: " WS-TARGET-ID
               WHEN WS-ID(IND) = WS-TARGET-ID
                   PERFORM DISPLAY-ENTRY
                   PERFORM MODIFY-FIELDS
           END-SEARCH.

       MODIFY-FIELDS SECTION.
           PERFORM UNTIL WS-CHOICE-MODIFY = 10
               DISPLAY "ENTER FIELD TO MODIFY (1-ID, 2-Type"
               ", 3-Name, 4-Position, 5-Code, 0-Exit): " 
               WITH NO ADVANCING
               ACCEPT WS-CHOICE-MODIFY
               EVALUATE WS-CHOICE-MODIFY
                   WHEN 1
                       DISPLAY "ENTER NEW ID: " WITH NO ADVANCING
                       ACCEPT WS-ID(IND)
                   WHEN 2
                       DISPLAY "ENTER NEW TYPE (D/E): " 
                       WITH NO ADVANCING
                       ACCEPT WS-TYPE(IND)
                   WHEN 3
                       DISPLAY "ENTER NEW NAME: " WITH NO ADVANCING
                       ACCEPT WS-NAME(IND)
                   WHEN 4
                       DISPLAY "ENTER NEW POSITION: " WITH NO ADVANCING
                       ACCEPT WS-POSITION(IND)
                   WHEN 5
                       DISPLAY "ENTER NEW CODE: " WITH NO ADVANCING
                       ACCEPT WS-CODE(IND)
                   WHEN 0
                       CONTINUE
                   WHEN OTHER
                       DISPLAY "INVALID OPTION."
               END-EVALUATE
           END-PERFORM.

       DISPLAY-ALL-ENTRIES SECTION.
           IF WS-COUNT > 0
               PERFORM VARYING IND FROM 1 BY 1 UNTIL IND > 9999
                   IF WS-ID(IND) > 0000
                       PERFORM DISPLAY-ENTRY
                       PERFORM DATE-CREATE
                   END-IF
               END-PERFORM
           ELSE
               DISPLAY "NO ENTRIES TO DISPLAY."
           END-IF.

       SORT-ENTRIES SECTION.
           PERFORM VARYING I FROM 1 BY 1 UNTIL I > WS-COUNT - 1
               PERFORM VARYING J FROM 1 BY 1 UNTIL J > WS-COUNT - I
                   IF WS-ID(J) > WS-ID(J + 1)
                       MOVE WS-ENTRY(J) TO WS-TEMP-ENTRY
                       MOVE WS-ENTRY(J + 1) TO WS-ENTRY(J)
                       MOVE WS-TEMP-ENTRY TO WS-ENTRY(J + 1)
                   END-IF
               END-PERFORM
           END-PERFORM.

       SEARCH-ENTRY SECTION.
           SORT WS-ENTRY ON ASCENDING KEY WS-ID
           DISPLAY "ENTER ID TO SEARCH: " WITH NO ADVANCING
           ACCEPT WS-TARGET-ID
           SEARCH ALL WS-ENTRY
               AT END
                   DISPLAY "ID NOT FOUND: " WS-TARGET-ID
               WHEN WS-ID(IND) = WS-TARGET-ID
                   PERFORM DISPLAY-ENTRY
           END-SEARCH.

       DISPLAY-ENTRY SECTION.
           DISPLAY "ID: " WS-ID(IND)
           DISPLAY "TYPE: " WS-TYPE(IND)
           DISPLAY "NAME: " WS-NAME(IND)
           DISPLAY "POSITION: " WS-POSITION(IND)
           DISPLAY "CODE: " WS-CODE(IND).
       DELETE-ENTRY SECTION.
           SORT WS-ENTRY ON ASCENDING KEY WS-ID
           DISPLAY "ENTER ID TO DELETE: " WITH NO ADVANCING
           ACCEPT WS-TARGET-ID
           SEARCH ALL WS-ENTRY
               AT END
                   DISPLAY "ID NOT FOUND: " WS-TARGET-ID
               WHEN WS-ID(IND) = WS-TARGET-ID
                   PERFORM MARK-AS-DELETED
           END-SEARCH.
           DISPLAY "ENTRY DELETED SUCCESSFULLY.".

       MARK-AS-DELETED SECTION.
           MOVE " " TO WS-NAME(IND)
           MOVE "X" TO WS-TYPE(IND)
           MOVE "0000" TO WS-ID(IND)
           DISPLAY "ID: " WS-ID(IND) " HAS BEEN MARKED AS DELETED.".
       MARK-AS-DELETED-END.
           EXIT.
