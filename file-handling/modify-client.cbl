       IDENTIFICATION DIVISION.
       PROGRAM-ID. modify-client.
       DATA DIVISION.
       LINKAGE SECTION.
       01  LS-VARIABLE.
           03  LS-CHOICE               PIC X(01).
           03  LS-NAME                 PIC X(15).
           03  LS-AGE                  PIC 9(02).
       PROCEDURE DIVISION USING LS-VARIABLE.
       MAIN SECTION.
       MAIN-START.
           EVALUATE LS-CHOICE
               WHEN 1
                   PERFORM EDIT-NAME
               WHEN 2
                   PERFORM EDIT-AGE         
           END-EVALUATE.
       MAIN-EXIT.
           EXIT PROGRAM.
      /
       EDIT-NAME SECTION.
       EDIT-NAME-START.
           DISPLAY 'INPUT NAME MODIFY: '       WITH NO ADVANCING
                                               ACCEPT LS-NAME.
       EDIT-NAME-EXIT.
           EXIT.
      /
       EDIT-AGE SECTION.
       EDIT-AGE-START.
           DISPLAY 'INPUT AGE MODIFY: '        WITH NO ADVANCING
                                               ACCEPT LS-AGE.
       EDIT-AGE-EXIT.
           EXIT.
