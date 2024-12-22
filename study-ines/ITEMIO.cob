       IDENTIFICATION                   DIVISION.
       PROGRAM-ID.                      ITEMIO.
       ENVIRONMENT                      DIVISION.
       INPUT-OUTPUT                     SECTION.
       FILE-CONTROL.
           SELECT ITEMIO ASSIGN TO "ITEM.txt"
           ORGANIZATION IS LINE SEQUENTIAL.
       DATA                             DIVISION.
       FILE                             SECTION.
       FD  ITEMIO.
       01  ITEM-REC.
         03  ITEM-NAME                  PIC  X(010).
         03  ITEM-AGE                   PIC  9(003).
       WORKING-STORAGE                  SECTION.
       01  WK-END-OF-FILE               PIC  X(001) VALUE 'N'.
       01  WK-FILE-OPENED               PIC  X(001) VALUE 'N'.
       LINKAGE                          SECTION.
       01  P-ITEM-REC.
         03  P-ITEM-NAME                PIC  X(010).
         03  P-ITEM-AGE                 PIC  9(003).
       01  P-END-OF-FILE                PIC  X(001).
       PROCEDURE                        DIVISION USING P-ITEM-REC
                                        P-END-OF-FILE.
           IF WK-END-OF-FILE            = 'Y'
               MOVE 'Y'                 TO P-END-OF-FILE
               EXIT PROGRAM
           END-IF.
           IF WK-FILE-OPENED        NOT = 'Y'
               OPEN INPUT ITEMIO
               MOVE 'Y'                 TO WK-FILE-OPENED
           END-IF.
           READ ITEMIO INTO ITEM-REC
               AT END
                   MOVE 'Y'             TO WK-END-OF-FILE
                   PERFORM EXIT-INIT
                   MOVE 'N'             TO WK-FILE-OPENED
               NOT AT END
                   MOVE ITEM-REC        TO P-ITEM-REC
           END-READ.
           MOVE WK-END-OF-FILE          TO P-END-OF-FILE.
           EXIT PROGRAM.

       EXIT-INIT                        SECTION.
           CLOSE ITEMIO.
       EXIT-INIT-EXIT.
           EXIT. 
           