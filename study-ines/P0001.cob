       IDENTIFICATION                   DIVISION.
       PROGRAM-ID.                      P0001.
       ENVIRONMENT                      DIVISION.
        CONFIGURATION                   SECTION.
       DATA DIVISION.
       WORKING-STORAGE                  SECTION.
       01  WK-RECORD.
         03  WK-NAME                    PIC  X(010).
         03  WK-AGE                     PIC  9(003).
       01  END-OF-FILE                  PIC  X(001) VALUE 'N'.
       PROCEDURE                        DIVISION.

           PERFORM UNTIL END-OF-FILE    = 'Y'
               CALL 'ITEMIO'            USING WK-RECORD
                                        END-OF-FILE
               IF END-OF-FILE       NOT = 'Y'
                   DISPLAY WK-RECORD
               END-IF
           END-PERFORM.
           STOP RUN.
