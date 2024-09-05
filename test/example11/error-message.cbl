       IDENTIFICATION DIVISION.
       PROGRAM-ID. error-message.
       DATA DIVISION.
       LINKAGE SECTION.
       01  LS-ERROR-CODE           PIC    X(04).
           88 E001                 VALUE  "E001".
           88 E002                 VALUE  "E002".
       01  LS-ERROR-MESSAGE        PIC    X(25) VALUE SPACES.
       PROCEDURE DIVISION USING LS-ERROR-CODE LS-ERROR-MESSAGE.
           EVALUATE TRUE
              WHEN E001
                  MOVE "THUAN OC CHO" TO LS-ERROR-MESSAGE
              WHEN E002
                  MOVE "THUAN NGU NHU BO" TO LS-ERROR-MESSAGE
           END-EVALUATE.
           EXIT PROGRAM.
           