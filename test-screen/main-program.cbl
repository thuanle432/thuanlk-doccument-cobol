       IDENTIFICATION DIVISION.
       PROGRAM-ID. InputOutputMenu.

       DATA DIVISION.
       WORKING-STORAGE SECTION.
       01 WS-USER-CHOICE   PIC X.
       01 WS-EXIT-FLAG     PIC X VALUE 'N'.
       01 WS-INPUT-DATA    PIC X(50).
       01 WS-DATA-COUNT    PIC 9(4) VALUE 0.
       01 WS-MAX-DATA      PIC 9(4) VALUE 100.
       01 WS-DATA-ARRAY.
           05 WS-DATA-ENTRY OCCURS 100 TIMES INDEXED BY WS-INDEX.
               10 WS-DATA-ITEM PIC X(50).

       SCREEN SECTION.
       01 MAIN-MENU-SCREEN.
           05 BLANK SCREEN.
           05 LINE 1  COLUMN 10 VALUE "==== MAIN MENU ====".
           05 LINE 3  COLUMN 10 VALUE "1. Nhap du lieu".
           05 LINE 4  COLUMN 10 VALUE "2. Xuat du lieu".
           05 LINE 5  COLUMN 10 VALUE "Q. Thoat".
           05 LINE 7  COLUMN 10 VALUE "Chon mot tuy chon: ".
           05 LINE 7  COLUMN 30 PIC X USING WS-USER-CHOICE.

       01 INPUT-SCREEN.
           05 BLANK SCREEN.
           05 LINE 1  COLUMN 1 VALUE "==== NHAP DU LIEU ====".
           05 LINE 3  COLUMN 10 VALUE "Nhap du lieu: ".
           05 LINE 3  COLUMN 25 PIC X(50) USING WS-INPUT-DATA.

       01 OUTPUT-SCREEN.
           05 BLANK SCREEN.
           05 LINE 1  COLUMN 10 VALUE "==== XUAT DU LIEU ====".
           05 LINE 3  COLUMN 10 VALUE "Danh sach du lieu da nhap:".
           05 LINE 5  COLUMN 25 PIC X(50) USING WS-INPUT-DATA.

       PROCEDURE DIVISION.
       MAIN-PROCEDURE.
           PERFORM UNTIL WS-EXIT-FLAG = 'Y'
               DISPLAY MAIN-MENU-SCREEN
               ACCEPT MAIN-MENU-SCREEN

               EVALUATE WS-USER-CHOICE
                   WHEN '1'
                       PERFORM INPUT-DATA-PROCEDURE
                   WHEN '2'
                       PERFORM OUTPUT-DATA-PROCEDURE
                   WHEN 'Q'
                       MOVE 'Y' TO WS-EXIT-FLAG
                   WHEN OTHER
                       DISPLAY "Lua chon khong hop le"
               END-EVALUATE
           END-PERFORM
           STOP RUN.

       INPUT-DATA-PROCEDURE.
           DISPLAY INPUT-SCREEN
           ACCEPT INPUT-SCREEN
           IF WS-DATA-COUNT < WS-MAX-DATA
               ADD 1 TO WS-DATA-COUNT
               MOVE WS-INPUT-DATA TO WS-DATA-ITEM(WS-DATA-COUNT)
               DISPLAY "Du lieu da duoc nhap thanh cong."
           ELSE
               DISPLAY "Khong the luu them du lieu, bo nho da day."
           END-IF
           DISPLAY "Nhan Enter de tiep tuc..."
           ACCEPT OMITTED.

       OUTPUT-DATA-PROCEDURE.
           IF WS-DATA-COUNT = 0
               DISPLAY "Chua co du lieu de xuat ra."
           ELSE
               SET WS-INDEX TO 1
               PERFORM UNTIL WS-INDEX > WS-DATA-COUNT
                   DISPLAY OUTPUT-SCREEN AT LINE 5 COLUMN 10
                   DISPLAY WS-INDEX UPON CONSOLE
                   DISPLAY ". " WS-DATA-ITEM(WS-INDEX)
                   ADD 1 TO WS-INDEX
               END-PERFORM
           END-IF
           DISPLAY "Nhan Enter de tiep tuc..."
           ACCEPT OMITTED.
