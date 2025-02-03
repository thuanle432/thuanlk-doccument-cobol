       IDENTIFICATION DIVISION.
       PROGRAM-ID. P0001.
       ENVIRONMENT DIVISION.
       INPUT-OUTPUT SECTION.
       FILE-CONTROL.
           SELECT U01 ASSIGN TO 'U01'
               ORGANIZATION IS LINE SEQUENTIAL.
       SELECT U30 ASSIGN TO 'U30'
               ORGANIZATION IS LINE SEQUENTIAL.
       
       DATA DIVISION.
       FILE SECTION.
       FD U01.
       01 U01-RECORD.
           05 U01-KEY     PIC X(10).  *> Key để kiểm tra trùng
           05 U01-DATA    PIC X(50).  *> Dữ liệu khác của record
       
       FD U30.
       01 U30-RECORD.
           05 U30-KEY     PIC X(10).
           05 U30-DATA    PIC X(50).
       
       WORKING-STORAGE SECTION.
       01 WS-END-OF-FILE    PIC X(1) VALUE 'N'.
       01 WS-READ-AGAIN     PIC X(1) VALUE 'N'.
       01 WS-TOTAL-RECORDS  PIC 9(4) VALUE 0.
       
       *> Bảng tạm để lưu key và số lần xuất hiện
       01 WS-KEY-STORAGE OCCURS 1000 TIMES.
           05 WS-STORED-KEY  PIC X(10).
           05 WS-COUNT       PIC 9(4).
       
       *> Biến tạm thời để kiểm tra
       01 WS-INDEX          PIC 9(4).
       01 WS-FOUND          PIC X(1).
       01 WS-CURRENT-KEY    PIC X(10).
       
       PROCEDURE DIVISION.
       MAIN-PROCEDURE.
           *> Mở file U01
           OPEN INPUT U01.
       
           *> Bước 1: Đếm số lần xuất hiện của từng key
           PERFORM READ-U01.
       
           PERFORM UNTIL WS-END-OF-FILE = 'Y'
               MOVE 'N' TO WS-FOUND
               MOVE 1 TO WS-INDEX
       
               *> Kiểm tra key đã tồn tại trong bảng WS-KEY-STORAGE chưa
               PERFORM UNTIL WS-INDEX > WS-TOTAL-RECORDS 
               OR WS-FOUND = 'Y'
                    IF WS-STORED-KEY(WS-INDEX) = U01-KEY THEN
                        ADD 1 TO WS-COUNT(WS-INDEX)
                        MOVE 'Y' TO WS-FOUND
                    END-IF
                    ADD 1 TO WS-INDEX
               END-PERFORM
       
               *> Nếu key chưa tồn tại, thêm vào danh sách
               IF WS-FOUND = 'N' THEN
                    ADD 1 TO WS-TOTAL-RECORDS
                    MOVE U01-KEY TO WS-STORED-KEY(WS-TOTAL-RECORDS)
                    MOVE 1 TO WS-COUNT(WS-TOTAL-RECORDS)
               END-IF
       
               *> Đọc record tiếp theo
               PERFORM READ-U01
           END-PERFORM.
       
           *> Đóng file U01
           CLOSE U01.
       
           *> Bước 2: Ghi lại các record có key trùng vào U30
           OPEN INPUT U01 OUTPUT U30.
           MOVE 'N' TO WS-END-OF-FILE
           MOVE 'Y' TO WS-READ-AGAIN
       
           PERFORM READ-U01.
       
           PERFORM UNTIL WS-END-OF-FILE = 'Y'
               MOVE 1 TO WS-INDEX
               MOVE 'N' TO WS-FOUND
       
               *> Kiểm tra key hiện tại có trùng không
               PERFORM UNTIL WS-INDEX > WS-TOTAL-RECORDS 
               OR WS-FOUND = 'Y'
                    IF WS-STORED-KEY(WS-INDEX) = U01-KEY 
                    AND WS-COUNT(WS-INDEX) > 1 THEN
                        MOVE 'Y' TO WS-FOUND
                    END-IF
                    ADD 1 TO WS-INDEX
               END-PERFORM
       
               *> Nếu key xuất hiện từ 2 lần trở lên, ghi vào U30
               IF WS-FOUND = 'Y' THEN
                    MOVE U01-RECORD TO U30-RECORD
                    WRITE U30-RECORD
               END-IF
       
               *> Đọc record tiếp theo
               PERFORM READ-U01
           END-PERFORM.
       
           *> Đóng file và kết thúc
           CLOSE U01 U30.
           STOP RUN.
       
       READ-U01.
           READ U01 AT END MOVE 'Y' TO WS-END-OF-FILE.
       