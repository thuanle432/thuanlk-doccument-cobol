       IDENTIFICATION DIVISION.
       PROGRAM-ID. CHECK-DUPLICATE.

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
       01  U01-RECORD.
         05 INPUT-KEY                   PIC  X(004).  
         05 SHIMEI-KANA                 PIC  X(020). 
         05 SHIMEI-KANJI                PIC  X(020).
         05 FILLER                      PIC  X(015). 

       FD U30.
       01  U30-RECORD.
         05 OUTPUT-KEY                  PIC  X(010).
         05 OUT-KANA                    PIC  X(020). 
         05 OUT-KANJI                   PIC  X(020). 
         05 FILLER                      PIC  X(015).

       WORKING-STORAGE SECTION.
       01  WK-END-OF-FILE               PIC  X(001) VALUE 'N'.
       01  WK-TOTAL-RECORDS             PIC  9(004) VALUE ZERO.
       
      *> Biến lưu key mới và key cũ, khởi tạo LOW-VALUES
       01  WK-NEW-KEY                   PIC  X(040) VALUE LOW-VALUE.
       01  WK-OLD-KEY                   PIC  X(040) VALUE LOW-VALUE.
       
      *> Bảng tạm lưu key và số lần xuất hiện
       01  WK-KEY-STORAGE               OCCURS 1000 TIMES.
         05  WK-STORED-KEY              PIC  X(040).
         05  WK-COUNT                   PIC  9(004).

      *> Biến tạm để kiểm tra
       01  WK-INDEX                     PIC  9(004).
       01  WK-FOUND                     PIC  X(001).

       *> Biến đếm số lượng record
       01  WK-INPUT-COUNT               PIC  9(005) VALUE ZERO. 
       01  WK-OUTPUT-COUNT              PIC  9(005) VALUE ZERO.

       PROCEDURE DIVISION.
       
       *> ============================
       *> MAIN PROCEDURE - Chương trình chính
       *> ============================
       MAIN-PROCEDURE.
           OPEN INPUT U01.
           PERFORM READ-RECORD.
       
           PERFORM PROCESS-RECORDS UNTIL WK-NEW-KEY = HIGH-VALUES.
       
           CLOSE U01.
       
           OPEN INPUT U01 OUTPUT U30.
           PERFORM READ-RECORD.
           PERFORM WRITE-DUPLICATES.
           PERFORM CLOSE-FILES.
       
           STOP RUN.

       *> ============================
       *> READ-RECORD - Đọc file U01
       *> ============================
       READ-RECORD.
           READ U01 AT END MOVE 'Y' TO WK-END-OF-FILE.

       *> ============================
       *> WRITE-DUPLICATES - Ghi các record trùng vào file U30
       *> ============================
       WRITE-DUPLICATES.
           MOVE 'N' TO WK-END-OF-FILE.

           PERFORM UNTIL WK-END-OF-FILE = 'Y'
               MOVE 1 TO WK-INDEX
               MOVE 'N' TO WK-FOUND
       
               *> Kiểm tra key có trùng không
               PERFORM UNTIL WK-INDEX > WK-TOTAL-RECORDS 
               OR WK-FOUND = 'Y'
                    IF WK-STORED-KEY(WK-INDEX) = INPUT-KEY 
                    AND WK-COUNT(WK-INDEX) > 1 THEN
                        MOVE 'Y' TO WK-FOUND
                    END-IF
                    ADD 1 TO WK-INDEX
               END-PERFORM
       
               *> Nếu key xuất hiện từ 2 lần trở lên, ghi vào U30
               IF WK-FOUND = 'Y' THEN
                    MOVE U01-RECORD TO U30-RECORD
                    WRITE U30-RECORD

                    *> Tăng số lượng record đầu ra
                    ADD 1 TO WK-OUTPUT-COUNT
               END-IF
       
               PERFORM READ-RECORD
           END-PERFORM.

       *> ============================
       *> PROCESS-RECORDS - Đọc file U01 và đếm số lần xuất hiện
       *> ============================
       PROCESS-RECORDS.
           MOVE INPUT-KEY TO WK-NEW-KEY.

           *> Tăng số lượng record đầu vào
           ADD 1 TO WK-INPUT-COUNT.
       
           IF WK-END-OF-FILE = 'Y' THEN
               MOVE HIGH-VALUES TO WK-NEW-KEY
           ELSE
               MOVE 'N' TO WK-FOUND
               MOVE 1 TO WK-INDEX
       
               *> Kiểm tra key đã tồn tại chưa
               PERFORM UNTIL WK-INDEX > WK-TOTAL-RECORDS 
               OR WK-FOUND = 'Y'
                    IF WK-STORED-KEY(WK-INDEX) = WK-NEW-KEY THEN
                        ADD 1 TO WK-COUNT(WK-INDEX)
                        MOVE 'Y' TO WK-FOUND
                    END-IF
                    ADD 1 TO WK-INDEX
               END-PERFORM
       
               *> Nếu key chưa tồn tại, thêm vào danh sách
               IF WK-FOUND = 'N' THEN
                    ADD 1 TO WK-TOTAL-RECORDS
                    MOVE WK-NEW-KEY TO WK-STORED-KEY(WK-TOTAL-RECORDS)
                    MOVE 1 TO WK-COUNT(WK-TOTAL-RECORDS)
               END-IF
       
               MOVE WK-NEW-KEY TO WK-OLD-KEY
               PERFORM READ-RECORD
           END-IF.

       *> ============================
       *> CLOSE-FILES - Đóng file
       *> ============================
       CLOSE-FILES.
           CLOSE U01 U30.

           *> Hiển thị số lượng record đã đọc và ghi
           DISPLAY "COUNT INPUT: " WK-INPUT-COUNT.
           DISPLAY "COUNT OUTPUT: " WK-OUTPUT-COUNT.
