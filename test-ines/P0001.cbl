      ******************************************************************
      *  PROGRAM: TEST002N - ĐỌC FILE, GHI RECORD TRÙNG VÀO U30        *
      ******************************************************************
       IDENTIFICATION                  DIVISION.
       PROGRAM-ID.                      TEST002N.

       ENVIRONMENT                      DIVISION.
       INPUT-OUTPUT                     SECTION.
         FILE-CONTROL.
           SELECT U01 ASSIGN TO 'U01'.
           SELECT U30 ASSIGN TO 'U30'.

       DATA                             DIVISION.
       FILE                             SECTION.

       FD  U01.
       01  U01-RECORD.
           03  INPUT-KEY                PIC  X(004).
           03  SHIMEI-KANA              PIC  X(020).
           03  SHIMEI-KANJI             PIC  N(020).
           03  FILLER                   PIC  X(15).

       FD  U30.
       01  U30-RECORD.
           03  OUTPUT-KEY               PIC  X(004).
           03  OUT-KANA                 PIC  X(020).
           03  OUT-KANJI                PIC  N(020).
           03  FILLER                   PIC  X(015).

       WORKING-STORAGE                  SECTION.
       01  WS-EOF-FLAG                  PIC  X(001) VALUE 'N'.  
       01  OLD-KEY                      PIC  X(004) VALUE LOW-VALUE.
       01  NEW-KEY                      PIC  X(004) VALUE LOW-VALUE.
       01  WS-RECORD-COUNT              PIC  9(005) VALUE ZERO.

       PROCEDURE                        DIVISION.
      ******************************************************************
      *  メイン処理                                                     *
      ******************************************************************
       MAIN                             SECTION.
         HAJIME.
           DISPLAY '***  TEST002N START ***'.
           OPEN INPUT U01.
           OPEN OUTPUT U30.

           PERFORM READ-INPUT.
           PERFORM PROCESS-FILE UNTIL WS-EOF-FLAG = 'Y'.

           PERFORM CLOSE-FILE.
           STOP RUN.
      ******************************************************************
      *  XỬ LÝ DỮ LIỆU                                                 *
      ******************************************************************
       PROCESS-FILE                     SECTION.
         HAJIME.
           MOVE INPUT-KEY TO NEW-KEY.

           IF OLD-KEY = LOW-VALUES THEN
               MOVE NEW-KEY TO OLD-KEY
               MOVE 1 TO WS-RECORD-COUNT
           ELSE
               IF NEW-KEY = OLD-KEY THEN
                   ADD 1 TO WS-RECORD-COUNT
                   PERFORM WRITE-OUTPUT
               ELSE
                   IF WS-RECORD-COUNT > 1 THEN
                       PERFORM WRITE-OUTPUT
                   END-IF
                   MOVE NEW-KEY TO OLD-KEY
                   MOVE 1 TO WS-RECORD-COUNT
               END-IF
           END-IF

           PERFORM READ-INPUT.
         OWARI.
           EXIT.
      ******************************************************************
      *  ファイルCLOSE                                                  *
      ******************************************************************
       CLOSE-FILE                        SECTION.
         HAJIME.
           CLOSE U01 U30.
           DISPLAY '***  TEST002N END ***'.
         OWARI.
           EXIT.
      ******************************************************************
      *  ĐỌC DỮ LIỆU TỪ U01                                            *
      ******************************************************************
       READ-INPUT                        SECTION.
         HAJIME.
           READ U01 INTO U01-RECORD
               AT END 
                   MOVE 'Y' TO WS-EOF-FLAG.
         OWARI.
           EXIT.
      ******************************************************************
      *  GHI RECORD TRÙNG VÀO U30                                      *
      ******************************************************************
       WRITE-OUTPUT                      SECTION.
         HAJIME.
           MOVE OLD-KEY TO OUTPUT-KEY.
           MOVE SHIMEI-KANA TO OUT-KANA.
           MOVE SHIMEI-KANJI TO OUT-KANJI.
           WRITE U30-RECORD.
         OWARI.
           EXIT.
