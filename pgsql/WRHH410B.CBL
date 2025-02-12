001000******************************************************************
001100*  Copyright (c) INES Corporation 2017. All rights reserved.     *
001200*                                                                *
001300*  業務名        ：国民健康保険　情報集約システム対応            *
001400*                                                                *
001500*  顧客名        ：共通                                          *
001600*                                                                *
001700*  処理名        ：市町村被保険者ID取込処理                      *
001800*                                                                *
001900*  作成者        ：大塚　丈義                                    *
002000*                                                                *
002100*  作成日        ：2017.05.22                                    *
002200*                                                                *
002300*  案件ID        ：71773,72245                                   *
002400*                                                                *
002500*  修正履歴      ：                                              *
002600*    修正年月日  -----------------  修正内容  -------------------*
002700*  2017.11.07  川上成憲       案件ID：72602 @履歴ID:171107SK     *
002800*              政令市の場合の判定変更                            *
002900*              (行政区コード≠SPACE→P汎用フラグ（6)(1:1)='1')   *
003000*                                                                *
003100******************************************************************
003200 IDENTIFICATION                   DIVISION.
003300 PROGRAM-ID.                      WRHH410B.
003400*
003500 ENVIRONMENT                      DIVISION.
003600 CONFIGURATION                    SECTION.
003700   SOURCE-COMPUTER.               FACOM.
003800   OBJECT-COMPUTER.               FACOM.
003900*
004000 SPECIAL-NAMES.
004100     ENVIRONMENT-NAME  IS KOKUHO
004200     ENVIRONMENT-VALUE IS KOKUHO-ADR.
004300*
004400 DATA                             DIVISION.
004500*
004600 WORKING-STORAGE                  SECTION.
004700 01  PID                          PIC  X(008)  VALUE 'WRHH410B'.
004800*ワークエリア
004900 01  WORK-AREA.
005000   03  IDX1                       PIC  9(003).
005100   03  IDX2                       PIC  9(003).
005200   03  Ｗ処理日                   PIC  X(008).
005300   03  Ｗ処理時                   PIC  X(008).
005400   03  Ｗ処理日時                 PIC  X(014).
005500   03  Ｗ行政区数                 PIC S9(003).
005600   03  Ｗ市町村保険者番号         PIC  X(008).
005700   03  Ｗ宛名番号Ｘ               PIC  X(012).
005800   03  Ｗ宛名番号９               REDEFINES  Ｗ宛名番号Ｘ
005900                                  PIC  9(012).
006000   03  Ｗ住民コード９             PIC  9(010).
006100   03  ＷメッセージＮｏ           PIC  X(003).
006200   03  Ｗメッセージ内容           PIC  N(060).
006300*
006400 01  WK-HOKEN-AREA.
006500   03  Ｗ保険者テーブル           OCCURS  100.
006600     05  ＷＫ行政区コード         PIC  X(003).
006700     05  ＷＫ表示用保険者番号     PIC  X(008).
006800*
006900     EXEC SQL BEGIN DECLARE  SECTION  END-EXEC.
007000*
007100 01  Ｈ情報集約－個人基本.
007200   03  Ｈ住民コード               PIC S9(010).
007300   03  Ｈ市町村被保険者ＩＤ       PIC  X(015).
007400   03  Ｈ該当件数                 PIC S9(009)  BINARY.
007500*ＳＱＬシステム変数
007600     COPY WRHDSQL.
007700*SERVER パラメータ
007800 01  W-ACCEPT.
007900   03  H-KOKUHO                   PIC  X(003).
008000   03  H-KOKUHO-ADR               PIC  X(010).
008100
008200*
008300     EXEC SQL END DECLARE  SECTION  END-EXEC.
008400*
008500 77  U01-EOF                      PIC  X(003)  VALUE  SPACE.
008600 77  U42-EOF                      PIC  X(003)  VALUE  SPACE.
008700 77  U01-CNT                      PIC  9(008)  VALUE  ZERO.
008800 77  U42-CNT                      PIC  9(008)  VALUE  ZERO.
008900 77  U30-CNT                      PIC  9(008)  VALUE  ZERO.
009000 77  U31-CNT                      PIC  9(008)  VALUE  ZERO.
009100 77  CNT-ERR-REC                  PIC  9(008)  VALUE  ZERO.
009200 77  CNT-UPD                      PIC  9(008)  VALUE  ZERO.
009300 77  CNT-INS                      PIC  9(008)  VALUE  ZERO.
009400 77  CNT-NORMAL                   PIC  9(008)  VALUE  ZERO.
009500 77  CNT-ERROR                    PIC  9(008)  VALUE  ZERO.
009600*
009700*================================================================
009800* KAMA021A(入力ﾌｧｲﾙ(U01)のｱｸｾｽ)インターフェース領域
009900*================================================================
010000 01  PARM-KAMA021-U01.
010100   03  KAMA021A-DD-U01            PIC  X(008)  VALUE  'U01'.
010200   03  KAMA021A-SYORI-U01         PIC  X(008)  VALUE  SPACE.
010300   03  KAMA021A-RL-U01            PIC S9(009)  COMP-5.
010400   03  KAMA021A-BL-U01            PIC S9(009)  COMP-5.
010500   03  KAMA021A-CNT-U01           PIC S9(009)  COMP-5.
010600   03  KAMA021A-EOF-U01           PIC  X(004)  VALUE  SPACE.
010700 01  U01-REC.
010800     COPY  WRHHT376.
010900*
011000*================================================================
011100* KAMA021A(入力ﾌｧｲﾙ(U42)のｱｸｾｽ)インターフェース領域
011200*================================================================
011300 01  PARM-KAMA021-U42.
011400   03  KAMA021A-DD-U42            PIC  X(008)  VALUE  'U42'.
011500   03  KAMA021A-SYORI-U42         PIC  X(008)  VALUE  SPACE.
011600   03  KAMA021A-RL-U42            PIC S9(009)  COMP-5.
011700   03  KAMA021A-BL-U42            PIC S9(009)  COMP-5.
011800   03  KAMA021A-CNT-U42           PIC S9(009)  COMP-5.
011900   03  KAMA021A-EOF-U42           PIC  X(004)  VALUE  SPACE.
012000 01  U42-REC.
012100     COPY  WRHHT313.
012200*
012300*================================================================
012400* KAMA026A(出力ﾌｧｲﾙのｱｸｾｽ)インターフェース領域
012500*================================================================
012600 01  PARM-KAMA026-U30.
012700   03  KAMA026A-DD-U30            PIC  X(008)  VALUE  'U30'.
012800   03  KAMA026A-SYORI-U30         PIC  X(008)  VALUE  SPACE.
012900   03  KAMA026A-RL-U30            PIC S9(009)  COMP-5.
013000   03  KAMA026A-BL-U30            PIC S9(009)  COMP-5.
013100   03  KAMA026A-CNT-U30           PIC S9(009)  COMP-5.
013200 01  U30-REC.
013300     COPY  WRHHT354.
013400*
013500*================================================================
013600* KAMA026A(出力ﾌｧｲﾙのｱｸｾｽ)インターフェース領域
013700*================================================================
013800 01  PARM-KAMA026-U31.
013900   03  KAMA026A-DD-U31            PIC  X(008)  VALUE  'U31'.
014000   03  KAMA026A-SYORI-U31         PIC  X(008)  VALUE  SPACE.
014100   03  KAMA026A-RL-U31            PIC S9(009)  COMP-5.
014200   03  KAMA026A-BL-U31            PIC S9(009)  COMP-5.
014300   03  KAMA026A-CNT-U31           PIC S9(009)  COMP-5.
014400 01  U31-REC.
014500     COPY  WRHHT356.
014600*
014700*================================================================
014800* ＮＶＨＤＺ０１Ｕ（メッセージ表示）
014900*================================================================
015000 01  WRHDZ01U-PARM.
015100     COPY  NVHDPZ01.
015200*
015300*================================================================
015400* ＷＲＨＨ３００Ｕ（情報集約共通制御情報取得）
015500*================================================================
015600 01  PARM-WRHHP300U.
015700     COPY  WRHHP300.
015800*
015900*================================================================
016000* ＷＲＨＲ８２Ｕ（被保険者被保険者証記号番号編集）
016100*================================================================
016200 01  PARM-WRHH302U.
016300     COPY  WRHHP302.
016400*
016500*================================================================
016600* ＰＣＢＮＶＤＩＶＵ（行政区名称索引）
016700*================================================================
016800     COPY  PCBNVDIV     REPLACING  ==()==  BY  ==Ｐ==.                      
016900*
017000*================================================================
017100* サブプログラム名
017200*================================================================
017300     COPY  PGNAMETB.
017400
017500 PROCEDURE                        DIVISION.
017600******************************************************************
017700* Level (0.0.0)  メ   イ   ン   処      理                       *
017800******************************************************************
017900 MAIN-SHORI                       SECTION.
018000 CONTOROL-START.
018100*
018200     PERFORM  INIT-RTN.
018300*
018400     PERFORM  MAIN-RTN
018500              UNTIL  U01-EOF  =  'END'.
018600*
018700     PERFORM  TERM-RTN.
018800*
018900     STOP    RUN.
019000 CONTOROL-EXIT.
019100     EXIT.
019200*****************************************************************
019300* Level (1.0.0)  初    期       処    理                        *
019400*****************************************************************
019500 INIT-RTN                         SECTION.
019600 INIT-START.
019700*
019800*-----------------------------*
019900* プログラム開始メッセージ表示*
020000*-----------------------------*
020100     INITIALIZE    WRHDZ01U-PARM.
020200     MOVE  PID                    TO  WRHDPZ01-PID.
020300     MOVE  '0'                    TO  WRHDPZ01-KBN.
020400     CALL  'NVHDZ01U'          USING  WRHDZ01U-PARM.
020500*
020600     MOVE  'OPEN'                 TO  KAMA021A-SYORI-U01.
020700     COMPUTE KAMA021A-RL-U01       =  FUNCTION LENG(U01-REC).
020800     CALL  'KAMA021A'          USING  PARM-KAMA021-U01.
020900     MOVE  'OPEN'                 TO  KAMA021A-SYORI-U42.
021000     COMPUTE KAMA021A-RL-U42       =  FUNCTION LENG(U42-REC).
021100     CALL  'KAMA021A'          USING  PARM-KAMA021-U42.
021200     MOVE  'OPEN'                 TO  KAMA026A-SYORI-U30.
021300     COMPUTE KAMA026A-RL-U30       =  FUNCTION LENG(U30-REC).
021400     CALL  'KAMA026A'          USING  PARM-KAMA026-U30.
021500     MOVE  'OPEN'                 TO  KAMA026A-SYORI-U31.
021600     COMPUTE KAMA026A-RL-U31       =  FUNCTION LENG(U31-REC).
021700     CALL  'KAMA026A'          USING  PARM-KAMA026-U31.
021800*
021900     INITIALIZE    WORK-AREA
022000                   WK-HOKEN-AREA
022100                   Ｈ情報集約－個人基本.
022200*
022300     MOVE  SPACE                  TO  W-ACCEPT.
022400*    処理日時取得
022500     PERFORM  SHORIBI-GET-RTN.
022600*    情報集約共通制御情報取得
022700     CALL  'WRHH300U'          USING  PARM-WRHHP300U.
022800*
022900     IF  Ｐリターンコード  OF  PARM-WRHHP300U  NOT =  ZERO
023000         DISPLAY  '共通制御パラメータ　項目番号エラー'
023100                  'Ｐリターンコード＝' Ｐリターンコード
023200                                       OF  PARM-WRHHP300U
023300                                     UPON  SYSOUT
023400         CALL   'ABENDU'
023500     END-IF.
023600*
023700     DISPLAY  '*** WRHH300U ***'     UPON  SYSOUT.
023800*
023900*項番001:保険者番号セット
024000     IF  Ｐ固定値－Ｘ(01)     NOT  =  SPACE
024100         DISPLAY  '保険者番号セット ＝ '
024200                  Ｐ固定値－Ｘ(01)  UPON  SYSOUT
024300         MOVE     Ｐ固定値－Ｘ(01)(1:8)
024400                                  TO  Ｗ市町村保険者番号
024500       ELSE
024600         DISPLAY  '保険者番号セットが存在しません。'
024700                                    UPON  SYSOUT
024800         CALL  'ABENDU'
024900     END-IF.
025000*項番006:表示用保険者番号汎用項目制御
025100     EVALUATE  Ｐ汎用フラグ(06)(1:1)
025200         WHEN  '1'
025300               DISPLAY  '表示用保険者番号汎用項目制御 ＝ '
025400                        Ｐ汎用フラグ(06)(1:1)  UPON  SYSOUT
025500               INITIALIZE  WK-HOKEN-AREA
025600               PERFORM  VARYING  IDX1  FROM  1  BY  1
025700                   UNTIL  U42-EOF  =  'END'  OR
025800                          IDX1     >  100
025900                     PERFORM  U42-READ-RTN
026000               END-PERFORM
026100               COMPUTE  Ｗ行政区数  =  IDX1  -  1
026200         WHEN  SPACE
026300               IF  Ｐ固定値－Ｘ(06)     NOT  =  SPACE
026400                   DISPLAY  '表示用保険者番号汎用項目制御 ＝ '
026500                            Ｐ固定値－Ｘ(06)  UPON  SYSOUT
026600                 ELSE
026700                   DISPLAY
026800                   '表示用保険者番号汎用項目制御が存在しません。'
026900                   UPON  SYSOUT
027000               END-IF
027100         WHEN  OTHER
027200               DISPLAY  '表示用保険者番号汎用項目制御エラー。'
027300               UPON  SYSOUT
027400               CALL  'ABENDU'
027500     END-EVALUATE.
027600*
027700     PERFORM   SQL-CONNECT-RTN.
027800*
027900     PERFORM   U01-READ-RTN.
028000*
028100 INIT-EXIT.
028200     EXIT.
028300******************************************************************
028400* Level (1.1.0)       処理日時取得処理                           *
028500******************************************************************
028600 SHORIBI-GET-RTN                  SECTION.
028700 SHORIBI-GET-RTN-START.
028800*
028900     MOVE  FUNCTION  CURRENT-DATE(1:8)
029000                                  TO  Ｗ処理日.
029100     MOVE  FUNCTION  CURRENT-DATE(9:6)
029200                                  TO  Ｗ処理時.
029300     MOVE  Ｗ処理日               TO  Ｗ処理日時 (1:8).
029400     MOVE  Ｗ処理時(1:2)          TO  Ｗ処理日時 (10:2).
029500     MOVE  ':'                    TO  Ｗ処理日時 (12:1).
029600     MOVE  Ｗ処理時(3:2)          TO  Ｗ処理日時 (13:2).
029700*
029800     DISPLAY ' '                                 UPON  SYSOUT.
029900     DISPLAY '(' PID ')'
030000             Ｗ処理日(1:4) '/' Ｗ処理日(5:2) '/' Ｗ処理日(7:2)
030100         ' ' Ｗ処理時(1:2) ':' Ｗ処理時(3:2) ':' Ｗ処理時(5:2)
030200                                                 UPON  SYSOUT.
030300     DISPLAY ' '                                 UPON  SYSOUT.
030400*
030500 SHORIBI-GET-RTN-EXIT.
030600     EXIT.
030700******************************************************************
030800* Level (1.2.0)       ファイル１  読込処理                       *
030900******************************************************************
031000 U42-READ-RTN                     SECTION.
031100 U42-READ-START.
031200*
031300     MOVE  'READ'                 TO  KAMA021A-SYORI-U42
031400     CALL  'KAMA021A'          USING  PARM-KAMA021-U42
031500                                      U42-REC.
031600*
031700     IF  KAMA021A-EOF-U42 (1:3)         =  X'B5DCD8'
031800         MOVE  'END'              TO  U42-EOF
031900       ELSE
032000         MOVE  行政区コード  OF U42-REC
032100                                  TO  ＷＫ行政区コード(IDX1)
032200         MOVE  表示用被保険者番号  OF  U42-REC
032300                                  TO  ＷＫ表示用保険者番号(IDX1)
032400     END-IF.
032500*
032600 U42-READ-EXIT.
032700     EXIT.
032800******************************************************************
032900* Level (1.3.0)   ＳＱＬ  ＳＥＲＶＥＲ コネクト 処 理            *
033000******************************************************************
033100 SQL-CONNECT-RTN                  SECTION.
033200 SQL-CONNECT-START.
033300*
033400*-----------------------------------*
033500* ＳＱＬ　ＳＥＲＶＥＲコネクト処理  *
033600*-----------------------------------*
033700*
033800***  国保
033900     DISPLAY    'KOKUHO'          UPON  KOKUHO.
034000     ACCEPT     H-KOKUHO-ADR      FROM  KOKUHO-ADR.
034100*
034200     MOVE  'KOK'                  TO  H-KOKUHO.
034300     EXEC   SQL
034400         CONNECT TO :H-KOKUHO-ADR AS :H-KOKUHO
034500     END-EXEC.
034600*
034700     EVALUATE  SQLSTATE
034800         WHEN  '00000'
034900         WHEN  '01000'
035000               DISPLAY NC'接続に成功しました' '(' H-KOKUHO-ADR ')'
035100         WHEN  OTHER
035200               DISPLAY NC'接続に失敗しました' '(' H-KOKUHO-ADR ')'
035300               PERFORM SQL-ERR-RTN
035400     END-EVALUATE.
035500*
035600 SQL-CONNECT-EXIT.
035700     EXIT.
035800*
035900******************************************************************
036000* Level (1.4.0)       ファイル    読込処理                       *
036100******************************************************************
036200 U01-READ-RTN                     SECTION.
036300 U01-READ-START.
036400*
036500     MOVE   'READ'                TO  KAMA021A-SYORI-U01.
036600     CALL   'KAMA021A'            USING  PARM-KAMA021-U01
036700                                         U01-REC.
036800*
036900     IF      KAMA021A-EOF-U01(1:3)  =  X'B5DCD8'
037000             MOVE 'END'           TO  U01-EOF
037100     END-IF.
037200*
037300 U01-READ-EXIT.
037400     EXIT.
037500******************************************************************
037600* Level (2.0.0)       主        処         理                    *
037700******************************************************************
037800 MAIN-RTN                         SECTION.
037900 MAIN-START.
038000*
038100     PERFORM   KOMOKU-CHECK-RTN.
038200*
038300     IF  ＷメッセージＮｏ  =  SPACE
038400         PERFORM  U30-WRITE-RTN
038500         PERFORM  TABLE-KOUSHIN-RTN
038600     ELSE
038700         COMPUTE  CNT-ERR-REC  =  CNT-ERR-REC  +  1
038800     END-IF.
038900*
039000     PERFORM   U01-READ-RTN.
039100*
039200 MAIN-EXIT.
039300     EXIT.
039400*
039500******************************************************************
039600* Level (2.1.0)    市町村被保険者ID連携ファイルチェック処理      *
039700******************************************************************
039800 KOMOKU-CHECK-RTN                 SECTION.
039900 KOMOKU-CHECK-RTN-START.
040000*
040100     MOVE  SPACE                TO  ＷメッセージＮｏ.
040200*    市町村保険者番号
040300     IF  個人特定情報－市町村保険者番号  OF  U01-REC
040400                                =   SPACE
040500         MOVE  'H01'            TO  ＷメッセージＮｏ
040600         PERFORM  MSG-READ-RTN
040700         PERFORM  U31-WRITE-RTN
040800     END-IF.
040900*
041000     IF  FUNCTION STORED-CHAR-LENGTH
041100         (個人特定情報－市町村保険者番号  OF  U01-REC)  NOT =  8
041200         MOVE  'H02'            TO  ＷメッセージＮｏ
041300         PERFORM  MSG-READ-RTN
041400         PERFORM  U31-WRITE-RTN
041500     END-IF.
041600*
041700     IF  個人特定情報－市町村保険者番号  OF  U01-REC
041800                                NOT =  Ｗ市町村保険者番号
041900         MOVE  'H03'            TO  ＷメッセージＮｏ
042000         PERFORM  MSG-READ-RTN
042100         PERFORM  U31-WRITE-RTN
042200     END-IF.
042300*
042400*    被保険者証番号
042500     IF  被保険者証番号  OF  U01-REC
042600                                =   SPACE
042700         MOVE  'H05'            TO  ＷメッセージＮｏ
042800         PERFORM  MSG-READ-RTN
042900         PERFORM  U31-WRITE-RTN
043000     END-IF.
043100*
043200*    宛名番号
043300     IF  宛名番号  OF  U01-REC  =   SPACE
043400         MOVE  'H06'            TO  ＷメッセージＮｏ
043500         PERFORM  MSG-READ-RTN
043600         PERFORM  U31-WRITE-RTN
043700     END-IF.
043800*
043900     MOVE  12                   TO  IDX2.
044000     MOVE  ZERO                 TO  Ｗ宛名番号Ｘ.
044100     PERFORM  VARYING  IDX1  FROM  12  BY  -1
044200       UNTIL  IDX1  <  1
044300         IF  宛名番号 OF U01-REC (IDX1:1)  NOT =  SPACE
044400             MOVE  宛名番号 OF U01-REC (IDX1:1)
044500                                TO  Ｗ宛名番号Ｘ (IDX2:1)
044600             COMPUTE  IDX2  =  IDX2  -  1
044700         END-IF
044800     END-PERFORM.
044900*
045000     IF  Ｗ宛名番号Ｘ  IS NOT  NUMERIC
045100         MOVE  'H07'            TO  ＷメッセージＮｏ
045200         PERFORM  MSG-READ-RTN
045300         PERFORM  U31-WRITE-RTN
045400     END-IF.
045500*
045600*市町村被保険者ID
045700     IF  市町村被保険者ＩＤ  OF  U01-REC  =  SPACE
045800         MOVE  'H09'            TO  ＷメッセージＮｏ
045900         PERFORM  MSG-READ-RTN
046000         PERFORM  U31-WRITE-RTN
046100     END-IF.
046200*
046300     IF  FUNCTION STORED-CHAR-LENGTH
046400         (市町村被保険者ＩＤ  OF  U01-REC)  NOT =  15
046500         MOVE  'H10'            TO  ＷメッセージＮｏ
046600         PERFORM  MSG-READ-RTN
046700         PERFORM  U31-WRITE-RTN
046800     END-IF.
046900*
047000*照合結果ＦＬＧ（T宛名とH資格個人の存在チェックの結果)
047100     IF  照合結果ＦＬＧ  OF  U01-REC  NOT =  '1'
047200         MOVE  'H08'            TO  ＷメッセージＮｏ
047300         PERFORM  MSG-READ-RTN
047400         PERFORM  U31-WRITE-RTN
047500     END-IF.
047600*
047700 KOMOKU-CHECK-RTN-EXIT.
047800     EXIT.
047900*
048000******************************************************************
048100* Level (2.2.0)   市町村被保険者ID連携取込結果ファイル出力処理   *
048200******************************************************************
048300 U30-WRITE-RTN                    SECTION.
048400 U30-WRITE-RTN-START.
048500*
048600     INITIALIZE                       U30-REC.
048700*    キーエリア
048800     MOVE  キーエリア  OF  U01-REC
048900                                  TO  キーエリア  OF  U30-REC.
049000*    連番
049100     COMPUTE  CNT-NORMAL  =  CNT-NORMAL  +  1.
049200     MOVE  CNT-NORMAL             TO  連番  OF  U30-REC.
049300*    個人特定情報
049400*    被保険者証記号
049500     MOVE  被保険者証記号  OF  U01-REC
049600                                  TO  被保険者証記号  OF  U30-REC.
049700*    被保険者証番号
049800     MOVE  被保険者証番号  OF  U01-REC
049900                                  TO  被保険者証番号  OF  U30-REC.
050000*    宛名番号
050100     MOVE  宛名番号  OF  U01-REC  TO  宛名番号  OF  U30-REC.
050200*    市町村被保険者ID
050300     MOVE  市町村被保険者ＩＤ  OF  U01-REC
050400                                  TO  市町村被保険者ＩＤ
050500                                      OF  U30-REC.
050600*    カナ氏名
050700     MOVE  カナ氏名  OF  U01-REC  TO  カナ氏名  OF  U30-REC.
050800*    氏名
050900     MOVE  氏名  OF  U01-REC      TO  氏名  OF  U30-REC.
051000*    行政区保険者番号
051100*    行政区コード≠SPACEの場合
051200*▼ 案件 ID=72602 2017/11/07 政令市の場合の判定変更               171107SK
051300*     IF  行政区コード  OF  U01-REC  NOT =  SPACE
051400*        汎用フラグ='1'の場合
051500     IF  Ｐ汎用フラグ(06)(1:1) =  '1'
051600*▲ 案件 ID=72602 2017/11/07 政令市の場合の判定変更               171107SK
051700*            テーブル検索
051800             PERFORM  VARYING  IDX1  FROM  1  BY  1
051900                  UNTIL    IDX1  >  Ｗ行政区数  OR
052000                           ＷＫ行政区コード(IDX1)
052100                             =  行政区コード  OF U01-REC
052200                 CONTINUE
052300             END-PERFORM
052400             IF  IDX1  <=  Ｗ行政区数
052500                 MOVE  ＷＫ表示用保険者番号(IDX1)
052600                   TO  行政区保険者番号  OF  U30-REC
052700             END-IF
052800*▼ 案件 ID=72602 2017/11/07 政令市の場合の判定変更               171107SK
052900*         ELSE
053000*             MOVE  Ｐ固定値－Ｘ(06) (1:8)
053100*               TO  行政区保険者番号  OF  U30-REC
053200*         END-IF
053300*▲ 案件 ID=72602 2017/11/07 政令市の場合の判定変更               171107SK
053400*    行政区名
053500         MOVE  NC'行政区　'       TO  行政区名  OF  U30-REC (1:4)
053600         INITIALIZE               PARM-PCBNVDIV
053700         MOVE  '1'                TO  Ｐ索引パターン
053800         MOVE  行政区コード  OF  U01-REC
053900                                  TO  Ｐ行政区コード
054000         CALL  PCBNVDIV            USING  PARM-PCBNVDIV
054100         IF  Ｐリターンコード  OF  PARM-PCBNVDIV  =  ZERO
054200             MOVE  Ｐ行政区名称略 TO  行政区名  OF  U30-REC (5:16)
054300             IF  Ｐ管理行政区コード (1:3)
054400                   NOT =  行政区コード  OF  U01-REC
054500                 MOVE  Ｐ管理行政区コード (1:3)
054600                                  TO  行政区コード  OF  U30-REC
054700             END-IF
054800         ELSE
054900             DISPLAY  '*** PCBNVDIVU ***'
055000             DISPLAY  '行政区名称取得エラー'
055100             DISPLAY  '　リターンコード　＝　'  Ｐリターンコード
055200                                                OF  PARM-PCBNVDIV
055300                      '　行政区コード　＝　'    Ｐ行政区コード
055400                      '　索引パターン　＝　'    Ｐ索引パターン
055500         END-IF
055600*▼ 案件 ID=72602 2017/11/07 政令市の場合の判定変更               171107SK
055700     ELSE
055800         MOVE  SPACE  TO  行政区コード  OF  U30-REC
055900         MOVE  Ｐ固定値－Ｘ(06) (1:8)
056000           TO  行政区保険者番号  OF  U30-REC
056100*▲ 案件 ID=72602 2017/11/07 政令市の場合の判定変更               171107SK
056200     END-IF.
056300*    備考
056400     MOVE  SPACE                  TO  備考  OF  U30-REC.
056500*    取込日時
056600     MOVE  Ｗ処理日時             TO  取込日時  OF  U30-REC.
056700*
056800     MOVE  'WRITE'                TO  KAMA026A-SYORI-U30.
056900     CALL  'KAMA026A'          USING  PARM-KAMA026-U30
057000                                      U30-REC.
057100*
057200 U30-WRITE-RTN-EXIT.
057300     EXIT.
057400*
057500******************************************************************
057600* Level (2.3.0)   Ｈ情報集約_個人基本  更新処理                  *
057700******************************************************************
057800 TABLE-KOUSHIN-RTN                SECTION.
057900 TABLE-KOUSHIN-RTN-START.
058000*
058100*    Ｈ情報集約_個人基本の存在チェック
058200     MOVE  ZERO                   TO  Ｈ該当件数.
058300     COMPUTE  Ｗ住民コード９
058400       =  FUNCTION  NUMVAL(住民コード  OF  U01-REC).
058500     MOVE  Ｗ住民コード９         TO  Ｈ住民コード.
058600*
058700     PERFORM  TABLE-SELECT-RTN.
058800*
058900*    Ｈ情報集約_個人基本の更新
059000*
059100     IF  Ｈ該当件数  =  ZERO
059200         PERFORM  TABLE-INSERT-RTN
059300     ELSE
059400         PERFORM  TABLE-UPDATE-RTN
059500     END-IF.
059600*
059700 TABLE-KOUSHIN-RTN--EXIT.
059800     EXIT.
059900*
060000******************************************************************
060100* Level (2.3.1)   Ｈ情報集約_個人基本  索引処理                  *
060200******************************************************************
060300 TABLE-SELECT-RTN                 SECTION.
060400 TABLE-SELECT-RTN-START.
060500*
060600     EXEC SQL
060700          SELECT  COUNT (H情報集約_個人基本.住民コード)
060800                    AS  該当件数
060900            INTO   :Ｈ該当件数
061000            FROM   H情報集約_個人基本
061100           WHERE   H情報集約_個人基本.住民コード  = :Ｈ住民コード
061200     END-EXEC.
061300*
061400     EVALUATE  SQLSTATE
061500         WHEN "00000"
061600         WHEN "02000"
061700           CONTINUE
061800         WHEN  OTHER
061900           DISPLAY  
062000             "（ H情報集約_個人基本）読み込み処理に失敗しました "
062100             " 住民コード = "  Ｈ住民コード
062200           PERFORM  SQL-ERR-RTN
062300     END-EVALUATE.
062400*
062500 TABLE-SELECT-RTN-EXIT.
062600     EXIT.
062700*
062800******************************************************************
062900* Level (2.3.2)   Ｈ情報集約_個人基本  追加処理                  *
063000******************************************************************
063100 TABLE-INSERT-RTN                 SECTION.
063200 TABLE-INSERT-RTN-START.
063300*
063400*    市町村保険者ＩＤ
063500     MOVE  市町村被保険者ＩＤ  OF  U01-REC
063600                                  TO  Ｈ市町村被保険者ＩＤ.
063700*
063800     EXEC SQL
063900          INSERT INTO H情報集約_個人基本
064000                 (住民コード
064100                 ,市町村被保険者ID
064200                 ,更新年月日)
064300          VALUES (:Ｈ住民コード
064400                 ,:Ｈ市町村被保険者ＩＤ
064500                 ,GetDate())
064600     END-EXEC.
064700*
064800     EVALUATE  SQLSTATE
064900         WHEN "00000"
065000           COMPUTE  CNT-INS  =  CNT-INS  +  1
065100         WHEN  OTHER
065200           DISPLAY  
065300             "（ H情報集約_個人基本）追加処理に失敗しました "
065400             " 住民コード = "  Ｈ住民コード
065500           PERFORM  SQL-ERR-RTN
065600     END-EVALUATE.
065700*
065800 TABLE-INSERT-RTN-EXIT.
065900     EXIT.
066000*
066100******************************************************************
066200* Level (2.3.3)   Ｈ情報集約_個人基本  更新処理                  *
066300******************************************************************
066400 TABLE-UPDATE-RTN                 SECTION.
066500 TABLE-UPDATE-RTN-START.
066600*
066700*    市町村保険者ＩＤ
066800     MOVE  市町村被保険者ＩＤ  OF  U01-REC
066900                                  TO  Ｈ市町村被保険者ＩＤ.
067000*
067100     EXEC SQL
067200          UPDATE  H情報集約_個人基本
067300             SET  市町村被保険者ID  = :Ｈ市町村被保険者ＩＤ
067400                 ,更新年月日        = GetDate()
067500           WHERE  住民コード        = :Ｈ住民コード
067600     END-EXEC.
067700*
067800     EVALUATE  SQLSTATE
067900         WHEN "00000"
068000           COMPUTE  CNT-UPD  =  CNT-UPD  +  1
068100         WHEN  OTHER
068200           DISPLAY  
068300             "（ H情報集約_個人基本）更新処理に失敗しました "
068400             " 住民コード = "  Ｈ住民コード
068500           PERFORM  SQL-ERR-RTN
068600     END-EVALUATE.
068700*
068800 TABLE-INSERT-RTN-EXIT.
068900     EXIT.
069000*
069100******************************************************************
069200* Level (2.4.0)   情報集約用メッセージファイル読込処理           *
069300******************************************************************
069400 MSG-READ-RTN                     SECTION.
069500 MSG-READ-RTN-START.
069600*
069700     INITIALIZE                   PARM-WRHH302U.
069800     MOVE  SPACE                  TO  Ｗメッセージ内容.
069900     MOVE  ＷメッセージＮｏ       TO  Ｐメッセージ番号.
070000     CALL  'WRHH302U'             USING  PARM-WRHH302U.
070100*
070200     IF  Ｐリターンコード  OF  PARM-WRHH302U  =  ZERO
070300         MOVE  Ｐメッセージ内容   TO  Ｗメッセージ内容
070400     ELSE
070500         DISPLAY  '*** WRHH302U ***'
070600         DISPLAY  '情報集約メッセージファイル読込エラー'
070700         DISPLAY  '　リターンコード　＝　'    Ｐリターンコード
070800                                              OF  PARM-WRHH302U
070900                  '　Ｐメッセージ番号　＝　'  Ｐメッセージ番号
071000     END-IF.
071100*
071200 MSG-READ-RTN-EXIT.
071300     EXIT.
071400*
071500******************************************************************
071600* Level (2.5.0)市町村被保険者ID連携取込結果エラーファイル出力処理*
071700******************************************************************
071800 U31-WRITE-RTN                    SECTION.
071900 U31-WRITE-RTN-START.
072000*
072100     INITIALIZE                       U31-REC.
072200*    キーエリア
072300     MOVE  キーエリア  OF  U01-REC
072400                                  TO  キーエリア  OF  U31-REC.
072500*    記号番号
072600*    記号番号≠SPACEの場合
072700     IF  記号番号  OF  U01-REC  NOT =  SPACE
072800         MOVE  記号番号  OF  U01-REC
072900                                  TO  記号番号  OF  U31-REC
073000     ELSE
073100*
073200         MOVE  世帯番号  OF  U01-REC
073300                                  TO  記号番号  OF  U31-REC
073400     END-IF.
073500*    連番
073600     COMPUTE  CNT-ERROR  =  CNT-ERROR  +  1.
073700     MOVE  CNT-ERROR              TO  連番  OF  U31-REC.
073800*    個人特定情報
073900*    被保険者証記号
074000     MOVE  被保険者証記号  OF  U01-REC
074100                                  TO  被保険者証記号  OF  U31-REC.
074200*    被保険者証番号
074300     MOVE  被保険者証番号  OF  U01-REC
074400                                  TO  被保険者証番号  OF  U31-REC.
074500*    宛名番号
074600     MOVE  宛名番号  OF  U01-REC  TO  宛名番号  OF  U31-REC.
074700*    市町村被保険者ID
074800     MOVE  市町村被保険者ＩＤ  OF  U01-REC
074900                                  TO  市町村被保険者ＩＤ
075000                                      OF  U31-REC.
075100*    カナ氏名
075200     MOVE  カナ氏名  OF  U01-REC  TO  カナ氏名  OF  U31-REC.
075300*    氏名
075400     MOVE  氏名  OF  U01-REC      TO  氏名  OF  U31-REC.
075500*    行政区保険者番号
075600*    行政区コード≠SPACEの場合
075700*▼ 案件 ID=72602 2017/11/07 政令市の場合の判定変更               171107SK
075800*     IF  行政区コード  OF  U01-REC  NOT =  SPACE
075900*        汎用フラグ='1'の場合
076000     IF  Ｐ汎用フラグ(06)(1:1) =  '1'
076100*▲ 案件 ID=72602 2017/11/07 政令市の場合の判定変更               171107SK
076200*            テーブル検索
076300             PERFORM  VARYING  IDX1  FROM  1  BY  1
076400                  UNTIL    IDX1  >  Ｗ行政区数  OR
076500                           ＷＫ行政区コード(IDX1)
076600                             =  行政区コード  OF U01-REC
076700                 CONTINUE
076800             END-PERFORM
076900             IF  IDX1  <=  Ｗ行政区数
077000                 MOVE  ＷＫ表示用保険者番号(IDX1)
077100                   TO  行政区保険者番号  OF  U31-REC
077200             END-IF
077300*▼ 案件 ID=72602 2017/11/07 政令市の場合の判定変更               171107SK
077400*         ELSE
077500*             MOVE  Ｐ固定値－Ｘ(06) (1:8)
077600*               TO  行政区保険者番号  OF  U31-REC
077700*         END-IF
077800*▲ 案件 ID=72602 2017/11/07 政令市の場合の判定変更               171107SK
077900*    行政区名
078000         MOVE  NC'行政区　'       TO  行政区名  OF  U31-REC (1:4)
078100         INITIALIZE               PARM-PCBNVDIV
078200         MOVE  '1'                TO  Ｐ索引パターン
078300         MOVE  行政区コード  OF  U01-REC
078400                                  TO  Ｐ行政区コード
078500         CALL  PCBNVDIV            USING  PARM-PCBNVDIV
078600         IF  Ｐリターンコード  OF  PARM-PCBNVDIV  =  ZERO
078700             MOVE  Ｐ行政区名称略 TO  行政区名  OF  U31-REC (5:16)
078800             IF  Ｐ管理行政区コード (1:3)
078900                   NOT =  行政区コード  OF  U01-REC
079000                 MOVE  Ｐ管理行政区コード (1:3)
079100                                  TO  行政区コード  OF  U31-REC
079200             END-IF
079300         ELSE
079400             DISPLAY  '*** PCBNVDIVU ***'
079500             DISPLAY  '行政区名称取得エラー'
079600             DISPLAY  '　リターンコード　＝　'  Ｐリターンコード
079700                                                OF  PARM-PCBNVDIV
079800                      '　行政区コード　＝　'    Ｐ行政区コード
079900                      '　索引パターン　＝　'    Ｐ索引パターン
080000         END-IF
080100*▼ 案件 ID=72602 2017/11/07 政令市の場合の判定変更               171107SK
080200     ELSE
080300         MOVE  SPACE  TO  行政区コード  OF  U31-REC
080400         MOVE  Ｐ固定値－Ｘ(06) (1:8)
080500           TO  行政区保険者番号  OF  U31-REC
080600*▲ 案件 ID=72602 2017/11/07 政令市の場合の判定変更               171107SK
080700     END-IF.
080800*    エラー内容コード
080900     MOVE  ＷメッセージＮｏ       TO  エラー内容コード
081000                                      OF  U31-REC.
081100*    エラー内容
081200     MOVE  Ｗメッセージ内容       TO  エラー内容
081300                                      OF  U31-REC.
081400*    取込日時
081500     MOVE  Ｗ処理日時             TO  取込日時  OF  U31-REC.
081600*
081700     MOVE  'WRITE'                TO  KAMA026A-SYORI-U31.
081800     CALL  'KAMA026A'          USING  PARM-KAMA026-U31
081900                                      U31-REC.
082000*
082100 U31-WRITE-RTN-EXIT.
082200     EXIT.
082300*
082400******************************************************************
082500* Level (3.0.0)  終      了      処      理                      *
082600******************************************************************
082700 TERM-RTN                         SECTION.
082800 TERM-START.
082900*
083000     EXEC SQL COMMIT END-EXEC.
083100     EXEC SQL DISCONNECT ALL END-EXEC.
083200*
083300     MOVE  'CLOSE'                TO  KAMA021A-SYORI-U01.
083400     CALL  'KAMA021A'          USING  PARM-KAMA021-U01.
083500*
083600     MOVE  'CLOSE'                TO  KAMA021A-SYORI-U42.
083700     CALL  'KAMA021A'          USING  PARM-KAMA021-U42.
083800*
083900     MOVE  'CLOSE'                TO  KAMA026A-SYORI-U30.
084000     CALL  'KAMA026A'          USING  PARM-KAMA026-U30.
084100*
084200     MOVE  'CLOSE'                TO  KAMA026A-SYORI-U31.
084300     CALL  'KAMA026A'          USING  PARM-KAMA026-U31.
084400*-----------------------------*
084500* プログラム終了メッセージ表示*
084600*-----------------------------*
084700*
084800     INITIALIZE    WRHDZ01U-PARM.
084900     MOVE  PID                    TO  WRHDPZ01-PID.
085000     MOVE  '1'                    TO  WRHDPZ01-KBN.
085100     MOVE  'U01'                  TO  WRHDPZ01-FILE.
085200     MOVE  KAMA021A-CNT-U01       TO  U01-CNT.
085300     MOVE  U01-CNT                TO  WRHDPZ01-CNT.
085400     CALL  'NVHDZ01U'          USING  WRHDZ01U-PARM.
085500*
085600     INITIALIZE    WRHDZ01U-PARM.
085700     MOVE  PID                    TO  WRHDPZ01-PID.
085800     MOVE  '1'                    TO  WRHDPZ01-KBN.
085900     MOVE  'U42'                  TO  WRHDPZ01-FILE.
086000     MOVE  KAMA021A-CNT-U42       TO  U42-CNT.
086100     MOVE  U42-CNT                TO  WRHDPZ01-CNT.
086200     CALL  'NVHDZ01U'          USING  WRHDZ01U-PARM.
086300*
086400     INITIALIZE    WRHDZ01U-PARM.
086500     MOVE  PID                    TO  WRHDPZ01-PID.
086600     MOVE  '2'                    TO  WRHDPZ01-KBN.
086700     MOVE  'INSERT'               TO  WRHDPZ01-FILE.
086800     MOVE   CNT-INS               TO  WRHDPZ01-CNT.
086900     CALL  'NVHDZ01U'          USING  WRHDZ01U-PARM.
087000*
087100     INITIALIZE    WRHDZ01U-PARM.
087200     MOVE  PID                    TO  WRHDPZ01-PID.
087300     MOVE  '2'                    TO  WRHDPZ01-KBN.
087400     MOVE  'UPDATE'               TO  WRHDPZ01-FILE.
087500     MOVE   CNT-UPD               TO  WRHDPZ01-CNT.
087600     CALL  'NVHDZ01U'          USING  WRHDZ01U-PARM.
087700*
087800     INITIALIZE    WRHDZ01U-PARM.
087900     MOVE  PID                    TO  WRHDPZ01-PID.
088000     MOVE  '2'                    TO  WRHDPZ01-KBN.
088100     MOVE  'U30'                  TO  WRHDPZ01-FILE.
088200     MOVE  KAMA026A-CNT-U30       TO  U30-CNT.
088300     MOVE  U30-CNT                TO  WRHDPZ01-CNT.
088400     CALL  'NVHDZ01U'          USING  WRHDZ01U-PARM.
088500*
088600     INITIALIZE    WRHDZ01U-PARM.
088700     MOVE  PID                    TO  WRHDPZ01-PID.
088800     MOVE  '2'                    TO  WRHDPZ01-KBN.
088900     MOVE  'U31'                  TO  WRHDPZ01-FILE.
089000     MOVE  KAMA026A-CNT-U31       TO  U31-CNT.
089100     MOVE  U31-CNT                TO  WRHDPZ01-CNT.
089200     CALL  'NVHDZ01U'          USING  WRHDZ01U-PARM.
089300*
089400     INITIALIZE    WRHDZ01U-PARM.
089500     MOVE  PID                    TO  WRHDPZ01-PID.
089600     MOVE  '2'                    TO  WRHDPZ01-KBN.
089700     MOVE  'ERRKENSU'             TO  WRHDPZ01-FILE.
089800     MOVE  CNT-ERR-REC            TO  WRHDPZ01-CNT.
089900     CALL  'NVHDZ01U'          USING  WRHDZ01U-PARM.
090000*
090100     INITIALIZE    WRHDZ01U-PARM.
090200     MOVE  PID                    TO  WRHDPZ01-PID.
090300     MOVE  '9'                    TO  WRHDPZ01-KBN.
090400     CALL  'NVHDZ01U'          USING  WRHDZ01U-PARM.
090500*
090600 TERM-EXIT.
090700     EXIT.
090800******************************************************************
090900* Level (4.0.0)   異常終了処理                                   *
091000******************************************************************
091100 SQL-ERR-RTN                      SECTION.
091200 SQL-ERR-START.
091300*
091400     DISPLAY 'SQLMSG   = '      SQLMSG    UPON SYSOUT.
091500     DISPLAY 'SQLSTATE = '      SQLSTATE  UPON SYSOUT.
091600*
091700     EXEC SQL ROLLBACK  END-EXEC.
091800     EXEC SQL DISCONNECT ALL END-EXEC.
091900*
092000     MOVE   'CLOSE'               TO  KAMA021A-SYORI-U01.
092100     CALL   'KAMA021A'            USING  PARM-KAMA021-U01.
092200*
092300     MOVE   'CLOSE'               TO  KAMA021A-SYORI-U42.
092400     CALL   'KAMA021A'            USING  PARM-KAMA021-U42.
092500*
092600     MOVE   'CLOSE'               TO  KAMA026A-SYORI-U30.
092700     CALL   'KAMA026A'            USING  PARM-KAMA026-U30.
092800*
092900     MOVE   'CLOSE'               TO  KAMA026A-SYORI-U31.
093000     CALL   'KAMA026A'            USING  PARM-KAMA026-U31.
093100*--------------------------------------------
093200*  異常終了　　メッセージ表示処理
093300*--------------------------------------------
093400     INITIALIZE    WRHDZ01U-PARM.
093500     MOVE  PID                    TO  WRHDPZ01-PID.
093600     MOVE  'E'                    TO  WRHDPZ01-KBN.
093700     MOVE  'SQL'                  TO  WRHDPZ01-CNT.
093800     CALL  'NVHDZ01U'             USING  WRHDZ01U-PARM.
093900*
094000     CALL  'ABENDU'.
094100*
094200 SQL-ERR-EXIT.
094300     EXIT.
094400*
094500*****************************************************************
094600* Ｐｒｏｇｒａｍ  Ｅｎｄ                                        *
094700*****************************************************************
