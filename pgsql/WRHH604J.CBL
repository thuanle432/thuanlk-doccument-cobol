000100 IDENTIFICATION                   DIVISION.
000200   PROGRAM-ID.                    WRHH604J.
000300*****************************************************************
000400*
000500*  業務名        ：WebRings  国民健康保険
000600*
000700*  顧客名        ：共通
000800*
000900*  プロセス名    ：被保険者証履歴編集処理
001000*
001100*  処理名        ：高齢受給者証履歴編集
001200*
001300*  処理概要      ：H前期高齢者月別資格テーブルを入力に
001400*                  高齢受給者証の証発行履歴編集ファイル
001500*                  （WRHHT326.CPY）を作成する。
001600*
001700*  実行時コード系：SJIS
001800*
001900*  文字コード    ：ﾃﾞｰﾀ半角英数項目：SJIS
002000*
002100*  作成者        ：INES / 金石東
002200*
002300*  案件ID        ：77309
002400*
002500*  作成日        ：2024.10.22
002600*
002700*  修正履歴      ：
002800*    修正年月日  -----------------  修正内容  -------------------
002900*
003000*****************************************************************
003100*****************************************************************
003200 ENVIRONMENT                      DIVISION.
003300*****************************************************************
003400*================================================================
003500 CONFIGURATION                    SECTION.
003600*================================================================
003700 SOURCE-COMPUTER.                 F-PC
003800*DBG*           WITH  DEBUGGING  MODE
003900     .
004000 OBJECT-COMPUTER.                 F-PC.
004100*
004200*================================================================
004300 INPUT-OUTPUT                     SECTION.
004400*================================================================
004500 FILE-CONTROL.
004600*****************************************************************
004700 DATA                             DIVISION.
004800*****************************************************************
004900*================================================================
005000 FILE                             SECTION.
005100*================================================================
005200*================================================================
005300 WORKING-STORAGE                  SECTION.
005400*================================================================
005500*----------------------------------------------------------------*
005600* 入出力定義領域
005700*----------------------------------------------------------------*
005800*  （ＯＵＴ）
005900   01  PARM-KAMA026A-U30.
006000     03  KAMA026A-DD-U30               PIC  X(08) VALUE  SPACE.
006100     03  KAMA026A-SYORI-U30            PIC  X(08) VALUE  SPACE.
006200     03  KAMA026A-RL-U30               PIC S9(09) COMP-5.
006300     03  KAMA026A-BL-U30               PIC S9(09) COMP-5.
006400     03  KAMA026A-CNT-U30              PIC S9(09) COMP-5.
006500*
006600   01  U30-REC.
006700       COPY  WRHHT326.
006800*
006900*----------------------------------------------------------------
007000*    ワーク領域
007100*----------------------------------------------------------------
007200   01  ワーク領域.
007300     03  IDX                           PIC  9(002).
007400     03  IDY                           PIC  9(002).
007500     03  IDZ                           PIC  9(002).
007600     03  PARM-JUMINCD-STA              PIC  9(002).
007700     03  Ｗ負担割合検索                PIC  X(001)  VALUE SPACE.
007800       88  Ｗ負担割合該当あり                       VALUE 'Y'.
007900       88  Ｗ負担割合該当なし                       VALUE 'N'.
008000     03  Ｗその他コード                PIC  X(001).
008100     03  Ｗ対象フラグ                  PIC  X(001).
008200*
008300     03  Ｗ対象年度９                  PIC  9(004).
008400     03  Ｗ対象年度Ｘ  REDEFINES  Ｗ対象年度９
008500                                       PIC  X(004).
008600     03  Ｗ対象月度９                  PIC  9(002).
008700     03  Ｗ対象月度Ｘ  REDEFINES  Ｗ対象月度９
008800                                       PIC  X(002).
008900     03  Ｗ対象日度９                  PIC  9(002).
009000     03  Ｗ対象日度Ｘ  REDEFINES  Ｗ対象日度９
009100                                       PIC  X(002).
009200     03  Ｗ住民コード９                PIC  9(010).
009300     03  Ｗ住民コードＸ  REDEFINES  Ｗ住民コード９
009400                                       PIC  X(010).
009500*
009600   01  Ｗ前期高齢月別資格Ｔ.
009700     03  Ｗ前期高齢月別資格テーブル    OCCURS  12.
009800       05  Ｗ前期高齢月別資格変換前    PIC  X(002).
009900       05  Ｗ前期高齢月別資格変換後    PIC  X(001).
010000*
010100   01  Ｗ高齢受給者証履歴作成Ｔ.
010200     03  Ｗ高齢受給者証履歴テーブル    OCCURS 12.
010300       05  Ｗ履歴月別資格              PIC X(002).
010400       05  Ｗ履歴資格取得年月日        PIC X(008).
010500       05  Ｗ履歴資格喪失年月日        PIC X(008).
010600     03  Ｗ高齢受給者証履歴数          PIC 9(002).
010700*
010800   01  Ｗ負担割合情報.
010900     03  Ｗ負担割合テーブル            OCCURS  50
011000                                       INDEXED  BY  IDX-T.
011100       05  Ｗ負担割合コード            PIC  X(010).
011200       05  Ｗ負担割合コード内容        PIC  N(010).
011300       05  Ｗ負担割合その他コード      PIC  X(010).
011400       05  Ｗ負担割合その他内容        PIC  N(010).
011500*
011600   01  カウント領域.
011700     03  ＴＢＬ件数                    PIC  9(009) VALUE ZERO.
011800     03  出力件数                      PIC  9(009) VALUE ZERO.
011900*----------------------------------------------------------------
012000* 指定年月日の末月を返す
012100*----------------------------------------------------------------
012200   01  PARM-KAMG421.
012300     03  KAMG421-IN.
012400       05  KAMG421-IN-NEN              PIC  9(004).
012500       05  KAMG421-IN-TSUKI            PIC  9(002).
012600     03  KAMG421-OUT                   PIC  9(002).
012700*
012800   77  Ｗ終了フラグ                    PIC  X(03) VALUE LOW-VALUE.
012900*
013000*----------------------------------------------------------------
013100*    ホスト変数  定義
013200*----------------------------------------------------------------
013300      EXEC SQL BEGIN DECLARE SECTION END-EXEC.
013400*
013500   01  Ｈ対象年度                      PIC  X(004).
013600
013700   01  Ｈ前期高齢者月別資格.
013800     03  Ｈ記号番号                    PIC  X(011).
013900     03  Ｈ住民コード                  PIC S9(010).
014000     03  Ｈ年度                        PIC  X(004).
014100     03  Ｈ年度Ｒ       REDEFINES      Ｈ年度.
014200       05  Ｈ年度９                    PIC  9(004).
014300     03  Ｈ履歴シーケンス              PIC S9(005).
014400     03  Ｈ前期高齢者月別資格８        PIC  X(002).
014500     03  Ｈ前期高齢者月別資格９        PIC  X(002).
014600     03  Ｈ前期高齢者月別資格１０      PIC  X(002).
014700     03  Ｈ前期高齢者月別資格１１      PIC  X(002).
014800     03  Ｈ前期高齢者月別資格１２      PIC  X(002).
014900     03  Ｈ前期高齢者月別資格１        PIC  X(002).
015000     03  Ｈ前期高齢者月別資格２        PIC  X(002).
015100     03  Ｈ前期高齢者月別資格３        PIC  X(002).
015200     03  Ｈ前期高齢者月別資格４        PIC  X(002).
015300     03  Ｈ前期高齢者月別資格５        PIC  X(002).
015400     03  Ｈ前期高齢者月別資格６        PIC  X(002).
015500     03  Ｈ前期高齢者月別資格７        PIC  X(002).
015600     03  Ｈ適用年月日                  PIC  X(008).
015700     03  Ｈ更新年月日                  PIC  X(008).
015800   01  Ｈ国保共通コード情報.
015900     03  Ｈ切替指定年月日              PIC  X(008).
016000     03  Ｈ切替指定年月日Ｒ REDEFINES  Ｈ切替指定年月日.
016100       05  Ｈ切替指定年                PIC  9(004).
016200       05  Ｈ切替指定月                PIC  9(002).
016300       05  Ｈ切替指定日                PIC  9(002).
016400   01  Ｈ負担割合情報.
016500     03  Ｈ負担割合テーブル.
016600       05  Ｈ負担割合コード            PIC  X(010)  OCCURS  50.
016700       05  Ｈ負担割合コード内容        PIC  N(010)  OCCURS  50.
016800       05  Ｈ負担割合その他コード      PIC  X(010)  OCCURS  50.
016900       05  Ｈ負担割合その他内容        PIC  N(010)  OCCURS  50.
017000     03  Ｈ負担割合テーブル件数        PIC S9(004).
017100*
017200*  ＳＱＬシステム変数
017300     COPY WRHDSQL.
017400*
017500   01  STMVAR                          PIC  X(4096).
017600*
017700*  SERVER パラメータ
017800   01  W-ACCEPT.
017900     03  H-KOKUHO                      PIC  X(003).
018000     03  H-KOKUHO-ADR                  PIC  X(015).
018100*
018200      EXEC SQL END   DECLARE SECTION END-EXEC.
018300*
018400  LINKAGE                         SECTION.
018500   01  PARA-AREA.
018600     03  PARM-LEN                      PIC S9(004) COMP.
018700     03  PARM-KADB-CONN                PIC  X(015).
018800     03  PARM-FIL1                     PIC  X(001).
018900     03  PARM-JUMINCD-KETA             PIC  9(002).
019000*
019100/****************************************************************
019200 PROCEDURE                        DIVISION
019300                                  USING   PARA-AREA.
019400*****************************************************************
019500*================================================================
019600 ベース処理                       SECTION.
019700*================================================================
019800 ベース処理−開始.
019900*
020000     PERFORM  前処理.
020100*
020200     PERFORM  メインループ処理
020300       UNTIL  Ｗ終了フラグ  =  HIGH-VALUE.
020400*
020500     PERFORM  後処理.
020600*
020700 ベース処理−終了.
020800     STOP RUN.
020900*================================================================
021000 前処理                           SECTION.
021100*================================================================
021200 HAJIME.
021300*
021400*------------------------------*
021500* プログラム開始メッセージ表示 *
021600*------------------------------*
021700*
021800     DISPLAY '*** WRHH604J   START ***'   UPON SYSOUT.
021900*
022000     PERFORM  実行パラメータチェック処理.
022100*
022200     MOVE   'U30'                 TO  KAMA026A-DD-U30.
022300     MOVE   'OPEN'                TO  KAMA026A-SYORI-U30.
022400     COMPUTE KAMA026A-RL-U30   =   FUNCTION LENG(U30-REC)
022500     CALL   'KAMA026A'            USING  PARM-KAMA026A-U30.
022600*
022700     PERFORM  ＤＢ接続処理.
022800*
022900     PERFORM  切替指定日情報取得処理.
023000*
023100     PERFORM  負担割合情報取得処理.
023200*
023300     PERFORM  前期高齢者カーソル定義処理.
023400*
023500     PERFORM  前期高齢者カーソルオープン処理.
023600*
023700     PERFORM  前期高齢者カーソル読込処理.
023800*
023900 OWARI.
024000     EXIT.
024100*================================================================
024200 メインループ処理                 SECTION.
024300*================================================================
024400 HAJIME.
024500*
024600     PERFORM  前期高齢月別資格変換処理.
024700*
024800     PERFORM  高齢受給者証履歴作成処理.
024900*
025000     PERFORM  高齢受給者証履歴出力制御処理.
025100*
025200     PERFORM  前期高齢者カーソル読込処理.
025300*
025400 OWARI.
025500     EXIT.
025600*================================================================
025700 後処理                           SECTION.
025800*================================================================
025900 HAJIME.
026000*
026100* ＤＢ切断
026200*
026300     EXEC SQL COMMIT END-EXEC.
026400     EXEC SQL DISCONNECT ALL END-EXEC.
026500*
026600     MOVE   'CLOSE'               TO  KAMA026A-SYORI-U30.
026700     CALL   'KAMA026A'            USING  PARM-KAMA026A-U30.
026800*
026900*-----------------------------*
027000* プログラム終了メッセージ表示*
027100*-----------------------------*
027200*
027300     DISPLAY '***(INPUT) DB :H前期高齢者月別資格テーブル ==> '
027400                                     ＴＢＬ件数  UPON SYSOUT.
027500     DISPLAY '***(OUTPUT)U30:高齢受給者証履歴ファイル　　==> '
027600                                     出力件数    UPON SYSOUT.
027700     DISPLAY ' '                                 UPON SYSOUT.
027800     DISPLAY '***  WRHH604J END  ***'            UPON SYSOUT.
027900*
028000 OWARI.
028100     EXIT.
028200*================================================================
028300 実行パラメータチェック処理       SECTION.
028400*================================================================
028500 HAJIME.
028600*
028700     DISPLAY '***  国保ＤＢ切替接続情報 = ' PARM-KADB-CONN
028800                                            UPON SYSOUT.
028900*
029000     DISPLAY '***  住民コードの桁数     = ' PARM-JUMINCD-KETA
029100                                            UPON SYSOUT.
029200*
029300     IF  PARM-JUMINCD-KETA        IS NOT NUMERIC  OR
029400         PARM-JUMINCD-KETA        =  ZERO  OR
029500         PARM-JUMINCD-KETA        >  10
029600         MOVE  10                 TO  PARM-JUMINCD-KETA
029700     END-IF.
029800     COMPUTE  PARM-JUMINCD-STA    =  10 - PARM-JUMINCD-KETA + 1.
029900*
030000 OWARI.
030100     EXIT.
030200*================================================================
030300 ＤＢ接続処理                     SECTION.
030400*================================================================
030500 HAJIME.
030600*
030700     MOVE   PARM-KADB-CONN        TO  H-KOKUHO-ADR.
030800*
030900     MOVE  'DB1'                  TO  H-KOKUHO.
031000     EXEC   SQL
031100         CONNECT TO :H-KOKUHO-ADR AS :H-KOKUHO
031200     END-EXEC.
031300*
031400     EVALUATE  SQLSTATE
031500       WHEN "00000"
031600       WHEN "01000"
031700           DISPLAY
031800           "国保DB接続に成功（" SQLSTATE " " H-KOKUHO-ADR " )"
031900       WHEN OTHER
032000           DISPLAY
032100           "国保DB接続に失敗（" SQLSTATE " " H-KOKUHO-ADR " )"
032200           CALL  'ABEND'
032300     END-EVALUATE.
032400*
032500 OWARI.
032600     EXIT.
032700*================================================================
032800 切替指定日情報取得処理           SECTION.
032900*================================================================
033000 HAJIME.
033100*
033200     EXEC SQL
033300       SELECT ISNULL(その他内容,'99999999') INTO :Ｈ切替指定年月日
033400         FROM X国保共通コード
033500        WHERE 処理区分 = 9
033600          AND 処理番号 = 9212
033700          AND コード   = '01'
033800      END-EXEC.
033900* 実行結果確認
034000     EVALUATE SQLSTATE
034100        WHEN "00000"
034200              CONTINUE
034300        WHEN OTHER
034400              DISPLAY
034500              "（X国保共通コード）切替指定日の取得に失敗しました"
034600              DISPLAY
034700              " 処理区分 = 9"
034800              " 処理番号 = 9212"
034900              " コード   = 01"
035000              PERFORM  ＳＱＬエラー処理
035100     END-EVALUATE.
035200*
035300     DISPLAY " "    UPON SYSOUT.
035400     DISPLAY "*** 切替指定日 = " Ｈ切替指定年月日    UPON SYSOUT.
035500     DISPLAY " "    UPON SYSOUT.
035600*
035700 OWARI.
035800     EXIT.
035900*================================================================
036000 負担割合情報取得処理             SECTION.
036100*================================================================
036200 HAJIME.
036300*
036400     INITIALIZE   Ｈ負担割合情報.
036500*
036600* 負担割合情報件数
036700*
036800     EXEC SQL
036900       SELECT COUNT(コード) INTO :Ｈ負担割合テーブル件数
037000         FROM X国保共通コード
037100        WHERE 処理区分 = 1
037200          AND 処理番号 = 64
037300          AND 使用有無 = 1
037400      END-EXEC.
037500*
037600     IF  Ｈ負担割合テーブル件数   >   50
037700         DISPLAY
037800         "***  Ｈ負担割合テーブル件数オーバーエラー"
037900         "（先頭50件で処理継続します。）：件数 = "
038000         Ｈ負担割合テーブル件数
038100         MOVE  50                 TO  Ｈ負担割合テーブル件数
038200     END-IF.
038300*
038400* 負担割合情報取得
038500*
038600     EXEC SQL
038700       SELECT TOP 50 コード,コード内容,その他コード,その他内容
038800         INTO :Ｈ負担割合コード,:Ｈ負担割合コード内容,
038900              :Ｈ負担割合その他コード,:Ｈ負担割合その他内容
039000         FROM X国保共通コード
039100        WHERE 処理区分 = 1
039200          AND 処理番号 = 64
039300          AND 使用有無 = 1
039400        ORDER BY コード
039500      END-EXEC.
039600* 実行結果確認
039700     EVALUATE SQLSTATE
039800        WHEN "00000"
039900              CONTINUE
040000        WHEN OTHER
040100              DISPLAY
040200              "（X国保共通コード）負担割合情報取得に失敗しました"
040300              DISPLAY
040400              " 処理区分 = 1"
040500              " 処理番号 = 64"
040600              " コード   = 1"
040700              PERFORM  ＳＱＬエラー処理
040800     END-EVALUATE.
040900*
041000     PERFORM  VARYING  IDX  FROM  1  BY  1
041100       UNTIL  IDX   >   Ｈ負担割合テーブル件数
041200          OR  IDX   >   50
041300        MOVE  Ｈ負担割合コード(IDX)
041400          TO  Ｗ負担割合コード(IDX)
041500        MOVE  Ｈ負担割合コード内容(IDX)
041600          TO  Ｗ負担割合コード内容(IDX)
041700        MOVE  Ｈ負担割合その他コード(IDX)
041800          TO  Ｗ負担割合その他コード(IDX)
041900* ▽ B-000973
042000* 各顧客のDB環境は、負担割合コード = '0'（非該当）に対して、
042100* 負担割合その他コード = '7'（３割負担）で設定されている。
042200* 本来は非該当なので、負担割合その他コード = '0'とすべきであるが
042300* 既存への影響が不明なため、DBの定義はそのままとし、本プログラム
042400* 内で非該当と判断されるように個別対応する。
042500        IF  Ｈ負担割合コード(IDX)  = '0'
042600          MOVE  '0'   TO  Ｗ負担割合その他コード(IDX)
042700        END-IF
042800* △ B-000973
042900        MOVE  Ｈ負担割合その他内容(IDX)
043000          TO  Ｗ負担割合その他内容(IDX)
043100     END-PERFORM.
043200 OWARI.
043300     EXIT.
043400*================================================================
043500 前期高齢者カーソル定義処理       SECTION.
043600*================================================================
043700 HAJIME.
043800*
043900     IF    Ｈ切替指定月    >=     8
044000           MOVE   Ｈ切替指定年    TO  Ｗ対象年度９
044100     ELSE
044200           COMPUTE  Ｗ対象年度９  =  Ｈ切替指定年  -  1
044300     END-IF.
044400     MOVE  Ｗ対象年度Ｘ           TO  Ｈ対象年度.
044500*
044600     EXEC SQL
044700          DECLARE  CSR01  CURSOR  FOR
044800           SELECT
044900             記号番号
045000            ,住民コード
045100            ,年度
045200            ,履歴シーケンス
045300            ,ISNULL(前期高齢月別資格8,' ')
045400            ,ISNULL(前期高齢月別資格9,' ')
045500            ,ISNULL(前期高齢月別資格10,' ')
045600            ,ISNULL(前期高齢月別資格11,' ')
045700            ,ISNULL(前期高齢月別資格12,' ')
045800            ,ISNULL(前期高齢月別資格1,' ')
045900            ,ISNULL(前期高齢月別資格2,' ')
046000            ,ISNULL(前期高齢月別資格3,' ')
046100            ,ISNULL(前期高齢月別資格4,' ')
046200            ,ISNULL(前期高齢月別資格5,' ')
046300            ,ISNULL(前期高齢月別資格6,' ')
046400            ,ISNULL(前期高齢月別資格7,' ')
046500            ,ISNULL(CONVERT(CHAR(8),適用年月日, 112),'99999999')
046600                                               AS 適用年月日
046700            ,CONVERT(CHAR(8),更新年月日, 112)  AS 更新年月日
046800           FROM
046900            ( SELECT 
047000                 ROW_NUMBER() OVER (PARTITION BY 記号番号,
047100                                                 住民コード,年度
047200                 ORDER BY 記号番号,住民コード,年度,
047300                          履歴シーケンス desc) as RowNo,*
047400                FROM H前期高齢者月別資格 ) ZENKI
047500           WHERE RowNo = 1  AND 年度 >=  :Ｈ対象年度
047600     END-EXEC.
047700*
047800 OWARI.
047900     EXIT.
048000*================================================================
048100 前期高齢者カーソルオープン処理   SECTION.
048200*================================================================
048300 HAJIME.
048400*
048500     EXEC SQL  OPEN  CSR01   END-EXEC.
048600*
048700     EVALUATE  SQLSTATE
048800         WHEN "00000"
048900         WHEN "01S02"
049000               CONTINUE
049100         WHEN  OTHER
049200               DISPLAY
049300               NC"（前期高齢者月別資格）"
049400               NC"カーソルオープン処理の失敗です。"
049500               PERFORM  ＳＱＬエラー処理
049600     END-EVALUATE.
049700*
049800 OWARI.
049900     EXIT.
050000*================================================================
050100 前期高齢者カーソル読込処理       SECTION.
050200*================================================================
050300 HAJIME.
050400*
050500     EXEC SQL
050600          FETCH  CSR01    INTO    :Ｈ記号番号,
050700                                  :Ｈ住民コード,
050800                                  :Ｈ年度,
050900                                  :Ｈ履歴シーケンス,
051000                                  :Ｈ前期高齢者月別資格８,
051100                                  :Ｈ前期高齢者月別資格９,
051200                                  :Ｈ前期高齢者月別資格１０,
051300                                  :Ｈ前期高齢者月別資格１１,
051400                                  :Ｈ前期高齢者月別資格１２,
051500                                  :Ｈ前期高齢者月別資格１,
051600                                  :Ｈ前期高齢者月別資格２,
051700                                  :Ｈ前期高齢者月別資格３,
051800                                  :Ｈ前期高齢者月別資格４,
051900                                  :Ｈ前期高齢者月別資格５,
052000                                  :Ｈ前期高齢者月別資格６,
052100                                  :Ｈ前期高齢者月別資格７,
052200                                  :Ｈ適用年月日,
052300                                  :Ｈ更新年月日
052400     END-EXEC.
052500*
052600     EVALUATE  SQLSTATE
052700         WHEN "00000"
052800               PERFORM  前期高齢月別資格転送処理
052900               ADD   1            TO  ＴＢＬ件数
053000         WHEN "02000"
053100               MOVE  HIGH-VALUE   TO  Ｗ終了フラグ
053200               EXEC  SQL CLOSE    CSR01 END-EXEC
053300         WHEN  OTHER
053400               DISPLAY
053500                NC"（前期高齢者月別資格）"
053600                NC"読み込み処理に失敗しました"
053700               DISPLAY
053800                " 記号番号 = " Ｈ記号番号
053900                " 住民コード = " Ｈ住民コード
054000                " 年度 = " Ｈ年度
054100                " 履歴シーケンス = " Ｈ履歴シーケンス
054200               PERFORM  ＳＱＬエラー処理
054300     END-EVALUATE.
054400*
054500 OWARI.
054600     EXIT.
054700*================================================================
054800 前期高齢月別資格転送処理         SECTION.
054900*================================================================
055000 HAJIME.
055100*
055200     MOVE  Ｈ住民コード
055300       TO  Ｗ住民コード９.
055400     MOVE  Ｈ前期高齢者月別資格８
055500       TO  Ｗ前期高齢月別資格変換前(1).
055600     MOVE  Ｈ前期高齢者月別資格９
055700       TO  Ｗ前期高齢月別資格変換前(2).
055800     MOVE  Ｈ前期高齢者月別資格１０
055900       TO  Ｗ前期高齢月別資格変換前(3).
056000     MOVE  Ｈ前期高齢者月別資格１１
056100       TO  Ｗ前期高齢月別資格変換前(4).
056200     MOVE  Ｈ前期高齢者月別資格１２
056300       TO  Ｗ前期高齢月別資格変換前(5).
056400     MOVE  Ｈ前期高齢者月別資格１
056500       TO  Ｗ前期高齢月別資格変換前(6).
056600     MOVE  Ｈ前期高齢者月別資格２
056700       TO  Ｗ前期高齢月別資格変換前(7).
056800     MOVE  Ｈ前期高齢者月別資格３
056900       TO  Ｗ前期高齢月別資格変換前(8).
057000     MOVE  Ｈ前期高齢者月別資格４
057100       TO  Ｗ前期高齢月別資格変換前(9).
057200     MOVE  Ｈ前期高齢者月別資格５
057300       TO  Ｗ前期高齢月別資格変換前(10).
057400     MOVE  Ｈ前期高齢者月別資格６
057500       TO  Ｗ前期高齢月別資格変換前(11).
057600     MOVE  Ｈ前期高齢者月別資格７
057700       TO  Ｗ前期高齢月別資格変換前(12).
057800*
057900 OWARI.
058000     EXIT.
058100*================================================================
058200 前期高齢月別資格変換処理         SECTION.
058300*================================================================
058400 HAJIME.
058500*
058600     PERFORM  VARYING  IDX  FROM  1  BY  1  UNTIL  IDX  >  12
058700         SET   IDX-T              TO   1
058800         MOVE  SPACE              TO   Ｗその他コード
058900         SEARCH Ｗ負担割合テーブル
059000             AT END
059100                MOVE  'N'              TO  Ｗ負担割合検索
059200           WHEN Ｗ負担割合コード(IDX-T)  =
059300                Ｗ前期高齢月別資格変換前(IDX)
059400                MOVE  'Y'              TO  Ｗ負担割合検索
059500                MOVE  Ｗ負担割合その他コード(IDX-T)(1:1)
059600                  TO  Ｗその他コード
059700         END-SEARCH
059800*
059900         IF   Ｗ負担割合該当あり  AND
060000            ( Ｗその他コード  =  '2' OR '7' )
060100              MOVE  Ｗその他コード
060200                TO  Ｗ前期高齢月別資格変換後(IDX)
060300         ELSE
060400              MOVE  '0'
060500                TO  Ｗ前期高齢月別資格変換後(IDX)
060600         END-IF
060700     END-PERFORM.
060800*
060900     IF  Ｈ対象年度   =   Ｈ年度
061000        IF  Ｈ切替指定月    >=     8
061100            COMPUTE  IDY  =  Ｈ切替指定月  -  8
061200        ELSE
061300            COMPUTE  IDY  =  Ｈ切替指定月  +  4
061400        END-IF
061500        PERFORM  VARYING  IDX  FROM  1  BY  1  UNTIL  IDX  >  IDY
061600             MOVE  '0'           TO  Ｗ前期高齢月別資格変換後(IDX)
061700        END-PERFORM
061800     END-IF.
061900*
062000 OWARI.
062100     EXIT.
062200*================================================================
062300 高齢受給者証履歴作成処理         SECTION.
062400*================================================================
062500 HAJIME.
062600*
062700     INITIALIZE   Ｗ高齢受給者証履歴作成Ｔ.
062800     MOVE   SPACE                 TO   Ｗ対象フラグ.
062900     MOVE     1                   TO   IDY.
063000*
063100* IDX：カレント添字(比較先の月別資格)
063200* IDY：比較元の月別資格として履歴資格取得年月日を求める為の添字
063300* IDZ：履歴資格喪失年月日を求める為の添字
063400*
063500     PERFORM  VARYING  IDX  FROM  IDY  BY  1  UNTIL  IDX  >  12
063600          IF  IDY   =   IDX
063700              CONTINUE
063800          ELSE
063900              IF  Ｗ前期高齢月別資格変換後(IDY)  =  '0'
064000                  IF  IDX  =  12
064100                      IF  Ｗ前期高齢月別資格変換後(IDX) = '0'
064200                          CONTINUE
064300                      ELSE
064400                          MOVE   'Y'           TO   Ｗ対象フラグ
064500                          MOVE   IDX           TO   IDZ
064600                      END-IF
064700                  END-IF
064800                  MOVE   IDX               TO   IDY
064900              ELSE
065000                  IF  Ｗ対象フラグ  =  SPACE
065100                      MOVE   'Y'           TO   Ｗ対象フラグ
065200                      MOVE   IDY           TO   IDZ
065300                  END-IF
065400                  IF  Ｗ前期高齢月別資格変換後(IDY) =
065500                      Ｗ前期高齢月別資格変換後(IDX)
065600                      MOVE   'Y'           TO   Ｗ対象フラグ
065700                      MOVE   IDX           TO   IDZ
065800                  ELSE
065900                      IF  Ｗ対象フラグ  =  'Y'
066000                          PERFORM  高齢受給者証月別資格編集
066100                          MOVE   SPACE     TO   Ｗ対象フラグ
066200                      END-IF
066300                      IF  IDX  =  12
066400                          IF  Ｗ前期高齢月別資格変換後(IDX) = '0'
066500                              CONTINUE
066600                          ELSE
066700                              MOVE   'Y'       TO   Ｗ対象フラグ
066800                              MOVE   IDX       TO   IDZ
066900                          END-IF
067000                      END-IF
067100                      MOVE   IDX               TO   IDY
067200                  END-IF
067300              END-IF
067400          END-IF
067500     END-PERFORM.
067600     IF  Ｗ対象フラグ  =  'Y'
067700         PERFORM  高齢受給者証月別資格編集
067800     END-IF.
067900*
068000 OWARI.
068100     EXIT.
068200*================================================================
068300 高齢受給者証月別資格編集         SECTION.
068400*================================================================
068500 HAJIME.
068600*
068700     ADD     1                    TO   Ｗ高齢受給者証履歴数.
068800*
068900     MOVE    Ｗ前期高齢月別資格変換後(IDY)
069000       TO    Ｗ履歴月別資格(Ｗ高齢受給者証履歴数).
069100* 履歴資格取得年月日編集
069200     IF    IDY    <     6
069300           MOVE   Ｈ年度９        TO   Ｗ対象年度９
069400           COMPUTE  Ｗ対象月度９  =  IDY  +  7
069500     ELSE
069600           COMPUTE  Ｗ対象年度９  =  Ｈ年度９  +  1
069700           COMPUTE  Ｗ対象月度９  =  IDY  -  5
069800     END-IF.
069900     MOVE  Ｗ対象年度Ｘ
070000       TO  Ｗ履歴資格取得年月日(Ｗ高齢受給者証履歴数)(1:4).
070100     MOVE  Ｗ対象月度Ｘ
070200       TO  Ｗ履歴資格取得年月日(Ｗ高齢受給者証履歴数)(5:2).
070300     IF    Ｗ対象年度９   =   Ｈ切替指定年 AND
070400           Ｗ対象月度９   =   Ｈ切替指定月
070500           MOVE  Ｈ切替指定日     TO  Ｗ対象日度９
070600           MOVE  Ｗ対象日度Ｘ
070700             TO  Ｗ履歴資格取得年月日(Ｗ高齢受給者証履歴数)(7:2)
070800     ELSE
070900           MOVE  '01'
071000             TO  Ｗ履歴資格取得年月日(Ｗ高齢受給者証履歴数)(7:2)
071100     END-IF.
071200* 履歴資格喪失年月日編集
071300     IF    IDZ    <     6
071400           MOVE   Ｈ年度９        TO   Ｗ対象年度９
071500           COMPUTE  Ｗ対象月度９  =  IDZ  +  7
071600     ELSE
071700           COMPUTE  Ｗ対象年度９  =  Ｈ年度９  +  1
071800           COMPUTE  Ｗ対象月度９  =  IDZ  -  5
071900     END-IF.
072000*
072100     MOVE  Ｗ対象年度９           TO  KAMG421-IN-NEN.
072200     MOVE  Ｗ対象月度９           TO  KAMG421-IN-TSUKI.
072300     CALL  'KAMG421'           USING  PARM-KAMG421.
072400     IF    KAMG421-OUT   =   ZERO
072500           DISPLAY  '*** WRHH604J 月別末日算出に誤り：指定年月='
072600                     KAMG421-IN       UPON  SYSOUT
072700           PERFORM  ＡＢＥＮＤ処理
072800     ELSE
072900           MOVE  KAMG421-OUT      TO  Ｗ対象日度９
073000     END-IF
073100     MOVE  Ｗ対象年度Ｘ
073200       TO  Ｗ履歴資格喪失年月日(Ｗ高齢受給者証履歴数)(1:4).
073300     MOVE  Ｗ対象月度Ｘ
073400       TO  Ｗ履歴資格喪失年月日(Ｗ高齢受給者証履歴数)(5:2).
073500     MOVE  Ｗ対象日度Ｘ
073600       TO  Ｗ履歴資格喪失年月日(Ｗ高齢受給者証履歴数)(7:2).
073700*
073800 OWARI.
073900     EXIT.
074000*================================================================
074100 高齢受給者証履歴出力制御処理     SECTION.
074200*================================================================
074300 HAJIME.
074400*
074500     INITIALIZE       U30-REC.
074600*
074700     MOVE  Ｈ記号番号                 TO   記号番号.
074800     MOVE  Ｗ住民コードＸ(PARM-JUMINCD-STA:PARM-JUMINCD-KETA)
074900                                      TO   住民コード.
075000     MOVE  SPACE                      TO   余白１.
075100     MOVE  06                         TO   証区分.
075200     MOVE  ZERO                       TO   履歴種類番号.
075300     MOVE  ZERO                       TO   開始年月日.
075400     MOVE  ZERO                       TO   終了年月日.
075500     MOVE  SPACE                      TO   割合.
075600     MOVE  ZERO                       TO   長期入院該当年月日.
075700     MOVE  SPACE                      TO   自己負担限度額.
075800     MOVE  SPACE                      TO   回収区分.
075900     MOVE  ZERO                       TO   回収年月日.
076000     MOVE  SPACE                      TO   申請番号.
076100     MOVE  ZERO                       TO   ソートフラグ.
076200     MOVE  99999999                   TO   ソート用回収年月日.
076300     MOVE  999                        TO   履歴シーケンス.
076400     MOVE  SPACE                      TO   ソート用特定疾病区分.
076500     MOVE  SPACE                      TO   余白２.
076600*
076700     PERFORM  VARYING  IDX  FROM  1  BY  1
076800       UNTIL  IDX   >   Ｗ高齢受給者証履歴数
076900         PERFORM  高齢受給者証履歴レコード出力処理
077000     END-PERFORM.
077100*
077200 OWARI.
077300     EXIT.
077400*================================================================
077500 高齢受給者証履歴レコード出力処理 SECTION.
077600*================================================================
077700 HAJIME.
077800*
077900     MOVE  Ｗ履歴資格取得年月日(IDX)  TO   発行期日
078000                                           交付年月日.
078100     MOVE  Ｗ履歴資格喪失年月日(IDX)  TO   有効期限.
078200     MOVE  Ｗ履歴月別資格(IDX)        TO   区分１.
078300*
078400     MOVE    'WRITE'                  TO   KAMA026A-SYORI-U30.
078500     CALL    'KAMA026A'            USING   PARM-KAMA026A-U30
078600                                           U30-REC.
078700     ADD      1                       TO   出力件数.
078800*
078900 OWARI.
079000     EXIT.
079100*================================================================
079200 ＳＱＬエラー処理                 SECTION.
079300*================================================================
079400 HAJIME.
079500*
079600     DISPLAY 'SQLSTATE   = ' SQLSTATE  UPON  SYSOUT.
079700     DISPLAY 'SQLMSG     = ' SQLMSG    UPON  SYSOUT.
079800*
079900     PERFORM  ＡＢＥＮＤ処理.
080000*
080100 OWARI.
080200     EXIT.
080300*================================================================
080400 ＡＢＥＮＤ処理                   SECTION.
080500*================================================================
080600 HAJIME.
080700*
080800     EXEC  SQL ROLLBACK END-EXEC.
080900     EXEC  SQL DISCONNECT ALL END-EXEC.
081000*
081100* 各種ファイルクローズ
081200*
081300     MOVE   'CLOSE'               TO  KAMA026A-SYORI-U30.
081400     CALL   'KAMA026A'            USING  PARM-KAMA026A-U30.
081500*
081600* 異常終了 プログラムステータスセット
081700     CALL  'ABEND'.
081800*
081900 OWARI.
082000     EXIT.
082100****************************************************************
082200*              << WRHH604J PROGRAM END >>                      *
082300****************************************************************
082400 END PROGRAM WRHH604J.
