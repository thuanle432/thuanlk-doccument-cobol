       IDENTIFICATION                   DIVISION.                       
       PROGRAM-ID.                      DBSEQ01N.                       
      ******************************************************************
      *  業務名        ：ＷebRings  テスト                              
      *                                                                 
      *  顧客名        ：共通                                           
      *                                                                 
      *  プロセス名    ：テスト                                         
      *                                                                 
      *  処理名        ：T宛名テーブルダウンロード                      
      *                                                                 
      *  処理概要      ：T宛名テーブルからデータを抽出し、              
      *                  出力ファイルに出力する                         
      *                                                                 
      *  実行時コード系：SJIS                                           
      *                                                                 
      *  作成者        ：アイネス　トゥアン                             
      *                                                                 
      *  作成日        ：2025.02.12                                     
      *                                                                 
      *  案件ID        ：TEST1                                          
      *                                                                 
      *  修正履歴      ：                                               
      *    修正年月日  -----------------  修正内容  ------------------- 
      *                                                                 
      ******************************************************************
       ENVIRONMENT                      DIVISION.                       
       CONFIGURATION                    SECTION.                        
       INPUT-OUTPUT                     SECTION.                        
       FILE-CONTROL.                                                    
            SELECT U30-FILE             ASSIGN  TO  U30.                
      *
       DATA                             DIVISION.                       
       FILE                             SECTION.                        
      *----------------------------------------------------------------*
      * 出力ファイル
      *----------------------------------------------------------------*
       FD  U30-FILE.
       01  U30-REC.                                                     
         03  住民コード                 PIC  9(012).                    
         03  カナ名称                   PIC  X(100).                    
         03  名称                       PIC  N(100).                    
         03  生年月日－西暦             PIC  X(010).                    
         03  生年月日－邦暦             PIC  N(011).                    
         03  年齢                       PIC  9(003).                    
         03  FILLER                     PIC  X(032).                    
      *
       WORKING-STORAGE                  SECTION.                        
      *----------------------------------------------------------------*
      * ワーク領域
      *----------------------------------------------------------------*
      *
           EXEC SQL BEGIN DECLARE SECTION END-EXEC.                     
      *
       01  FETCH-RECORD.                                                
         03  住民コード                 PIC  9(012).                    
         03  カナ名称                   PIC  X(100).                    
         03  名称                       PIC  N(100).                    
         03  生年月日－西暦             PIC  X(010).                    
         03  生年月日－邦暦             PIC  N(011).                    
         03  年齢                       PIC  9(003).                    
         03  FILLER                     PIC  X(032).                    
      *
           EXEC SQL END DECLARE SECTION END-EXEC.                       
      *
       01  WK-U30-CNT                   PIC  9(006).                    
      *
       01  WK-DATE-COMPILE              PIC  9(008).                    
       01  WK-DATE-TIME                 PIC  X(017).                    
      *
       01  WK-OEF                       PIC  X(003) VALUE LOW-VALUE.    

       PROCEDURE DIVISION.                                              
      ******************************************************************
      * メイン処理                                                     *
      ******************************************************************
       MAIN-SHORI.                      SECTION.                        
         HAJIME.                                                        
      *
           PERFORM ZEN-SHORI.                                           
      *
           PERFORM SHU-SHORI UNTIL WK-OEF = HIGH-VALUE.                 
      *
           PERFORM ATO-SHORI.                                           
      *
         OWARI.                                                         
           STOP RUN.                                                    
      ******************************************************************
      * 前処理                                                         *
      ******************************************************************
       ZEN-SHORI                        SECTION.                        
         HAJIME.                                                        
      *
           MOVE FUNCTION WHEN-COMPILED(1:8)  TO WK-DATE-COMPILE.        
           DISPLAY '*** DBSEQ01N START VER.'                            
                   WK-DATE-COMPILE ' ***'    UPON SYSOUT.               
      *
           MOVE FUNCTION CURRENT-DATE        TO WK-DATE-TIME.           
           DISPLAY '(DBSEQ01N)'                                         
                   WK-DATE-TIME(1:4) '/'                                
                   WK-DATE-TIME(5:2) '/'                                
                   WK-DATE-TIME(7:2) ' '                                
                   WK-DATE-TIME(9:2) ':'                                
                   WK-DATE-TIME(11:2)':'                                
                   WK-DATE-TIME(13:2)        UPON  SYSOUT.              
      *
           OPEN OUTPUT U30-FILE                                         
      *
           PERFORM DB-CONNECT.                                          
      *
           PERFORM OPENS-CURSOR.                                        
      *
           PERFORM FETCH-CURSOR.                                        
      *
         OWARI.                                                         
           EXIT.                                                        
      ******************************************************************
      * 主処理                                                         *
      ******************************************************************
       SHU-SHORI                        SECTION.                        
         HAJIME.                                                        
      *
           PERFORM WRITE-OUTPUT.                                        
      *
           PERFORM FETCH-CURSOR.                                        
      *
         OWARI.                                                         
           EXIT.                                                        
      ******************************************************************
      * 後処理                                                         *
      ******************************************************************
       ATO-SHORI                        SECTION.                        
         HAJIME.                                                        
      *
           EXEC SQL                                                     
               COMMIT WORK                                              
           END-EXEC.                                                    
           EXEC SQL                                                     
               DISCONNECT ALL                                           
           EXEC-END.                                                    
      *
           CLOSE U30-FILE.                                              
      *
           DISPLAY '(OUTPUT) U30：出力ファイル = '                      
                   WK-U30-CNT '件'      UPON  SYSOUT.                   
      *
           MOVE FUNCTION CURRENT-DATE   TO    WK-DATE-TIME.             
           DISPLAY '(DBSEQ01N)'                                         
                   WK-DATE-TIME(1:4) '/'                                
                   WK-DATE-TIME(5:2) '/'                                
                   WK-DATE-TIME(7:2) ' '                                
                   WK-DATE-TIME(9:2) ':'                                
                   WK-DATE-TIME(11:2)':'                                
                   WK-DATE-TIME(13:2)   UPON  SYSOUT.                   
      *
           DISPLAY '*** DBSEQ01N END ***'.                              
      *
         OWARI.                                                         
           EXIT.                                                        
      ******************************************************************
      * ファイル出力処理                                               *
      ******************************************************************
       WRITE-OUTPUT                     SECTION.                        
         HAJIME.                                                        
      *
           WRITE  U30-RECORD.                                           
      *
         OWARI.                                                         
           EXIT.                                                        
      ******************************************************************
      * ＤＢ接続処理                                                   *
      ******************************************************************
       DB-CONNECT                       SECTION.                        
         HAJIME.                                                        
         OWARI.                                                         
           EXIT.                                                        
      ******************************************************************
      * カーソルオープン処理                                           *
      ******************************************************************
       OPENS-CURSOR                     SECTION.                        
         HAJIME.                                                        
      *
           EXEC SQL                                                     
               DECLARE DATA CURSOR FOR                                  
               SELECT 住民コード,                                       
               LEFT(カナ名称 + ' ' + カナ名称2, 100 ) AS カナ名称,      
               LEFT(名称 + ' ' + 名称2, 100) AS 名称,                   
               REPLACE(CONVERT(DATE, 生年月日, 120),'-','/')            
                                                   AS 生年月日_西暦     
               FROM T宛名                                               
           END-EXEC.                                                    
      *
           EXEC SQL                                                     
               OPEN DATA                                                
           END-EXEC.                                                    
      *
           EVALUATE  SQLSTATE                                           
               WHEN "00000"                                             
               WHEN "10S02"                                             
                 CONTINUE                                               
               WHEN OTHER                                               
                 DISPLAY                                                
                   "（ H情報集約_個人基本）読み込み処理に失敗しました " 
                   PERFORM  ABEN-HANDLING                               
           END-EVALUATE.                                                
      *
         OWARI.                                                         
           EXIT.                                                        
      ******************************************************************
      * カーソルフェッチ処理                                           *
      ******************************************************************
       FETCH-CURSOR                     SECTION.                        
         HAJIME.                                                        
      *
           EXEC SQL                                                     
               FETCH DATA INTO          :住民コード  OF FETCH-RECORD,   
                                        :カナ名称    OF FETCH-RECORD,   
                                        :名称        OF FETCH-RECORD,   
                                        :生年月日－西暦                 
                                                     OF FETCH-RECORD    
           END-EXEC.                                                    
      *
           EVALUATE  SQLSTATE                                           
               WHEN "00000"                                             
                 ADD 1             TO WK-U30-CNT                        
               WHEN "02000"                                             
                 MOVE WK-OEF       TO HIGH-VALUE                        
               WHEN OTHER                                               
                 DISPLAY                                                
                   "（ H情報集約_個人基本）読み込み処理に失敗しました " 
                   PERFORM  ABEN-HANDLING                               
           END-EVALUATE                                                 
      *
         OWARI.                                                         
           EXIT.                                                        
      ******************************************************************
      * ＡＢＥＮＤ処理                                                 *
      ******************************************************************
       ABEN-HANDLING                    SECTION.                        
         HAJIME.                                                        
      *
           DISPLAY 'SQLMSG     = ' SQLMSG    UPON SYSOUT.               
           DISPLAY 'SQLSTATE   = ' SQLSTATE  UPON SYSOUT.               
      *
           EXEC SQL                                                     
               ROLLBACK                                                 
           END-EXEC.                                                    
      *
           EXEC SQL                                                     
               DISCONNECT ALL                                           
           END-EXEC.                                                    
      *
           CLOSE U30-FILE.                                              
      *
           CALL 'ABENDU'.                                               
      *
         OWARI.                                                         
           EXIT.                                                        
      ******************************************************************
      *****    DBSEQ01N  PROGRAM  END                              *****
       END PROGRAM DBSEQ01N.                                            
      