000010 IDENTIFICATION                   DIVISION.                               
000020 PROGRAM-ID.                      WRHN2210.                               
000030****************************************************************          
000040*?@?@?@?@?@?@?@?@?@?@?@?@?@?@?@?@?@?@?@?@?@?@?@?@?@?@?@?@?@?@?@*          
000050*?@?@?@?@?@?@??q???@?@?@?@?@?F?@????@?@?@?@?@?@?@?@?@?@?@?@?@*          
000060*?@?@?@?@?@?@?@?@?@?@?@?@?@?@?@?@?@?@?@?@?@?@?@?@?@?@?@?@?@?@?@*          
000070*?@?@?@?@?@?@??????@?@?@?@?@?F?@???????N????@?@?@?@?@?@?@?@?@*          
000080*?@?@?@?@?@?@?@?@?@?@?@?@?@?@?@?@?@?@?@?@?@?@?@?@?@?@?@?@?@?@?@*          
000090*?@?@?@?@?@?@?????@?@?@?@?@?F?@???????@?@?@?@?@?@?@?@?@*          
000100*?@?@?@?@?@?@?@?@?@?@?@?@?@?@?@?@?@?@?@?@?@?@?@?@?@?@?@?@?@?@?@*          
000110*?@?@?@?@?@?@?? ?? ???@?@?@?@?F?@?P?O?O?O?~?P????W?v?@?@?@?@?@*          
000120*?@?@?@?@?@?@?@?@?@?@?@?@?@?@?@?@?@?@?@?@?@?@?@?@?@?@?@?@?@?@?@*          
000130*2009.04.08?@???ID:57206????@(090408LEE) ?@?@?@?@?@?@?@?@?@?@*          
000140*?@?@?@?@?@?@WRHN2210?]?p?V?K?@(?x???????????)?@?@?@?@?@?@?@?@*          
000150*2023.04.11  ?I??  ???ID=76878  @????ID:230411MT              *          
000160*                  R04?@????2???J??                            *          
000170*2025.01.08  ?A?C?l?X  ?g?D?A??  ???ID=77180  @????ID:250108TL*          
000180*                  ??a5?N?x?@???????                         *          
000190****************************************************************          
000200 DATE-WRITTEN.                    H21.04.08.                              
000210   AUTHOR.                        INES.                                   
000220 ENVIRONMENT                      DIVISION.                               
000230 CONFIGURATION                    SECTION.                                
000240   SOURCE-COMPUTER.               FACOM-M380.                             
000250   OBJECT-COMPUTER.               FACOM-M380.                             
000260*                                                                         
000270 INPUT-OUTPUT                     SECTION.                                
000280   FILE-CONTROL.                                                          
000290*                                                                         
000300 DATA                             DIVISION.                               
000310 FILE                             SECTION.                                
000320*                                                                         
000330 WORKING-STORAGE                  SECTION.                                
000340 77  U01-CNT                      PIC  9(06)  VALUE   ZERO.               
000350 77  U02-CNT                      PIC  9(06)  VALUE   ZERO.               
000360 77  U30-CNT                      PIC  9(06)  VALUE   ZERO.               
000370 77  I                            PIC  9(06)  VALUE   ZERO.               
000380 77  W-KEISAN                     PIC  9(11)V9 VALUE  ZERO.               
000390*                                                                         
000400 77  NEW-KEY                      PIC  X(01)  VALUE   LOW-VALUE.          
000410 77  OLD-KEY                      PIC  X(01)  VALUE   LOW-VALUE.          
000420 77  K-KUBUN                      PIC  X(01)  VALUE   LOW-VALUE.          
000430 77  S-KUBUN                      PIC  X(01)  VALUE   LOW-VALUE.  090408LEE
000440*                                                                         
000450*----------------------------------------------------------------*
000460* ???o???`???
000470*----------------------------------------------------------------*
000480*  ?iU01?j                                        
000490 01  PARM-KAMA021A-U01.
000500     03  KAMA021A-DD-U01             PIC  X(08) VALUE SPACE.
000510     03  KAMA021A-SYORI-U01          PIC  X(08) VALUE SPACE.
000520     03  KAMA021A-RL-U01             PIC S9(09) COMP-5.
000530     03  KAMA021A-BL-U01             PIC S9(09) COMP-5.
000540     03  KAMA021A-CNT-U01            PIC S9(09) COMP-5.
000550     03  KAMA021A-EOF-U01            PIC  X(04) VALUE SPACE.
000560*                                                                         
000570   01  U01-REC.                                                           
000580       COPY  WRHNC030.                                            090408LEE
000590*
000600*  ?i?n?t?s?j 
000610 01  PARM-KAMA026A-U30.
000620     03  KAMA026A-DD-U30             PIC  X(08) VALUE SPACE.
000630     03  KAMA026A-SYORI-U30          PIC  X(08) VALUE SPACE.
000640     03  KAMA026A-RL-U30             PIC S9(09) COMP-5.
000650     03  KAMA026A-BL-U30             PIC S9(09) COMP-5.
000660     03  KAMA026A-CNT-U30            PIC S9(09) COMP-5.
000670*                                                                         
000680   01  U30-REC.                                                           
000690       COPY  WRHNC030.                                            090408LEE
000700*-------------------------------------------------------------------------
000710*                                                                         
000720 01  W-REC.                                                               
000730     03  W-IRYO.                                                          
000740         05  W-RANK-I             PIC  X(02).                             
000750*?? ???ID=76878  R04?@????2???J??                                230411MT
000760*        05  W-REC-NAIYOU-I       OCCURS  29.                             
000770*  ???ID=77180 2025/01/08 ??a5?N?x?@???????                    250108TL
000780*        05  W-REC-NAIYOU-I       OCCURS  32.                     250108TL
000790         05  W-REC-NAIYOU-I       OCCURS  36.                     250108TL
000800*?? ???ID=76878                                                  230411MT
000810             07  W-NAIYOU-I       PIC  S9(11)  COMP-3.                    
000820*?? ???ID=76878  R04?@????2???J??                                230411MT
000830*        05  FIL                  PIC  X(03).                     090408LEE
000840*   ???ID=77180 2025/01/08 ??a5?N?x?@???????                   250108TL
000850*        05  FIL                  PIC  X(05).                     250108TL
000860         05  FIL                  PIC  X(11).                     250108TL
000870*?? ???ID=76878                                                  230411MT
000880         05  W-KUBUN-I            PIC  X(01).                             
000890         05  W-KEY-I              PIC  X(20).                             
000900     03  W-KAIGO.                                                         
000910         05  W-RANK-K             PIC  X(02).                             
000920*?? ???ID=76878  R04?@????2???J??                                230411MT
000930*        05  W-REC-NAIYOU-K       OCCURS  29.                             
000940*   ???ID=77180 2025/01/08 ??a5?N?x?@???????                   250108TL
000950*        05  W-REC-NAIYOU-K       OCCURS  32.                     250108TL
000960         05  W-REC-NAIYOU-K       OCCURS  36.                     250108TL
000970*?? ???ID=76878                                                  230411MT
000980             07  W-NAIYOU-K       PIC  S9(11)  COMP-3.                    
000990*?? ???ID=76878  R04?@????2???J??                                230411MT
001000*        05  FIL                  PIC  X(03).                     090408LEE
001010*   ???ID=77180 2025/01/08 ??a5?N?x?@???????                   250108TL
001020*        05  FIL                  PIC  X(05).                     250108TL
001030         05  FIL                  PIC  X(11).                     250108TL
001040*?? ???ID=76878                                                  230411MT
001050         05  W-KUBUN-K            PIC  X(01).                             
001060         05  W-KEY-K              PIC  X(20).                             
001070     03  W-SIEN.                                                  090408LEE
001080         05  W-RANK-S             PIC  X(02).                     090408LEE
001090*?? ???ID=76878  R04?@????2???J??                                230411MT
001100*        05  W-REC-NAIYOU-S       OCCURS  29.                     090408LEE
001110*   ???ID=77180 2025/01/08 ??a5?N?x?@???????                   250108TL
001120*        05  W-REC-NAIYOU-S       OCCURS  32.                     250108TL
001130         05  W-REC-NAIYOU-S       OCCURS  36.                     250108TL
001140*?? ???ID=76878                                                  230411MT
001150             07  W-NAIYOU-S       PIC  S9(11)  COMP-3.            090408LEE
001160*?? ???ID=76878  R04?@????2???J??                                230411MT
001170*        05  FIL                  PIC  X(03).                     090408LEE
001180*   ???ID=77180 2025/01/08 ??a5?N?x?@???????                   250108TL
001190*        05  FIL                  PIC  X(05).                     250108TL
001200         05  FIL                  PIC  X(11).                     250108TL
001210*?? ???ID=76878                                                  230411MT
001220         05  W-KUBUN-S            PIC  X(01).                     090408LEE
001230         05  W-KEY-S              PIC  X(20).                     090408LEE
001240*                                                                         
001250 01  W-REC-G.                                                             
001260     03  W-G-IRYO.                                                        
001270         05  W-RANK-G-I           PIC  X(02).                             
001280*?? ???ID=76878  R04?@????2???J??                                230411MT
001290*        05  W-REC-NAIYOU-G-I     OCCURS  29.                             
001300*   ???ID=77180 2025/01/08 ??a5?N?x?@???????                   250108TL
001310*        05  W-REC-NAIYOU-G-I     OCCURS  32.                     250108TL
001320         05  W-REC-NAIYOU-G-I     OCCURS  36.                     250108TL
001330*?? ???ID=76878                                                  230411MT
001340             07  W-NAIYOU-G-I     PIC  S9(11)  COMP-3.                    
001350*?? ???ID=76878  R04?@????2???J??                                230411MT
001360*        05  FIL                  PIC  X(03).                     090408LEE
001370*   ???ID=77180 2025/01/08 ??a5?N?x?@???????                   250108TL
001380*        05  FIL                  PIC  X(05).                     250108TL
001390         05  FIL                  PIC  X(11).                     250108TL
001400*?? ???ID=76878                                                  230411MT
001410         05  W-KUBUN-G-I          PIC  X(01).                             
001420         05  W-KEY-G-I            PIC  X(20).                             
001430     03  W-G-KAIGO.                                                       
001440         05  W-RANK-G-K           PIC  X(02).                             
001450*?? ???ID=76878  R04?@????2???J??                                230411MT
001460*        05  W-REC-NAIYOU-G-K     OCCURS  29.                             
001470*   ???ID=77180 2025/01/08 ??a5?N?x?@???????                   250108TL
001480*        05  W-REC-NAIYOU-G-K     OCCURS  32.                     250108TL
001490         05  W-REC-NAITOU-G-K     OCCURS  36.                     250108TL
001500*?? ???ID=76878                                                  230411MT
001510             07  W-NAIYOU-G-K     PIC  S9(11)  COMP-3.                    
001520*?? ???ID=76878  R04?@????2???J??                                230411MT
001530*        05  FIL                  PIC  X(03).                     090408LEE
001540*   ???ID=77180 2025/01/08 ??a5?N?x?@???????                   250108TL
001550*        05  FIL                  PIC  X(05).                     250108TL
001560         05  FIL                  PIC  X(11).                     250108TL
001570*?? ???ID=76878                                                  230411MT
001580         05  W-KUBUN-G-K          PIC  X(01).                             
001590         05  W-KEY-G-K            PIC  X(20).                             
001600     03  W-G-SIEN.                                                090408LEE
001610         05  W-RANK-G-S           PIC  X(02).                     090408LEE
001620*?? ???ID=76878  R04?@????2???J??                                230411MT
001630*        05  W-REC-NAIYOU-G-S     OCCURS  29.                     090408LEE
001640*   ???ID=77180 2025/01/08 ??a5?N?x?@???????                   250108TL
001650*        05  W-REC-NAIYOU-G-S     OCCURS  32.                     250108TL
001660         05  W-REC-NAIYOU-G-S     OCCURS  36.                     250108TL
001670*?? ???ID=76878                                                  230411MT
001680             07  W-NAIYOU-G-S     PIC  S9(11)  COMP-3.            090408LEE
001690*?? ???ID=76878  R04?@????2???J??                                230411MT
001700*        05  FIL                  PIC  X(03).                     090408LEE
001710*   ???ID=77180 2025/01/08 ??a5?N?x?@???????                   250108TL
001720*        05  FIL                  PIC  X(05).                     250108TL
001730         05  FIL                  PIC  X(11).                     250108TL
001740*?? ???ID=76878                                                  230411MT
001750         05  W-KUBUN-G-S          PIC  X(01).                     090408LEE
001760         05  W-KEY-G-S            PIC  X(20).                     090408LEE
001770*                                                                         
001780 01  PARM-AREA.                                                           
001790     02  EXEC-P                   PIC X(01).                              
001800     02  FIL                      PIC X(01).                              
001810     02  P-MARUME-SW              PIC X(01).                              
001820*                                                                         
001830 PROCEDURE                   DIVISION.                                    
001840******************************************************************        
001850*****    ??  ???                                            *****        
001860******************************************************************        
001870 MAIN                             SECTION.                                
001880   HAJIME.                                                                
001890     DISPLAY '***  WRHN2210 START ***'           UPON  SYSOUT.    090408LEE
001900     MOVE   'U01'              TO  KAMA021A-DD-U01.
001910     MOVE   'OPEN'             TO  KAMA021A-SYORI-U01.
001920     COMPUTE KAMA021A-RL-U01   =   FUNCTION LENG(U01-REC).
001930     CALL   'KAMA021A'      USING  PARM-KAMA021A-U01.
001940*
001950     MOVE   'U30'              TO  KAMA026A-DD-U30.
001960     MOVE   'OPEN'             TO  KAMA026A-SYORI-U30.
001970     COMPUTE KAMA026A-RL-U30   =   FUNCTION LENG(U30-REC)
001980     CALL   'KAMA026A'      USING  PARM-KAMA026A-U30.
001990*
002000     ACCEPT   PARM-AREA           FROM  SYSIN.                            
002010     DISPLAY '(WRHN2210)?r?x?r?h?m??'                             090408LEE
002020             PARM-AREA            UPON  SYSOUT.                           
002030     EVALUATE P-MARUME-SW         OF    PARM-AREA                         
002040         WHEN '1'                                                         
002050             DISPLAY '(WRHN2210)1000?~?P???????s??'           090408LEE
002060                                  UPON  SYSOUT                            
002070         WHEN OTHER                                               090408LEE
002080             DISPLAY '(WRHN2210)1000?~?P???????????'                 
002090                                  UPON  SYSOUT                            
002100     END-EVALUATE.                                                        
002110*                                                                         
002120     PERFORM  U01-REC-READ.                                               
002130     INITIALIZE                   W-REC-G.                                
002140*                                                                         
002150     PERFORM  SUB-MAIN      UNTIL NEW-KEY    =  HIGH-VALUE.               
002160*                                                                         
002170     MOVE    '9'                  TO W-KUBUN-G-I.                         
002180     MOVE    W-REC-G       TO  U30-REC.
002190     MOVE  'WRITE'         TO  KAMA026A-SYORI-U30
002200     CALL  'KAMA026A'   USING  PARM-KAMA026A-U30
002210                                 U30-REC.
002220*                                                                         
002230     ADD     1                 TO U30-CNT.                                
002240*                                                                         
002250     MOVE   'CLOSE'             TO  KAMA021A-SYORI-U01.
002260     CALL   'KAMA021A'       USING  PARM-KAMA021A-U01.
002270*
002280     MOVE   'CLOSE'             TO  KAMA026A-SYORI-U30.
002290     CALL   'KAMA026A'       USING  PARM-KAMA026A-U30.
002300*                                                                         
002310     DISPLAY '***(INPUT) U01:??????e==> '                            
002320                                  U01-CNT     UPON  SYSOUT.               
002330     DISPLAY '***(OUTPUT)U30:??????e==> '                            
002340                                  U30-CNT     UPON  SYSOUT.               
002350     DISPLAY '***  WRHN2210 END  ***'         UPON  SYSOUT.       090408LEE
002360*
002370     STOP    RUN.                                                         
002380******************************************************************        
002390*****     SUB  -  MAIN                                       *****        
002400******************************************************************        
002410 SUB-MAIN                    SECTION.                                     
002420   HAJIME.                                                                
002430*                                                                         
002440     INITIALIZE                   W-REC.                                  
002450*                                                                         
002460     MOVE    NEW-KEY              TO  OLD-KEY                             
002470                                      W-KUBUN-I.                          
002480     MOVE    ZERO                 TO  W-KEISAN.                           
002490     MOVE    K-KUBUN              TO  W-KUBUN-K                           
002500     MOVE    S-KUBUN              TO  W-KUBUN-S                   090408LEE
002510                                                                          
002520*                                                                         
002530     PERFORM   A-KASAN     UNTIL  NEW-KEY NOT  =  OLD-KEY.                
002540*                                                                         
002550     PERFORM   SAIKEISAN.                                                 
002560*                                                                         
002570     MOVE    W-REC         TO  U30-REC.
002580     MOVE  'WRITE'         TO  KAMA026A-SYORI-U30
002590     CALL  'KAMA026A'   USING  PARM-KAMA026A-U30
002600                                 U30-REC.
002610*                                                                         
002620     ADD     1             TO     U30-CNT.                                
002630*                                                                         
002640   OWARI.                                                                 
002650     EXIT.                                                                
002660******************************************************************        
002670*****     A-KASAN                                            *****        
002680******************************************************************        
002690 A-KASAN                          SECTION.                                
002700   HAJIME.                                                                
002710*                                                                         
002720     PERFORM  VARYING  I  FROM    1  BY  1                                
002730*?? ???ID=76878  R04?@????2???J??                                230411MT
002740*                      UNTIL      I  >  29                                
002750*   ???ID=77180 2025/01/08 ??a5?N?x?@???????                   250108TL
002760*                      UNTIL      I  >  32                        250108TL
002770                       UNTIL      I  >  36                        250108TL
002780*?? ???ID=76878                                                  230411MT
002790*                                                                         
002800      IF   I    =  1  OR  18 OR  19  OR  20  OR  21                       
002810*   ???ID=76878  R04?@????2???J??                                230411MT
002820                      OR  30 OR  31  OR  32                       230411MT
002830*   ???ID=77180 2025/01/08 ??a5?N?x?@???????                   250108TL
002840                      OR  33 OR  34  OR  35  OR  36               250108TL
002850           IF   ??E??          OF ???? OF U01-REC                    
002860                                   = '2'                                  
002870                AND        EXEC-P  = '9'                                  
002880                CONTINUE                                                  
002890           ELSE                                                           
002900                ADD  ???e         OF ???? OF U01-REC(I)                 
002910                 TO  W-NAIYOU-I(I)                                        
002920           END-IF                                                         
002930           IF   ??E??          OF ??? OF U01-REC                    
002940                                   = '2'                                  
002950                AND        EXEC-P  = '9'                                  
002960                CONTINUE                                                  
002970           ELSE                                                           
002980                ADD  ???e         OF ??? OF U01-REC(I)                 
002990                 TO  W-NAIYOU-K(I)                                        
003000           END-IF                                                         
003010           IF   ??E??          OF ?x???? OF U01-REC            090408LEE
003020                                   = '2'                          090408LEE
003030                AND        EXEC-P  = '9'                                  
003040                CONTINUE                                          090408LEE
003050           ELSE                                                   090408LEE
003060                ADD  ???e         OF ?x???? OF U01-REC(I)         090408LEE
003070                 TO  W-NAIYOU-S(I)                                090408LEE
003080           END-IF                                                 090408LEE
003090      ELSE                                                                
003100                ADD  ???e         OF ???? OF U01-REC(I)                 
003110                 TO  W-NAIYOU-I(I)                                        
003120                ADD  ???e         OF ??? OF U01-REC(I)                 
003130                 TO  W-NAIYOU-K(I)                                        
003140                ADD  ???e         OF ?x???? OF U01-REC(I)         090408LEE
003150                 TO  W-NAIYOU-S(I)                                090408LEE
003160      END-IF                                                              
003170*                                                                         
003180     END-PERFORM.                                                         
003190*                                                                         
003200     PERFORM  U01-REC-READ.                                               
003210*                                                                         
003220   OWARI.                                                                 
003230     EXIT.                                                                
003240******************************************************************        
003250*****     SAIKEISAN                                          *****        
003260******************************************************************        
003270 SAIKEISAN                   SECTION.                                     
003280   HAJIME.                                                                
003290*                                                                         
003300     IF     P-MARUME-SW           =  '1'                                  
003310         PERFORM MARUME-SHORI                                             
003320     END-IF.                                                              
003330*                                                                         
003340     MOVE   ZERO         TO     W-KEISAN.                                 
003350     COMPUTE W-KEISAN =    W-NAIYOU-I(09) + W-NAIYOU-I(14) +              
003360                           W-NAIYOU-I(15) + W-NAIYOU-I(16) +              
003370                           W-NAIYOU-I(17)                                 
003380     MOVE    W-KEISAN    TO   W-NAIYOU-I(04).                             
003390*                                                                         
003400     COMPUTE W-KEISAN =    W-NAIYOU-K(09) + W-NAIYOU-K(14) +              
003410                           W-NAIYOU-K(15) + W-NAIYOU-K(16) +              
003420                           W-NAIYOU-K(17)                                 
003430     MOVE    W-KEISAN    TO   W-NAIYOU-K(04).                             
003440*                                                                         
003450     COMPUTE W-KEISAN =    W-NAIYOU-S(09) + W-NAIYOU-S(14) +      090408LEE
003460                           W-NAIYOU-S(15) + W-NAIYOU-S(16) +      090408LEE
003470                           W-NAIYOU-S(17)                         090408LEE
003480     MOVE    W-KEISAN    TO   W-NAIYOU-S(04).                     090408LEE
003490*                                                                         
003500     PERFORM  VARYING  I  FROM   1  BY  1                                 
003510*?? ???ID=76878  R04?@????2???J??                                230411MT
003520*                      UNTIL     I  >  29                                 
003530*   ???ID=77180 2025/01/08 ??a5?N?x?@???????                   250108TL
003540*                      UNTIL     I  >  32                         250108TL
003550                       UNTIL     I  >  36                         250108TL
003560*?? ???ID=76878                                                  230411MT
003570      ADD  W-NAIYOU-I(I)  TO     W-NAIYOU-G-I(I)                          
003580      ADD  W-NAIYOU-K(I)  TO     W-NAIYOU-G-K(I)                          
003590      ADD  W-NAIYOU-S(I)  TO     W-NAIYOU-G-S(I)                  090408LEE
003600     END-PERFORM.                                                         
003610*                                                                         
003620   OWARI.                                                                 
003630     EXIT.                                                                
003640******************************************************************        
003650*****     ?P?O?O?O?~?P??????                             *****        
003660******************************************************************        
003670 MARUME-SHORI                SECTION.                                     
003680   HAJIME.                                                                
003690*                                                                         
003700     MOVE   ZERO         TO     W-KEISAN.                                 
003710     COMPUTE W-KEISAN =   (W-NAIYOU-I(09) / 1000) + 0.5.                  
003720     MOVE    W-KEISAN    TO   W-NAIYOU-I(09).                             
003730*                                                                         
003740     MOVE   ZERO         TO     W-KEISAN.                                 
003750     COMPUTE W-KEISAN =   (W-NAIYOU-I(14) / 1000) + 0.5.                  
003760     MOVE    W-KEISAN    TO   W-NAIYOU-I(14).                             
003770*                                                                         
003780     MOVE   ZERO         TO     W-KEISAN.                                 
003790     COMPUTE W-KEISAN =   (W-NAIYOU-I(15) / 1000) + 0.5.                  
003800     MOVE    W-KEISAN    TO   W-NAIYOU-I(15).                             
003810*                                                                         
003820     MOVE   ZERO         TO     W-KEISAN.                                 
003830     COMPUTE W-KEISAN =   (W-NAIYOU-I(16) / 1000) + 0.5.                  
003840     MOVE    W-KEISAN    TO   W-NAIYOU-I(16).                             
003850*                                                                         
003860     MOVE   ZERO         TO     W-KEISAN.                                 
003870     COMPUTE W-KEISAN =   (W-NAIYOU-I(17) / 1000) + 0.5.                  
003880     MOVE    W-KEISAN    TO   W-NAIYOU-I(17).                             
003890*                                                                         
003900     MOVE   ZERO         TO     W-KEISAN.                                 
003910     COMPUTE W-KEISAN =   (W-NAIYOU-I(20) / 1000) + 0.5.                  
003920     MOVE    W-KEISAN    TO   W-NAIYOU-I(20).                             
003930*                                                                         
003940     MOVE   ZERO         TO     W-KEISAN.                                 
003950     COMPUTE W-KEISAN =   (W-NAIYOU-I(21) / 1000) + 0.5.                  
003960     MOVE    W-KEISAN    TO   W-NAIYOU-I(21).                             
003970*                                                                         
003980     MOVE   ZERO         TO     W-KEISAN.                                 
003990     COMPUTE W-KEISAN =   (W-NAIYOU-I(23) / 1000) + 0.5.                  
004000     MOVE    W-KEISAN    TO   W-NAIYOU-I(23).                             
004010*                                                                         
004020     MOVE   ZERO         TO     W-KEISAN.                                 
004030     COMPUTE W-KEISAN =   (W-NAIYOU-I(26) / 1000) + 0.5.                  
004040     MOVE    W-KEISAN    TO   W-NAIYOU-I(26).                             
004050*                                                                         
004060     MOVE   ZERO         TO     W-KEISAN.                                 
004070     COMPUTE W-KEISAN =   (W-NAIYOU-I(29) / 1000) + 0.5.                  
004080     MOVE    W-KEISAN    TO   W-NAIYOU-I(29).                             
004090*                                                                         
004100*?? ???ID=76878  R04?@????2???J??                                230411MT
004110     MOVE   ZERO         TO     W-KEISAN.                                 
004120     COMPUTE W-KEISAN =   (W-NAIYOU-I(32) / 1000) + 0.5.                  
004130     MOVE    W-KEISAN    TO   W-NAIYOU-I(32).                             
004140*                                                                         
004150*?? ???ID=76878                                                  230411MT
004160*?? ???ID=77180 2025/01/08 ??a5?N?x?@???????                   250108TL
004170     MOVE   ZERO         TO     W-KEISAN.                                 
004180     COMPUTE W-KEISAN =    (W-NAIYOU-I(35) / 1000) + 0.5.                  
004190     MOVE    W-KEISAN    TO   W-NAIYOU-I(35).                             
004200*                                                                         
004210     MOVE   ZERO         TO     W-KEISAN.                                 
004220     COMPUTE W-KEISAN =    (W-NAIYOU-I(36) / 1000) + 0.5.                  
004230     MOVE    W-KEISAN    TO   W-NAIYOU-I(36).                             
004240*?? ???ID=77180                                                  250108TL
004250     MOVE   ZERO         TO     W-KEISAN.                                 
004260     COMPUTE W-KEISAN =   (W-NAIYOU-K(09) / 1000) + 0.5.                  
004270     MOVE    W-KEISAN    TO   W-NAIYOU-K(09).                             
004280*                                                                         
004290     MOVE   ZERO         TO     W-KEISAN.                                 
004300     COMPUTE W-KEISAN =   (W-NAIYOU-K(14) / 1000) + 0.5.                  
004310     MOVE    W-KEISAN    TO   W-NAIYOU-K(14).                             
004320*                                                                         
004330     MOVE   ZERO         TO     W-KEISAN.                                 
004340     COMPUTE W-KEISAN =   (W-NAIYOU-K(15) / 1000) + 0.5.                  
004350     MOVE    W-KEISAN    TO   W-NAIYOU-K(15).                             
004360*                                                                         
004370     MOVE   ZERO         TO     W-KEISAN.                                 
004380     COMPUTE W-KEISAN =   (W-NAIYOU-K(16) / 1000) + 0.5.                  
004390     MOVE    W-KEISAN    TO   W-NAIYOU-K(16).                             
004400*                                                                         
004410     MOVE   ZERO         TO     W-KEISAN.                                 
004420     COMPUTE W-KEISAN =   (W-NAIYOU-K(17) / 1000) + 0.5.                  
004430     MOVE    W-KEISAN    TO   W-NAIYOU-K(17).                             
004440*                                                                         
004450     MOVE   ZERO         TO     W-KEISAN.                                 
004460     COMPUTE W-KEISAN =   (W-NAIYOU-K(20) / 1000) + 0.5.                  
004470     MOVE    W-KEISAN    TO   W-NAIYOU-K(20).                             
004480*                                                                         
004490     MOVE   ZERO         TO     W-KEISAN.                                 
004500     COMPUTE W-KEISAN =   (W-NAIYOU-K(21) / 1000) + 0.5.                  
004510     MOVE    W-KEISAN    TO   W-NAIYOU-K(21).                             
004520*                                                                         
004530     MOVE   ZERO         TO     W-KEISAN.                                 
004540     COMPUTE W-KEISAN =   (W-NAIYOU-K(23) / 1000) + 0.5.                  
004550     MOVE    W-KEISAN    TO   W-NAIYOU-K(23).                             
004560*                                                                         
004570     MOVE   ZERO         TO     W-KEISAN.                                 
004580     COMPUTE W-KEISAN =   (W-NAIYOU-K(26) / 1000) + 0.5.                  
004590     MOVE    W-KEISAN    TO   W-NAIYOU-K(26).                             
004600*                                                                         
004610     MOVE   ZERO         TO     W-KEISAN.                                 
004620     COMPUTE W-KEISAN =   (W-NAIYOU-K(29) / 1000) + 0.5.                  
004630     MOVE    W-KEISAN    TO   W-NAIYOU-K(29).                             
004640*
004650*?? ???ID=76878  R04?@????2???J??                                230411MT
004660     MOVE   ZERO         TO     W-KEISAN.                                 
004670     COMPUTE W-KEISAN =   (W-NAIYOU-K(32) / 1000) + 0.5.                  
004680     MOVE    W-KEISAN    TO   W-NAIYOU-K(32).                             
004690*                         
004700*?? ???ID=76878                                                  230411MT
004710*?? ???ID=77180 2025/01/08 ??a5?N?x?@???????                   250108TL
004720     MOVE   ZERO         TO     W-KEISAN.                                 
004730     COMPUTE W-KEISAN =    (W-NAIYOU-K(35) / 1000) + 0.5.                  
004740     MOVE    W-KEISAN    TO   W-NAIYOU-K(35).                             
004750*                                                                         
004760     MOVE   ZERO         TO     W-KEISAN.                                 
004770     COMPUTE W-KEISAN =    (W-NAIYOU-K(36) / 1000) + 0.5.                  
004780     MOVE    W-KEISAN    TO   W-NAIYOU-K(36).                             
004790*?? ???ID=77180                                                  250108TL
004800     MOVE   ZERO         TO     W-KEISAN.                         090408LEE
004810     COMPUTE W-KEISAN =   (W-NAIYOU-S(09) / 1000) + 0.5.          090408LEE
004820     MOVE    W-KEISAN    TO   W-NAIYOU-S(09).                     090408LEE
004830*                                                                         
004840     MOVE   ZERO         TO     W-KEISAN.                         090408LEE
004850     COMPUTE W-KEISAN =   (W-NAIYOU-S(14) / 1000) + 0.5.          090408LEE
004860     MOVE    W-KEISAN    TO   W-NAIYOU-S(14).                     090408LEE
004870*                                                                         
004880     MOVE   ZERO         TO     W-KEISAN.                         090408LEE
004890     COMPUTE W-KEISAN =   (W-NAIYOU-S(15) / 1000) + 0.5.          090408LEE
004900     MOVE    W-KEISAN    TO   W-NAIYOU-S(15).                     090408LEE
004910*                                                                          
004920     MOVE   ZERO         TO     W-KEISAN.                         090408LEE
004930     COMPUTE W-KEISAN =   (W-NAIYOU-S(16) / 1000) + 0.5.          090408LEE
004940     MOVE    W-KEISAN    TO   W-NAIYOU-S(16).                     090408LEE
004950*                                                                         
004960     MOVE   ZERO         TO     W-KEISAN.                         090408LEE
004970     COMPUTE W-KEISAN =   (W-NAIYOU-S(17) / 1000) + 0.5.          090408LEE
004980     MOVE    W-KEISAN    TO   W-NAIYOU-S(17).                     090408LEE
004990*                                                                          
005000     MOVE   ZERO         TO     W-KEISAN.                         090408LEE
005010     COMPUTE W-KEISAN =   (W-NAIYOU-S(20) / 1000) + 0.5.          090408LEE
005020     MOVE    W-KEISAN    TO   W-NAIYOU-S(20).                     090408LEE
005030*                                                                         
005040     MOVE   ZERO         TO     W-KEISAN.                         090408LEE
005050     COMPUTE W-KEISAN =   (W-NAIYOU-S(21) / 1000) + 0.5.          090408LEE
005060     MOVE    W-KEISAN    TO   W-NAIYOU-S(21).                     090408LEE
005070*                                                                          
005080     MOVE   ZERO         TO     W-KEISAN.                         090408LEE
005090     COMPUTE W-KEISAN =   (W-NAIYOU-S(23) / 1000) + 0.5.          090408LEE
005100     MOVE    W-KEISAN    TO   W-NAIYOU-S(23).                     090408LEE
005110*                                                                         
005120     MOVE   ZERO         TO     W-KEISAN.                         090408LEE
005130     COMPUTE W-KEISAN =   (W-NAIYOU-S(26) / 1000) + 0.5.          090408LEE
005140     MOVE    W-KEISAN    TO   W-NAIYOU-S(26).                     090408LEE
005150*                                                                          
005160     MOVE   ZERO         TO     W-KEISAN.                         090408LEE
005170     COMPUTE W-KEISAN =   (W-NAIYOU-S(29) / 1000) + 0.5.          090408LEE
005180     MOVE    W-KEISAN    TO   W-NAIYOU-S(29).                     090408LEE
005190*                                                                         
005200*?? ???ID=76878  R04?@????2???J??                                230411MT
005210     MOVE   ZERO         TO     W-KEISAN.                                 
005220     COMPUTE W-KEISAN =   (W-NAIYOU-S(32) / 1000) + 0.5.                  
005230     MOVE    W-KEISAN    TO   W-NAIYOU-S(32).                             
005240*                                                                         
005250*?? ???ID=76878                                                  230411MT
005260*?? ???ID=77180 2025/01/08 ??a5?N?x?@???????                   250108TL
005270     MOVE   ZERO         TO     W-KEISAN.                                 
005280     COMPUTE W-KEISAN =    (W-NAIYOU-S(35) / 1000) + 0.5.                  
005290     MOVE    W-KEISAN    TO   W-NAIYOU-S(35).                             
005300*                                                                         
005310     MOVE   ZERO         TO     W-KEISAN.                                 
005320     COMPUTE W-KEISAN =    (W-NAIYOU-S(36) / 1000) + 0.5.                  
005330     MOVE    W-KEISAN    TO   W-NAIYOU-S(36).                             
005340*?? ???ID=77180                                                  250108TL
005350   OWARI.                                                                 
005360     EXIT.                                                                
005370******************************************************************        
005380*****    A ??? ? ?„{?          ( ???? ??- )                *****        
005390******************************************************************        
005400 U01-REC-READ                     SECTION.                                
005410   HAJIME.                                                                
005420*
005430     MOVE   'U01'           TO   KAMA021A-DD-U01.
005440     MOVE   'READ'          TO   KAMA021A-SYORI-U01.
005450     CALL   'KAMA021A'   USING   PARM-KAMA021A-U01
005460                                 U01-REC.
005470*
005480*@@                                     '??? '.
005490     IF      KAMA021A-EOF-U01(1:3)    =   X'B5DCD8'
005500             MOVE    HIGH-VALUE  TO  NEW-KEY
005510             GO  TO  OWARI
005520     END-IF.
005530*
005540     ADD     1                    TO  U01-CNT.                            
005550*                                                                         
005560     MOVE    ?y????             OF  ????  OF  U01-REC                 
005570       TO    NEW-KEY.                                                     
005580     MOVE    ?y????             OF  ???  OF  U01-REC                 
005590       TO    K-KUBUN.                                                     
005600     MOVE    ?y????             OF  ?x????  OF  U01-REC         090408LEE
005610       TO    S-KUBUN.                                             090408LEE
005620*                                                                         
005630   OWARI.                                                                 
005640     EXIT.                                                                
005650******************************************************************        
005660*****    WRHN2210  PROGRAM  END                              *****        
005670******************************************************************        
