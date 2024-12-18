       IDENTIFICATION                   DIVISION.
       PROGRAM-ID.                      EMPSLRMATCHP0001.
      ******************************************************************
       AUTHOR.                          LE KIM THUAN.
       DATE-WRITTEN.                    2024/12/17.
       DATE-COMPILED.                   2024/12/17.
      *
      ******************************************************************
      ******************************************************************
       ENVIRONMENT                      DIVISION.
      ******************************************************************
      *=================================================================
       CONFIGURATION                    SECTION.
      *=================================================================
       SOURCE-COMPUTER.                 MYCOMPUTER.
      *
      *=================================================================
       INPUT-OUTPUT                     SECTION.
      *=================================================================
       FILE-CONTROL.
           SELECT  ID01                 ASSIGN       TO "EMPLOYEE.txt"
                                        ORGANIZATION IS LINE SEQUENTIAL.

           SELECT  IDO2                 ASSIGN       TO "SALARY.txt"
                                        ORGANIZATION IS LINE SEQUENTIAL.
                                        
           SELECT  OD01                 ASSIGN       TO "RESULT.txt"
                                        ORGANIZATION IS LINE SEQUENTIAL.
      ******************************************************************
       DATA                             DIVISION.
       FILE                             SECTION.
      *-----------------------------------------------------------------
      *  CẤU TRÚC CỦA FILE ID01
      *-----------------------------------------------------------------
       FD  IDO1.
       01  IN-EMP-REC.
           03  IN-EMP.
               05  IN-EMP-ID            PIC  X(005).
               05  IN-EMP-NAME          PIC  X(030).
           03  IN-HOURS-WORKED          PIC  9(004).
           03  IN-HOURLY-RATE           PIC  9(003)V99.
           03  FILLER                   PIC  X(016).
      *-----------------------------------------------------------------
      *  CẤU TRÚC CỦA FILE ID02
      *-----------------------------------------------------------------
       FD  ID02.
       01  IN-EMP-REC.
           03  IN-EMP.
               05  IN-EMP-ID            PIC  X(005).
               05  IN-EMP-NAME          PIC  X(030).
           03  IN-GROSS-SALARY          PIC  9(007)V99.
           03  IN-TAX                   PIC  9(007)V99.
           03  IN-NET-SALARY            PIC  9(007)V99.
           03  FILLER                   PIC  X(016).
      *-----------------------------------------------------------------
      *  CẤU TRÚC CỦA FILE OD01
      *-----------------------------------------------------------------
       FD  OD01.
       01  OUT-EMP-MATCH-REC.
           03  OUT-EMP.
               05  OUT-EMP-ID           PIC  X(005).
               05  OUT-EMP-NAME         PIC  X(030).
           03  OUT-GROSS-SALARY         PIC  9(007)V99.
           03  OUT-TAX                  PIC  9(007)V99.
           03  OUT-NET-SALARY           PIC  9(007)V99.
           03  OUT-STATUS               PIC  X(010).
           03  FILLER                   PIC  X(016).
      *=================================================================
      *=================================================================
       WORKING-STORAGE                  SECTION.
      *=================================================================
      *-----------------------------------------------------------------
      *  KHU VỰC ĐỊNH NGHĨA ĐẦU RA
      *-----------------------------------------------------------------
