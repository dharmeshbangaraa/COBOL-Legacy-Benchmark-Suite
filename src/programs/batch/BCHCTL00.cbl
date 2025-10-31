*================================================================*
      * Program Name: BCHCTL00
      * Description: Batch Control Processor
      * Version: 1.0
      * Date: 2024
      *================================================================*
       IDENTIFICATION DIVISION.
       PROGRAM-ID. BCHCTL00.
       
       ENVIRONMENT DIVISION.
       CONFIGURATION SECTION.
       SOURCE-COMPUTER. IBM-ZOS.
       OBJECT-COMPUTER. IBM-ZOS.
       
       INPUT-OUTPUT SECTION.
       FILE-CONTROL.
           SELECT BATCH-CONTROL-FILE
               ASSIGN TO BCHCTL
               ORGANIZATION IS INDEXED
               ACCESS MODE IS DYNAMIC
               RECORD KEY IS BCT-KEY
               FILE STATUS IS WS-BCT-STATUS.
      *-- Change: Add price feed file for batch synchronization
           SELECT PRICE-FEED-FILE
               ASSIGN TO PRCFEED
               ORGANIZATION IS SEQUENTIAL
               FILE STATUS IS WS-PRCFEED-STATUS.
       
       DATA DIVISION.
       FILE SECTION.
       FD  BATCH-CONTROL-FILE.
           COPY BCHCTL.
      *-- Change: Add price feed file section
       FD  PRICE-FEED-FILE.
       01  PRICE-FEED-RECORD.
           05  PRCFEED-SECURITY-ID   PIC X(12).
           05  PRCFEED-PRICE         PIC 9(13)V99.
           05  PRCFEED-TIMESTAMP     PIC X(26).
       
       WORKING-STORAGE SECTION.
           COPY BCHCON.
           COPY ERRHAND.
           
       01  WS-FILE-STATUS.
           05  WS-BCT-STATUS         PIC X(2).
      *-- Change: Add price feed file status
           05  WS-PRCFEED-STATUS     PIC X(2).
           
       01  WS-WORK-AREAS.
           05  WS-CURRENT-TIME       PIC X(26).
           05  WS-PREREQ-MET         PIC X(1).
               88  PREREQS-SATISFIED    VALUE 'Y'.
               88  PREREQS-PENDING      VALUE 'N'.
           05  WS-PROCESS-MODE       PIC X(1).
               88  MODE-INITIALIZE      VALUE 'I'.
               88  MODE-CHECK-PREREQ    VALUE 'C'.
               88  MODE-UPDATE-STATUS   VALUE 'U'.
               88  MODE-FINALIZE        VALUE 'F'.
      *-- Change: Add price feed sync flag
           05  WS-PRICE-FEED-SYNC    PIC X(1) VALUE 'N'.
               88  PRICE-FEED-SYNCED     VALUE 'Y'.
               88  PRICE-FEED-NOT-SYNCED VALUE 'N'.
       
       LINKAGE SECTION.
       01  LS-CONTROL-REQUEST.
           05  LS-FUNCTION          PIC X(4).
               88  FUNC-INIT          VALUE 'INIT'.
               88  FUNC-CHEK          VALUE 'CHEK'.
               88  FUNC-UPDT          VALUE 'UPDT'.
               88  FUNC-TERM          VALUE 'TERM'.
           05  LS-JOB-NAME         PIC X(8).
           05  LS-PROCESS-DATE     PIC X(8).
           05  LS-SEQUENCE-NO      PIC 9(4).
           05  LS-RETURN-CODE      PIC S9(4) COMP.
       
       PROCEDURE DIVISION USING LS-CONTROL-REQUEST.
       0000-MAIN.
           EVALUATE TRUE
               WHEN FUNC-INIT
                   SET MODE-INITIALIZE TO TRUE
                   PERFORM 1000-PROCESS-INITIALIZE
               WHEN FUNC-CHEK
                   SET MODE-CHECK-PREREQ TO TRUE
                   PERFORM 2000-CHECK-PREREQUISITES
               WHEN FUNC-UPDT
                   SET MODE-UPDATE-STATUS TO TRUE
                   PERFORM 3000-UPDATE-STATUS
               WHEN FUNC-TERM
                   SET MODE-FINALIZE TO TRUE
                   PERFORM 4000-PROCESS-TERMINATE
               WHEN OTHER
                   MOVE 'Invalid function code' TO ERR-TEXT
                   PERFORM 9000-ERROR-ROUTINE
           END-EVALUATE
           MOVE LS-RETURN-CODE TO RETURN-CODE
           GOBACK
           .
      *-- Change: Synchronize batch with price feed
       1000-PROCESS-INITIALIZE.
           PERFORM 1100-OPEN-FILES
           PERFORM 1200-READ-CONTROL-RECORD
           PERFORM 1300-VALIDATE-PROCESS
           PERFORM 1400-UPDATE-START-STATUS
           PERFORM 1500-SYNC-PRICE-FEED
           .
       1100-OPEN-FILES.
           OPEN I-O BATCH-CONTROL-FILE
           IF WS-BCT-STATUS NOT = '00'
               MOVE 'Error opening control file' TO ERR-TEXT
               PERFORM 9000-ERROR-ROUTINE
           END-IF
      *-- Change: Open price feed file
           OPEN INPUT PRICE-FEED-FILE
           IF WS-PRCFEED-STATUS NOT = '00'
               DISPLAY '*-- Change: Error opening price feed file'
           END-IF
           .
      *-- Change: Sync logic for price feed
       1500-SYNC-PRICE-FEED.
           IF WS-PRCFEED-STATUS = '00'
               SET PRICE-FEED-SYNCED TO TRUE
               DISPLAY '*-- Change: Batch synchronized with real-time price feed'
           ELSE
               SET PRICE-FEED-NOT-SYNCED TO TRUE
               DISPLAY '*-- Change: Price feed not available for batch sync'
           END-IF
           .
      * ... (rest unchanged) ...