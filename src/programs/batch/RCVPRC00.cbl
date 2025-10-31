*================================================================*
      * Program Name: RCVPRC00
      * Description: Process Recovery Handler
      * Version: 1.0
      * Date: 2024
      *================================================================*
       IDENTIFICATION DIVISION.
       PROGRAM-ID. RCVPRC00.
       
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
               
           SELECT PROCESS-SEQ-FILE
               ASSIGN TO PRCSEQ
               ORGANIZATION IS INDEXED
               ACCESS MODE IS DYNAMIC
               RECORD KEY IS PSR-KEY
               FILE STATUS IS WS-PSR-STATUS.
      *-- Change: Add real-time price feed polling file
           SELECT PRICE-FEED-FILE
               ASSIGN TO PRCFEED
               ORGANIZATION IS SEQUENTIAL
               FILE STATUS IS WS-PRCFEED-STATUS.
       
       DATA DIVISION.
       FILE SECTION.
       FD  BATCH-CONTROL-FILE.
           COPY BCHCTL.
           
       FD  PROCESS-SEQ-FILE.
           COPY PRCSEQ.
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
           05  WS-PSR-STATUS         PIC X(2).
      *-- Change: Add price feed file status
           05  WS-PRCFEED-STATUS     PIC X(2).
       
       01  WS-WORK-AREAS.
           05  WS-CURRENT-TIME       PIC X(26).
           05  WS-RECOVERY-MODE      PIC X(1).
               88  WS-RECOVER-PROCESS  VALUE 'P'.
               88  WS-RECOVER-SEQUENCE VALUE 'S'.
               88  WS-RECOVER-ALL      VALUE 'A'.
           05  WS-RECOVERY-ACTION    PIC X(1).
               88  WS-ACTION-RESTART   VALUE 'R'.
               88  WS-ACTION-BYPASS    VALUE 'B'.
               88  WS-ACTION-TERMINATE VALUE 'T'.
      *-- Change: Add error tracking for price feed
           05  WS-PRICE-FEED-ERROR   PIC X(80).
       
       LINKAGE SECTION.
       01  LS-RECOVERY-REQUEST.
           05  LS-FUNCTION          PIC X(4).
               88  FUNC-INIT          VALUE 'INIT'.
               88  FUNC-RECV          VALUE 'RECV'.
               88  FUNC-TERM          VALUE 'TERM'.
           05  LS-PROCESS-DATE     PIC X(8).
           05  LS-PROCESS-ID       PIC X(8).
           05  LS-RECOVERY-TYPE    PIC X(1).
           05  LS-RECOVERY-PARM    PIC X(50).
           05  LS-RETURN-CODE      PIC S9(4) COMP.
       
       PROCEDURE DIVISION USING LS-RECOVERY-REQUEST.
       0000-MAIN.
           EVALUATE TRUE
               WHEN FUNC-INIT
                   PERFORM 1000-INITIALIZE-RECOVERY
               WHEN FUNC-RECV
                   PERFORM 2000-PROCESS-RECOVERY
               WHEN FUNC-TERM
                   PERFORM 3000-TERMINATE-RECOVERY
               WHEN OTHER
                   MOVE 'Invalid function code' TO ERR-TEXT
                   PERFORM 9000-ERROR-ROUTINE
           END-EVALUATE
           MOVE LS-RETURN-CODE TO RETURN-CODE
           GOBACK
           .
           
       1000-INITIALIZE-RECOVERY.
           PERFORM 1100-OPEN-FILES
           PERFORM 1200-VALIDATE-REQUEST
           PERFORM 1300-SET-RECOVERY-MODE
           .
           
       1100-OPEN-FILES.
           OPEN I-O BATCH-CONTROL-FILE
           IF WS-BCT-STATUS NOT = '00'
               MOVE 'Error opening control file' TO ERR-TEXT
               PERFORM 9000-ERROR-ROUTINE
           END-IF
           OPEN INPUT PROCESS-SEQ-FILE
           IF WS-PSR-STATUS NOT = '00'
               MOVE 'Error opening sequence file' TO ERR-TEXT
               PERFORM 9000-ERROR-ROUTINE
           END-IF
      *-- Change: Open price feed file for polling
           OPEN INPUT PRICE-FEED-FILE
           IF WS-PRCFEED-STATUS NOT = '00'
               MOVE 'Error opening price feed file' TO WS-PRICE-FEED-ERROR
               DISPLAY '*-- Change: Price feed file open error: ' WS-PRICE-FEED-ERROR
           END-IF
           .
           
       1200-VALIDATE-REQUEST.
           IF LS-PROCESS-DATE = SPACES
               MOVE 'Process date required' TO ERR-TEXT
               PERFORM 9000-ERROR-ROUTINE
           END-IF
           EVALUATE LS-RECOVERY-TYPE
               WHEN 'P'
               WHEN 'S'
               WHEN 'A'
                   CONTINUE
               WHEN OTHER
                   MOVE 'Invalid recovery type' TO ERR-TEXT
                   PERFORM 9000-ERROR-ROUTINE
           END-EVALUATE
      *-- Change: Validate price feed file status
           IF WS-PRCFEED-STATUS NOT = '00'
               DISPLAY '*-- Change: Price feed file not available for polling'
           END-IF
           .
      *-- Change: Add polling logic for real-time price feed
       1250-POLL-PRICE-FEED.
           IF WS-PRCFEED-STATUS = '00'
               READ PRICE-FEED-FILE
                   AT END
                       MOVE '10' TO WS-PRCFEED-STATUS
                   NOT AT END
                       DISPLAY '*-- Change: Polled price feed: ' PRCFEED-SECURITY-ID ' ' PRCFEED-PRICE
               END-READ
           END-IF
           .
       2000-PROCESS-RECOVERY.
           EVALUATE WS-RECOVERY-MODE
               WHEN 'P'
                   PERFORM 2100-RECOVER-PROCESS
               WHEN 'S'
                   PERFORM 2200-RECOVER-SEQUENCE
               WHEN 'A'
                   PERFORM 2300-RECOVER-ALL
           END-EVALUATE
      *-- Change: Poll price feed during recovery
           PERFORM 1250-POLL-PRICE-FEED
           .
       3000-TERMINATE-RECOVERY.
           PERFORM 3100-UPDATE-FINAL-STATUS
           PERFORM 3200-CLOSE-FILES
           .
       3200-CLOSE-FILES.
           CLOSE BATCH-CONTROL-FILE
                 PROCESS-SEQ-FILE
                 PRICE-FEED-FILE
           IF WS-BCT-STATUS NOT = '00' OR
              WS-PSR-STATUS NOT = '00'
               MOVE 'Error closing files' TO ERR-TEXT
               PERFORM 9000-ERROR-ROUTINE
           END-IF
           .
       9000-ERROR-ROUTINE.
           MOVE 'RCVPRC00' TO ERR-PROGRAM
           MOVE BCT-RC-ERROR TO LS-RETURN-CODE
           CALL 'ERRPROC' USING ERR-MESSAGE
           .
      * ... (rest unchanged) ...