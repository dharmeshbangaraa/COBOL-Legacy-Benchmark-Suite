*================================================================*
      * Program Name: PORTTRAN
      * Description: Portfolio Transaction Processing
      * Author: [Author name]
      * Date Written: 2024-03-20
      *================================================================*
       IDENTIFICATION DIVISION.
       PROGRAM-ID. PORTTRAN.
       
       ENVIRONMENT DIVISION.
       CONFIGURATION SECTION.
       SOURCE-COMPUTER. IBM-ZOS.
       OBJECT-COMPUTER. IBM-ZOS.
       
       INPUT-OUTPUT SECTION.
       FILE-CONTROL.
           SELECT TRANSACTION-FILE
               ASSIGN TO TRANFILE
               ORGANIZATION IS SEQUENTIAL
               ACCESS MODE IS SEQUENTIAL
               FILE STATUS IS WS-TRAN-STATUS.
               
           SELECT PORTFOLIO-FILE
               ASSIGN TO PORTFILE
               ORGANIZATION IS INDEXED
               ACCESS MODE IS RANDOM
               RECORD KEY IS PORT-ID
               FILE STATUS IS WS-PORT-STATUS.
      *-- Change: Add price feed file for valuation logic
           SELECT PRICE-FEED-FILE
               ASSIGN TO PRCFEED
               ORGANIZATION IS SEQUENTIAL
               FILE STATUS IS WS-PRCFEED-STATUS.
       
       DATA DIVISION.
       FILE SECTION.
       FD  TRANSACTION-FILE
           RECORDING MODE IS F
           BLOCK CONTAINS 0 RECORDS.
       COPY TRNREC.
       
       FD  PORTFOLIO-FILE
           RECORDING MODE IS F
           BLOCK CONTAINS 0 RECORDS.
       COPY PORTREC.
      *-- Change: Add price feed file section
       FD  PRICE-FEED-FILE.
       01  PRICE-FEED-RECORD.
           05  PRCFEED-SECURITY-ID   PIC X(12).
           05  PRCFEED-PRICE         PIC 9(13)V99.
           05  PRCFEED-TIMESTAMP     PIC X(26).
       
       WORKING-STORAGE SECTION.
           COPY ERRHAND.
           COPY AUDITLOG.
           
       01  WS-FILE-STATUS.
           05  WS-TRAN-STATUS      PIC X(2).
           05  WS-PORT-STATUS      PIC X(2).
      *-- Change: Add price feed file status
           05  WS-PRCFEED-STATUS   PIC X(2).
           
       01  WS-COUNTERS.
           05  WS-READ-COUNT       PIC 9(8) COMP.
           05  WS-PROCESS-COUNT    PIC 9(8) COMP.
           05  WS-ERROR-COUNT      PIC 9(8) COMP.
           
       01  WS-EOF-FLAG            PIC X(1).
           88  END-OF-FILE          VALUE 'Y'.
           88  MORE-RECORDS         VALUE 'N'.
      *-- Change: Add latest price for valuation
       01  WS-LATEST-PRICE        PIC 9(13)V99 VALUE ZERO.
       01  WS-LATEST-SECURITY     PIC X(12) VALUE SPACES.
       01  WS-LATEST-TS           PIC X(26) VALUE SPACES.
           
       PROCEDURE DIVISION.
       0000-MAIN.
           PERFORM 1000-INITIALIZE
      *-- Change: Ingest price feed before processing transactions
           PERFORM 1500-INGEST-PRICE-FEED
           IF WS-TRAN-STATUS = '00'
               PERFORM 2000-PROCESS-TRANSACTIONS
                   UNTIL END-OF-FILE
                   OR WS-ERROR-COUNT > 100
           END-IF
           PERFORM 3000-TERMINATE
           GOBACK
           .
           
       1000-INITIALIZE.
           INITIALIZE WS-FILE-STATUS
                      WS-COUNTERS
           SET MORE-RECORDS TO TRUE
           OPEN INPUT TRANSACTION-FILE
           IF WS-TRAN-STATUS NOT = '00'
               MOVE 'Error opening transaction file' TO ERR-TEXT
               PERFORM 9000-ERROR-ROUTINE
           END-IF
           OPEN I-O PORTFOLIO-FILE
           IF WS-PORT-STATUS NOT = '00'
               MOVE 'Error opening portfolio file' TO ERR-TEXT
               PERFORM 9000-ERROR-ROUTINE
           END-IF
      *-- Change: Open price feed file
           OPEN INPUT PRICE-FEED-FILE
           IF WS-PRCFEED-STATUS NOT = '00'
               DISPLAY '*-- Change: Error opening price feed file'
           END-IF
           .
      *-- Change: Ingest price feed for valuation
       1500-INGEST-PRICE-FEED.
           PERFORM UNTIL WS-PRCFEED-STATUS = '10'
               READ PRICE-FEED-FILE
                   AT END
                       MOVE '10' TO WS-PRCFEED-STATUS
                   NOT AT END
                       MOVE PRCFEED-SECURITY-ID TO WS-LATEST-SECURITY
                       MOVE PRCFEED-PRICE TO WS-LATEST-PRICE
                       MOVE PRCFEED-TIMESTAMP TO WS-LATEST-TS
                       DISPLAY '*-- Change: Price feed: ' WS-LATEST-SECURITY ' ' WS-LATEST-PRICE
               END-READ
           END-PERFORM
           .
       2000-PROCESS-TRANSACTIONS.
           READ TRANSACTION-FILE
               AT END
                   SET END-OF-FILE TO TRUE
               NOT AT END
                   ADD 1 TO WS-READ-COUNT
                   PERFORM 2100-VALIDATE-TRANSACTION
           END-READ
           .
      *-- Change: Use latest price for valuation in transaction processing
       2110-CHECK-PORTFOLIO.
           IF TRN-PORTFOLIO-ID = SPACES
               MOVE 'Portfolio ID is required' TO ERR-TEXT
               EXIT PARAGRAPH
           END-IF
           MOVE TRN-PORTFOLIO-ID TO PORT-ID
           READ PORTFOLIO-FILE
               INVALID KEY
                   STRING 'Invalid Portfolio ID: '
                          TRN-PORTFOLIO-ID
                     DELIMITED BY SIZE
                     INTO ERR-TEXT
           END-READ
      *-- Change: Apply real-time price if security matches
           IF PORT-SECURITY-ID = WS-LATEST-SECURITY
               MOVE WS-LATEST-PRICE TO PORT-MARKET-PRICE
               DISPLAY '*-- Change: Real-time price applied in transaction'
           END-IF
           .
      * ... (rest unchanged) ...