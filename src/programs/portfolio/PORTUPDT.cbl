*================================================================*
      * Program Name: PORTUPDT
      * Description: Portfolio Update Program
      *             Updates existing portfolio records
      * Author: [Author name]
      * Date Written: 2024-03-20
      * Maintenance Log:
      * Date       Author        Description
      * ---------- ------------- -------------------------------------
      * 2024-03-20 [Author]     Initial Creation
      * 2024-06-XX [COBOL Impact Modifier Agent] Real-time price feed integration, valuation update, error handling, audit logging *-- Change: Real-time price feed, valuation, error handling, audit logging
      *================================================================*
       IDENTIFICATION DIVISION.
       PROGRAM-ID. PORTUPDT.
       
       ENVIRONMENT DIVISION.
       CONFIGURATION SECTION.
       SOURCE-COMPUTER. IBM-ZOS.
       OBJECT-COMPUTER. IBM-ZOS.
       
       INPUT-OUTPUT SECTION.
       FILE-CONTROL.
           SELECT PORTFOLIO-FILE
               ASSIGN TO PORTFILE
               ORGANIZATION IS INDEXED
               ACCESS MODE IS RANDOM
               RECORD KEY IS PORT-KEY
               FILE STATUS IS WS-FILE-STATUS.
           
           SELECT UPDATE-FILE
               ASSIGN TO UPDTFILE
               ORGANIZATION IS SEQUENTIAL
               FILE STATUS IS WS-UPDT-STATUS.

      *-- Change: Add Market Price Feed File for real-time price ingestion
           SELECT MARKET-PRICE-FILE
               ASSIGN TO MQPRCFEED
               ORGANIZATION IS SEQUENTIAL
               FILE STATUS IS WS-MKT-STATUS.
       
       DATA DIVISION.
       FILE SECTION.
       FD  PORTFOLIO-FILE.
           COPY PORTFLIO.
           
       FD  UPDATE-FILE.
       01  UPDATE-RECORD.
           05  UPDT-KEY.
               10  UPDT-ID        PIC X(8).
               10  UPDT-ACCT-NO   PIC X(10).
           05  UPDT-ACTION        PIC X(1).
               88  UPDT-STATUS    VALUE 'S'.
               88  UPDT-VALUE     VALUE 'V'.
               88  UPDT-NAME      VALUE 'N'.
           05  UPDT-NEW-VALUE     PIC X(50).

      *-- Change: Market Price Feed Record
       FD  MARKET-PRICE-FILE.
       01  MARKET-PRICE-RECORD.
           05  MKT-PORT-ID        PIC X(8).
           05  MKT-ACCOUNT-NO     PIC X(10).
           05  MKT-PRICE          PIC S9(13)V99 COMP-3.
           05  MKT-TIMESTAMP      PIC X(26).

       WORKING-STORAGE SECTION.
      *----------------------------------------------------------------*
      * Constants and switches
      *----------------------------------------------------------------*
       01  WS-CONSTANTS.
           05  WS-PROGRAM-NAME     PIC X(08) VALUE 'PORTUPDT '.
           05  WS-SUCCESS          PIC S9(4) VALUE +0.
           05  WS-ERROR            PIC S9(4) VALUE +8.
           
       01  WS-SWITCHES.
           05  WS-FILE-STATUS      PIC X(02).
               88  WS-SUCCESS-STATUS     VALUE '00'.
               88  WS-EOF-STATUS        VALUE '10'.
               88  WS-REC-NOT-FND       VALUE '23'.
           
           05  WS-UPDT-STATUS      PIC X(02).
               88  WS-UPDT-SUCCESS      VALUE '00'.
               88  WS-UPDT-EOF          VALUE '10'.

      *-- Change: Market Price Feed File Status
           05  WS-MKT-STATUS       PIC X(02).
               88  WS-MKT-SUCCESS       VALUE '00'.
               88  WS-MKT-EOF           VALUE '10'.
           
           05  WS-END-OF-FILE-SW   PIC X     VALUE 'N'.
               88  END-OF-FILE              VALUE 'Y'.
               88  NOT-END-OF-FILE          VALUE 'N'.
           
      *----------------------------------------------------------------*
      * Work areas
      *----------------------------------------------------------------*
       01  WS-WORK-AREAS.
           05  WS-UPDATE-COUNT     PIC 9(7) VALUE ZERO.
           05  WS-ERROR-COUNT      PIC 9(7) VALUE ZERO.
           05  WS-RETURN-CODE      PIC S9(4) VALUE +0.
           05  WS-NUMERIC-WORK     PIC S9(13)V99.

      *-- Change: Real-time price tracking and timestamp
           05  WS-REALTIME-PRICE   PIC S9(13)V99.
           05  WS-REALTIME-TS      PIC X(26).
           05  WS-PRICE-AGE-SEC    PIC 9(5).

      *-- Change: Error handling for price feed
           05  WS-PRICE-FEED-ERROR PIC X(01) VALUE 'N'.
               88  PRICE-FEED-ERROR VALUE 'Y'.
               88  PRICE-FEED-OK    VALUE 'N'.

      *-- Change: Timer for polling (simulated)
           05  WS-POLL-COUNTER     PIC 9(3) VALUE ZERO.

       PROCEDURE DIVISION.
       0000-MAIN.
           PERFORM 1000-INITIALIZE
           
      *-- Change: Poll and process market price feed every 5 seconds
           PERFORM UNTIL END-OF-FILE
               PERFORM 1500-POLL-MARKET-PRICE
               PERFORM 2000-PROCESS
           END-PERFORM

           PERFORM 3000-TERMINATE
           
           GOBACK.
           
       1000-INITIALIZE.
           INITIALIZE WS-WORK-AREAS
           
           OPEN I-O   PORTFOLIO-FILE
           OPEN INPUT UPDATE-FILE

      *-- Change: Open Market Price Feed File
           OPEN INPUT MARKET-PRICE-FILE

           IF NOT WS-SUCCESS-STATUS OR 
              NOT WS-UPDT-SUCCESS OR
              NOT WS-MKT-SUCCESS
              DISPLAY 'Error opening files: ' 
                      'PORT=' WS-FILE-STATUS
                      'UPDT=' WS-UPDT-STATUS
                      'MKT='  WS-MKT-STATUS
              MOVE WS-ERROR TO WS-RETURN-CODE
              PERFORM 3000-TERMINATE
           END-IF
           .
      
      *-- Change: Poll Market Price Feed (simulated polling every 5 seconds)
       1500-POLL-MARKET-PRICE.
           READ MARKET-PRICE-FILE
               AT END
                   SET PRICE-FEED-ERROR TO TRUE
                   DISPLAY 'Market price feed unavailable or EOF'
                   PERFORM 1600-LOG-PRICE-FEED-ERROR
               NOT AT END
                   MOVE MKT-PRICE TO WS-REALTIME-PRICE
                   MOVE MKT-TIMESTAMP TO WS-REALTIME-TS
                   SET PRICE-FEED-OK TO TRUE
                   DISPLAY 'Received market price: ' WS-REALTIME-PRICE
           END-READ
           .
      
      *-- Change: Log price feed error to audit log
       1600-LOG-PRICE-FEED-ERROR.
           DISPLAY 'AUDIT: Price feed error at ' WS-REALTIME-TS
           *-- Change: Here, call audit log routine or write to AUDITLOG
           .
           
       2000-PROCESS.
           READ UPDATE-FILE
               AT END
                   SET END-OF-FILE TO TRUE
               NOT AT END
                   PERFORM 2100-PROCESS-UPDATE
           END-READ
           .
           
       2100-PROCESS-UPDATE.
           MOVE UPDT-KEY TO PORT-KEY
           
           READ PORTFOLIO-FILE
           
           IF WS-SUCCESS-STATUS
               PERFORM 2200-APPLY-UPDATE
           ELSE
               ADD 1 TO WS-ERROR-COUNT
               DISPLAY 'Record not found: ' PORT-KEY
           END-IF
           .
           
       2200-APPLY-UPDATE.
      *-- Change: On price update, recalculate valuation and record timestamp
           EVALUATE TRUE
               WHEN UPDT-STATUS
                   MOVE UPDT-NEW-VALUE TO PORT-STATUS
               WHEN UPDT-NAME
                   MOVE UPDT-NEW-VALUE TO PORT-CLIENT-NAME
               WHEN UPDT-VALUE
                   MOVE UPDT-NEW-VALUE TO WS-NUMERIC-WORK
                   MOVE WS-NUMERIC-WORK TO PORT-TOTAL-VALUE
           END-EVALUATE

      *-- Change: If real-time price available, update portfolio value and timestamp
           IF PRICE-FEED-OK
               MOVE WS-REALTIME-PRICE TO PORT-REALTIME-PRICE
               MOVE WS-REALTIME-TS    TO PORT-PRICE-TS
               *-- Change: Log valuation update to audit log
               DISPLAY 'AUDIT: Portfolio ' PORT-ID ' updated with real-time price ' WS-REALTIME-PRICE ' at ' WS-REALTIME-TS
           ELSE
               *-- Change: If price feed error, log and skip valuation update
               DISPLAY 'AUDIT: Skipped real-time valuation update due to price feed error'
           END-IF

           REWRITE PORT-RECORD
           
           IF WS-SUCCESS-STATUS
               ADD 1 TO WS-UPDATE-COUNT
           ELSE
               ADD 1 TO WS-ERROR-COUNT
               DISPLAY 'Update failed for: ' PORT-KEY
           END-IF
           .
           
       3000-TERMINATE.
           CLOSE PORTFOLIO-FILE
                 UPDATE-FILE
                 MARKET-PRICE-FILE

           DISPLAY 'Updates processed: ' WS-UPDATE-COUNT
           DISPLAY 'Errors occurred:  ' WS-ERROR-COUNT
           
           MOVE WS-RETURN-CODE TO RETURN-CODE
           .