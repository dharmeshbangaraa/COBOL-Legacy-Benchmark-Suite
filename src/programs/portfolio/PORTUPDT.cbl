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
      * 2024-06-XX [COBOL Impact Modifier] Real-time price feed integration, dynamic valuation, error handling, audit logging *-- Change: Real-time price feed, valuation, error handling, audit logging
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
      *-- Change: Add real-time price feed file definition
           SELECT PRICEFEED-FILE
               ASSIGN TO PRICEFEED
               ORGANIZATION IS SEQUENTIAL
               FILE STATUS IS WS-PRICEFEED-STATUS.
       
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
      *-- Change: Add real-time price feed record
       FD  PRICEFEED-FILE.
       01  PRICEFEED-RECORD.
           05  PF-PORT-ID         PIC X(8).
           05  PF-PRICE           PIC S9(13)V99 COMP-3.
           05  PF-TIMESTAMP       PIC X(26).
           05  PF-STATUS          PIC X(1).
               88  PF-VALID       VALUE 'Y'.
               88  PF-INVALID     VALUE 'N'.
       
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
      *-- Change: Add price feed file status
           05  WS-PRICEFEED-STATUS PIC X(02).
               88  WS-PRICEFEED-SUCCESS VALUE '00'.
               88  WS-PRICEFEED-EOF     VALUE '10'.
           
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
      *-- Change: Add real-time price and timestamp work areas
           05  WS-REALTIME-PRICE   PIC S9(13)V99 COMP-3 VALUE ZERO.
           05  WS-REALTIME-TIMESTAMP PIC X(26) VALUE SPACES.
           05  WS-PRICE-FOUND      PIC X VALUE 'N'.
               88  PRICE-FOUND     VALUE 'Y'.
               88  PRICE-NOT-FOUND VALUE 'N'.
           05  WS-FEED-ERROR       PIC X VALUE 'N'.
               88  FEED-ERROR      VALUE 'Y'.
               88  NO-FEED-ERROR   VALUE 'N'.
      *-- Change: Add audit log record
       01  WS-AUDIT-RECORD.
           COPY AUDITLOG.
           
       PROCEDURE DIVISION.
       0000-MAIN.
           PERFORM 1000-INITIALIZE
           
           PERFORM 2000-PROCESS
              UNTIL END-OF-FILE
           
           PERFORM 3000-TERMINATE
           
           GOBACK.
           
       1000-INITIALIZE.
           INITIALIZE WS-WORK-AREAS
           
           OPEN I-O   PORTFOLIO-FILE
           OPEN INPUT UPDATE-FILE
      *-- Change: Open price feed file
           OPEN INPUT PRICEFEED-FILE
           
           IF NOT WS-SUCCESS-STATUS OR 
              NOT WS-UPDT-SUCCESS
              DISPLAY 'Error opening files: ' 
                      'PORT=' WS-FILE-STATUS
                      'UPDT=' WS-UPDT-STATUS
              MOVE WS-ERROR TO WS-RETURN-CODE
              PERFORM 3000-TERMINATE
           END-IF
      *-- Change: Check price feed file open status
           IF NOT WS-PRICEFEED-SUCCESS
              DISPLAY 'Error opening price feed file: ' WS-PRICEFEED-STATUS
              MOVE WS-ERROR TO WS-RETURN-CODE
              PERFORM 3000-TERMINATE
           END-IF
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
      *-- Change: Integrate real-time price feed lookup
               PERFORM 2150-GET-REALTIME-PRICE
               IF PRICE-FOUND
                   MOVE WS-REALTIME-PRICE TO PORT-REALTIME-PRICE
                   MOVE WS-REALTIME-TIMESTAMP TO PORT-PRICE-TIMESTAMP
               ELSE
                   SET FEED-ERROR TO TRUE
                   DISPLAY 'Real-time price not found for: ' PORT-KEY
      *-- Change: Audit feed failure
                   PERFORM 2500-AUDIT-FEED-FAILURE
               END-IF
               PERFORM 2200-APPLY-UPDATE
           ELSE
               ADD 1 TO WS-ERROR-COUNT
               DISPLAY 'Record not found: ' PORT-KEY
           END-IF
           .
      *-- Change: Real-time price feed lookup paragraph
       2150-GET-REALTIME-PRICE.
           MOVE 'N' TO WS-PRICE-FOUND
           MOVE 'N' TO WS-FEED-ERROR
           MOVE ZERO TO WS-REALTIME-PRICE
           MOVE SPACES TO WS-REALTIME-TIMESTAMP
           READ PRICEFEED-FILE
               AT END
                   SET WS-PRICEFEED-EOF TO TRUE
               NOT AT END
                   IF PF-PORT-ID = PORT-ID AND PF-VALID
                       MOVE PF-PRICE TO WS-REALTIME-PRICE
                       MOVE PF-TIMESTAMP TO WS-REALTIME-TIMESTAMP
                       SET PRICE-FOUND TO TRUE
                   ELSE
                       SET PRICE-NOT-FOUND TO TRUE
                   END-IF
           END-READ
           .
           
       2200-APPLY-UPDATE.
           EVALUATE TRUE
               WHEN UPDT-STATUS
                   MOVE UPDT-NEW-VALUE TO PORT-STATUS
               WHEN UPDT-NAME
                   MOVE UPDT-NEW-VALUE TO PORT-CLIENT-NAME
               WHEN UPDT-VALUE
                   MOVE UPDT-NEW-VALUE TO WS-NUMERIC-WORK
                   MOVE WS-NUMERIC-WORK TO PORT-TOTAL-VALUE
      *-- Change: Recalculate valuation using real-time price if available
                   IF PRICE-FOUND
                       COMPUTE PORT-TOTAL-VALUE = WS-REALTIME-PRICE
                   END-IF
           END-EVALUATE
           
           REWRITE PORT-RECORD
           
           IF WS-SUCCESS-STATUS
               ADD 1 TO WS-UPDATE-COUNT
      *-- Change: Audit successful update
               PERFORM 2600-AUDIT-REALTIME-UPDATE
           ELSE
               ADD 1 TO WS-ERROR-COUNT
               DISPLAY 'Update failed for: ' PORT-KEY
           END-IF
           .
      *-- Change: Audit feed failure paragraph
       2500-AUDIT-FEED-FAILURE.
           MOVE FUNCTION CURRENT-DATE TO AUD-TIMESTAMP OF WS-AUDIT-RECORD
           MOVE WS-PROGRAM-NAME TO AUD-PROGRAM OF WS-AUDIT-RECORD
           MOVE 'SYST' TO AUD-TYPE OF WS-AUDIT-RECORD
           MOVE 'FAIL' TO AUD-STATUS OF WS-AUDIT-RECORD
           MOVE PORT-ID TO AUD-PORTFOLIO-ID OF WS-AUDIT-RECORD
           MOVE PORT-ACCOUNT-NO TO AUD-ACCOUNT-NO OF WS-AUDIT-RECORD
           MOVE 'FEEDFAIL' TO AUD-ACTION OF WS-AUDIT-RECORD
           MOVE 'Real-time price feed failure' TO AUD-MESSAGE OF WS-AUDIT-RECORD
           .
      *-- Change: Audit real-time update paragraph
       2600-AUDIT-REALTIME-UPDATE.
           MOVE FUNCTION CURRENT-DATE TO AUD-TIMESTAMP OF WS-AUDIT-RECORD
           MOVE WS-PROGRAM-NAME TO AUD-PROGRAM OF WS-AUDIT-RECORD
           MOVE 'TRAN' TO AUD-TYPE OF WS-AUDIT-RECORD
           MOVE 'SUCC' TO AUD-STATUS OF WS-AUDIT-RECORD
           MOVE PORT-ID TO AUD-PORTFOLIO-ID OF WS-AUDIT-RECORD
           MOVE PORT-ACCOUNT-NO TO AUD-ACCOUNT-NO OF WS-AUDIT-RECORD
           MOVE 'UPDATE' TO AUD-ACTION OF WS-AUDIT-RECORD
           MOVE 'Real-time update applied' TO AUD-MESSAGE OF WS-AUDIT-RECORD
           .
           
       3000-TERMINATE.
           CLOSE PORTFOLIO-FILE
                 UPDATE-FILE
      *-- Change: Close price feed file
                 PRICEFEED-FILE
           
           DISPLAY 'Updates processed: ' WS-UPDATE-COUNT
           DISPLAY 'Errors occurred:  ' WS-ERROR-COUNT
           
           MOVE WS-RETURN-CODE TO RETURN-CODE
           .