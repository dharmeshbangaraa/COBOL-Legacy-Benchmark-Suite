*================================================================*
      * Copybook Name: AUDITLOG
      * Description: Audit Trail Record Definitions
      * Author: [Author name]
      * Date Written: 2024-03-20
      * 2024-06-XX [COBOL Impact Modifier Agent] Real-time price feed event types and timestamp *-- Change: Added event types and timestamp for price feed integration
      *================================================================*
       01  AUDIT-RECORD.
           05  AUD-HEADER.
               10  AUD-TIMESTAMP     PIC X(26).
               10  AUD-SYSTEM-ID     PIC X(8).
               10  AUD-USER-ID       PIC X(8).
               10  AUD-PROGRAM       PIC X(8).
               10  AUD-TERMINAL      PIC X(8).
           05  AUD-TYPE             PIC X(4).
               88  AUD-TRANSACTION     VALUE 'TRAN'.
               88  AUD-USER-ACTION     VALUE 'USER'.
               88  AUD-SYSTEM-EVENT    VALUE 'SYST'.
      *-- Change: Add price feed event type
               88  AUD-PRICEFEED-EVENT VALUE 'PRCF'.
           05  AUD-ACTION           PIC X(8).
               88  AUD-CREATE         VALUE 'CREATE  '.
               88  AUD-UPDATE         VALUE 'UPDATE  '.
               88  AUD-DELETE         VALUE 'DELETE  '.
               88  AUD-INQUIRE        VALUE 'INQUIRE '.
               88  AUD-LOGIN          VALUE 'LOGIN   '.
               88  AUD-LOGOUT         VALUE 'LOGOUT  '.
               88  AUD-STARTUP        VALUE 'STARTUP '.
               88  AUD-SHUTDOWN       VALUE 'SHUTDOWN'.
      *-- Change: Add price feed action
               88  AUD-PRICEFEED      VALUE 'PRICEFD '.
           05  AUD-STATUS           PIC X(4).
               88  AUD-SUCCESS        VALUE 'SUCC'.
               88  AUD-FAILURE        VALUE 'FAIL'.
               88  AUD-WARNING        VALUE 'WARN'.
           05  AUD-KEY-INFO.
               10  AUD-PORTFOLIO-ID  PIC X(8).
               10  AUD-ACCOUNT-NO    PIC X(10).
      *-- Change: Add price feed timestamp
           05  AUD-PRICEFEED-TIME   PIC 9(14).
           05  AUD-BEFORE-IMAGE     PIC X(100).
           05  AUD-AFTER-IMAGE      PIC X(100).
           05  AUD-MESSAGE          PIC X(100).