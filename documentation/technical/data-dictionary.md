# Investment Portfolio Management System - Data Dictionary

Version: 1.1
Last Updated: 2024-10-29

## Table of Contents

1. Common Fields Reference
2. File Structures
3. Database Tables
4. CICS Interfaces
5. Validation Rules
6. Error Codes
7. Batch Control Structures
8. Batch Processing Codes
9. Job Scheduling Dependencies

## 1. Common Fields Reference

### 1.1 Key Identifiers

| Field Name | Type    | Length | Description             | Valid Values/Format |
| ---------- | ------- | ------ | ----------------------- | ------------------- |
| ACCOUNT-NO | NUMERIC | 9      | Customer Account Number | 100000000-999999999 |
| FUND-ID    | CHAR    | 6      | Mutual Fund Identifier  | ALPHA-NUMERIC       |
| TRANS-ID   | CHAR    | 12     | Transaction Identifier  | YYYYMMDD + 4 digits |
| CUSIP      | CHAR    | 9      | Security Identifier     | ALPHA-NUMERIC       |

### 1.2 Common Data Elements

| Field Name | Type    | Length | Decimals | Description        | Valid Values        |
| ---------- | ------- | ------ | -------- | ------------------ | ------------------- |
| TRANS-DATE | NUMERIC | 8      | 0        | Transaction Date   | YYYYMMDD            |
| PROC-DATE  | NUMERIC | 8      | 0        | Processing Date    | YYYYMMDD            |
| AMOUNT     | NUMERIC | 11     | 2        | Transaction Amount | +/-99999999.99      |
| SHARE-QTY  | NUMERIC | 11     | 3        | Share Quantity     | +/-99999999.999     |
| PRICE      | NUMERIC | 9      | 4        | Share Price        | 0000.0000-9999.9999 |

## 2. File Structures

### 2.1 Transaction Input File (TRANFILE)

Type: Sequential File, Fixed Block
Record Length: 200 bytes

#### Record Layout

```cobol
01  TRANS-RECORD.
    05  TR-HEADER           PIC X(01).
        88  TR-DETAIL       VALUE 'D'.
        88  TR-TRAILER      VALUE 'T'.
    05  TR-DETAIL-REC       REDEFINES TR-HEADER.
        10  TR-ACCOUNT-NO   PIC 9(09).
        10  TR-FUND-ID      PIC X(06).
        10  TR-TRANS-TYPE   PIC X(02).
            88  TR-BUY      VALUE 'BY'.
            88  TR-SELL     VALUE 'SL'.
            88  TR-FEE      VALUE 'FE'.
        10  TR-TRANS-DATE   PIC 9(08).
        10  TR-TRANS-ID     PIC X(12).
        10  TR-SHARE-QTY    PIC S9(11)V999.
        10  TR-PRICE        PIC 9(5)V9999.
        10  TR-AMOUNT       PIC S9(11)V99.
        10  TR-STATUS       PIC X(01).
            88  TR-PENDING  VALUE 'P'.
            88  TR-COMPLETE VALUE 'C'.
            88  TR-ERROR    VALUE 'E'.
        10  FILLER          PIC X(131).
```

### 2.2 Position Master File (POSMSTRE)

Type: VSAM KSDS
Key: ACCOUNT-NO + FUND-ID
Record Length: 250 bytes

#### Record Layout

```cobol
01  POSITION-RECORD.
    05  POS-KEY.
        10  POS-ACCOUNT-NO  PIC 9(09).
        10  POS-FUND-ID     PIC X(06).
    05  POS-CUSIP          PIC X(09).
    05  POS-SHARE-BAL      PIC S9(11)V999.
    05  POS-AVG-COST       PIC 9(5)V9999.
    05  POS-COST-BASIS     PIC S9(11)V99.
    05  POS-LAST-DATE      PIC 9(08).
    05  POS-LAST-TRANS     PIC X(12).
    05  POS-STATUS         PIC X(01).
        88  POS-ACTIVE     VALUE 'A'.
        88  POS-CLOSED     VALUE 'C'.
    05  FILLER             PIC X(177).
```

### 2.3 Transaction History (TRANHIST)

Type: VSAM ESDS
Record Length: 300 bytes

#### Record Layout

```cobol
01  HISTORY-RECORD.
    05  HIST-TIMESTAMP     PIC X(26).
    05  HIST-ACCOUNT-NO    PIC 9(09).
    05  HIST-FUND-ID       PIC X(06).
    05  HIST-TRANS-ID      PIC X(12).
    05  HIST-TRANS-TYPE    PIC X(02).
    05  HIST-SHARE-QTY     PIC S9(11)V999.
    05  HIST-PRICE         PIC 9(5)V9999.
    05  HIST-AMOUNT        PIC S9(11)V99.
    05  HIST-RESULT-CODE   PIC X(04).
    05  HIST-BEFORE-BAL    PIC S9(11)V999.
    05  HIST-AFTER-BAL     PIC S9(11)V999.
    05  FILLER             PIC X(189).
```

### 2.4 Batch Control File (BCHCTL)

Type: VSAM KSDS
Key: PROCESS-DATE + PROCESS-ID
Record Length: 200 bytes

#### Record Layout

```cobol
01  BATCH-CONTROL-RECORD.
    05  BCH-KEY.
        10  BCH-PROCESS-DATE    PIC 9(08).
        10  BCH-PROCESS-ID      PIC X(08).
    05  BCH-STATUS              PIC X(01).
        88  BCH-WAITING         VALUE 'W'.
        88  BCH-IN-PROCESS      VALUE 'P'.
        88  BCH-COMPLETE        VALUE 'C'.
        88  BCH-ERROR           VALUE 'E'.
    05  BCH-START-TIME          PIC 9(08).
    05  BCH-END-TIME           PIC 9(08).
    05  BCH-RECORD-COUNT       PIC 9(09).
    05  BCH-ERROR-COUNT        PIC 9(09).
    05  BCH-LAST-POS           PIC 9(09).
    05  BCH-RETURN-CODE        PIC 9(04).
    05  BCH-MESSAGE            PIC X(50).
    05  FILLER                 PIC X(86).
```

### 2.5 Process Control File (PRCCTL)

Type: Sequential File, Fixed Block
Record Length: 80 bytes

#### Record Layout

```cobol
01  PROCESS-CONTROL-RECORD.
    05  PRC-DATE               PIC 9(08).
    05  PRC-SEQUENCE           PIC 9(04).
    05  PRC-PROGRAM-ID         PIC X(08).
    05  PRC-PROGRAM-DESC       PIC X(30).
    05  PRC-REQUIRED-RC        PIC 9(04).
    05  PRC-DEPENDENCY         PIC X(08).
    05  PRC-RESTART-IND        PIC X(01).
        88  PRC-RESTARTABLE    VALUE 'Y'.
        88  PRC-NO-RESTART     VALUE 'N'.
    05  FILLER                 PIC X(17).
```

### 2.6 Checkpoint/Restart Record (In VSAM BCHCTL)

```cobol
01  CHECKPOINT-RECORD.
    05  CHK-KEY.
        10  CHK-PROCESS-DATE   PIC 9(08).
        10  CHK-PROCESS-ID     PIC X(08).
    05  CHK-LAST-TRANS-ID     PIC X(12).
    05  CHK-LAST-ACCOUNT      PIC 9(09).
    05  CHK-LAST-FUND         PIC X(06).
    05  CHK-RECORDS-PROC      PIC 9(09).
    05  CHK-TIMESTAMP         PIC X(26).
    05  FILLER                PIC X(122).
```

## 3. Database Tables

### 3.1 Position History Table (POSHIST)

Type: DB2 Table

```sql
CREATE TABLE POSHIST (
    ACCOUNT_NO      DECIMAL(9,0)    NOT NULL,
    FUND_ID         CHAR(6)         NOT NULL,
    TRANS_DATE      DATE            NOT NULL,
    SHARE_BAL       DECIMAL(14,3)   NOT NULL,
    COST_BASIS      DECIMAL(13,2)   NOT NULL,
    AVG_COST        DECIMAL(9,4)    NOT NULL,
    PROC_TIMESTAMP  TIMESTAMP       NOT NULL,
    PRIMARY KEY (ACCOUNT_NO, FUND_ID, TRANS_DATE)
);
```

### 3.2 Error Log Table (ERRLOG)

Type: DB2 Table

```sql
CREATE TABLE ERRLOG (
    ERROR_TIMESTAMP TIMESTAMP       NOT NULL,
    PROGRAM_ID     CHAR(8)         NOT NULL,
    ERROR_CODE     CHAR(4)         NOT NULL,
    ACCOUNT_NO     DECIMAL(9,0),
    FUND_ID        CHAR(6),
    TRANS_ID       CHAR(12),
    ERROR_DESC     VARCHAR(100)    NOT NULL,
    PRIMARY KEY (ERROR_TIMESTAMP, PROGRAM_ID)
);
```

## 4. CICS Interfaces

### 4.1 Online Inquiry Commarea

```cobol
01  INQUERY-COMMAREA.
    05  INQ-FUNCTION      PIC X(01).
        88  INQ-POSITION  VALUE 'P'.
        88  INQ-HISTORY   VALUE 'H'.
    05  INQ-ACCOUNT-NO    PIC 9(09).
    05  INQ-FUND-ID       PIC X(06).
    05  INQ-RETURN-CODE   PIC X(02).
    05  INQ-MESSAGE       PIC X(50).
```

## 5. Validation Rules

### 5.1 Transaction Validation

- Account Number must be numeric and exist in customer master
- Fund ID must exist in fund master
- Transaction Date must not be future date
- Share Quantity must not be zero for BY/SL
- Amount must be non-zero for FE
- Price must be greater than zero for BY/SL

### 5.2 Position Validation

- Share Balance must not go negative
- Cost Basis must be updated for every BY/SL
- Average Cost must be recalculated for buys
- Position Status must be Active for transactions

## 6. Error Codes

| Code | Description                   | Severity | Action Required |
| ---- | ----------------------------- | -------- | --------------- |
| E001 | Invalid Account Number        | Error    | Reject          |
| E002 | Invalid Fund ID               | Error    | Reject          |
| E003 | Invalid Transaction Type      | Error    | Reject          |
| E004 | Insufficient Position Balance | Error    | Reject          |
| W001 | Zero Dollar Transaction       | Warning  | Process         |
| W002 | Duplicate Transaction ID      | Warning  | Log             |

## 7. Batch Control Structures

[Content already included in File Structures section]

## 8. Batch Processing Codes

### 8.1 Process IDs

| Process ID | Description            | Dependency | Restart |
| ---------- | ---------------------- | ---------- | ------- |
| TRNVAL00   | Transaction Validation | None       | Yes     |
| POSUPD00   | Position Update        | TRNVAL00   | Yes     |
| HISTLD00   | History Load to DB2    | POSUPD00   | Yes     |
| RPTGEN00   | Report Generation      | POSUPD00   | No      |

### 8.2 Return Codes

| Code | Description                  | Action           |
| ---- | ---------------------------- | ---------------- |
| 0000 | Successful completion        | Continue         |
| 0004 | Warning, processing complete | Review warnings  |
| 0008 | Errors, processing complete  | Review errors    |
| 0012 | Critical error, abend        | Immediate action |
| 0016 | Environment error            | System support   |

### 8.3 Checkpoint Frequency

- Transaction processor: Every 1000 records
- Position update: Every 500 updates
- History load: Every 1000 records
- Minimum checkpoint interval: 2 minutes

## 9. Job Scheduling Dependencies

### 9.1 Prerequisites

| Job Step | Prerequisite | Time Window | Condition        |
| -------- | ------------ | ----------- | ---------------- |
| TRNVAL00 | None         | 1800-1815   | Day must be open |
| POSUPD00 | TRNVAL00     | 1815-1900   | RC <= 0004       |
| HISTLD00 | POSUPD00     | 1900-1930   | RC <= 0004       |
| RPTGEN00 | HISTLD00     | 1930-2000   | None             |
