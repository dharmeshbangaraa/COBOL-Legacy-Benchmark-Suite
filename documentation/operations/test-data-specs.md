# Test Data Specifications

## 1. File Record Examples

### 1.1 Portfolio Master Record (VSAM KSDS)

```cobol
01 PORTFOLIO-RECORD.
05 PORT-ID PIC X(10). *> Key field
05 PORT-NAME PIC X(50).
05 PORT-CREATE-DATE PIC X(10).
05 PORT-STATUS PIC X(01).
05 PORT-TOTAL-VALUE PIC S9(13)V99 COMP-3.
05 FILLER PIC X(24).
```

**Sample Test Records:**

```
PORT00001GROWTH PORTFOLIO                                   2024-03-20A0000012345678.99
PORT00002INCOME PORTFOLIO                                  2024-03-20A0000098765432.10
PORT00003BALANCED PORTFOLIO                                2024-03-20I0000005555555.55
```

### 1.2 Transaction Record (Sequential)

```cobol
01  TRANSACTION-RECORD.
    05  TXN-TIMESTAMP       PIC X(26).
    05  TXN-PORT-ID         PIC X(10).
    05  TXN-TYPE            PIC X(01).  *> B=Buy, S=Sell
    05  TXN-AMOUNT          PIC S9(11)V99 COMP-3.
    05  TXN-SECURITY-ID     PIC X(10).
    05  FILLER              PIC X(20).
```

**Sample Test Records:**

```
2024-03-20-15.30.45.123456PORT00001B0000012500.00IBM0000001
2024-03-20-15.31.12.789012PORT00002S0000005000.00MSFT000001
2024-03-20-15.32.01.456789PORT00003B0000007500.00AAPL000001
```

## 2. Test Case Specifications

### 2.1 Portfolio Management Test Cases

1. **Portfolio Creation**

   - Valid portfolio creation with all required fields
   - Attempt duplicate portfolio creation (should fail)
   - Create portfolio with minimum values
   - Create portfolio with maximum values

2. **Portfolio Updates**

   - Update portfolio name
   - Update portfolio status (Active to Inactive)
   - Update total value after transaction

3. **Transaction Processing**
   - Process valid buy transaction
   - Process valid sell transaction
   - Process transaction exceeding portfolio value
   - Process transaction with invalid security ID

### 2.2 Error Handling Test Cases

1. **File Handling Errors**

   - VSAM record not found
   - Duplicate key violation
   - End of file condition
   - File status error conditions

2. **Data Validation Errors**
   - Invalid portfolio ID format
   - Invalid transaction type
   - Invalid amount format
   - Missing required fields

## 3. Data Validation Criteria

### 3.1 Portfolio ID (PORT-ID)

- Format: 'PORT' followed by 5 digits
- Must be unique
- Required field
- Example: PORT00001

### 3.2 Portfolio Name (PORT-NAME)

- Length: 1-50 characters
- Required field
- Allowed characters: A-Z, 0-9, spaces, hyphens
- Must not be all spaces

### 3.3 Status Codes (PORT-STATUS)

- Valid values:
  - 'A' = Active
  - 'I' = Inactive
  - 'C' = Closed
- Required field

### 3.4 Transaction Types (TXN-TYPE)

- Valid values:
  - 'B' = Buy
  - 'S' = Sell
- Required field

### 3.5 Numeric Values

- Portfolio Total Value (PORT-TOTAL-VALUE)

  - Maximum: 9,999,999,999,999.99
  - Minimum: 0.00
  - Must be properly signed

- Transaction Amount (TXN-AMOUNT)
  - Maximum: 99,999,999,999.99
  - Minimum: 0.01
  - Must be properly signed

### 3.6 Date/Timestamp Fields

- Creation Date (PORT-CREATE-DATE)

  - Format: YYYY-MM-DD
  - Must be valid calendar date
  - Cannot be future date

- Transaction Timestamp (TXN-TIMESTAMP)
  - Format: YYYY-MM-DD-HH.MM.SS.MICSEC
  - Must be valid date/time
  - Cannot be future timestamp
