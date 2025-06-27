export interface CobolExample {
  name: string;
  description: string;
  code: string;
}

export const cobolExamples: CobolExample[] = [
  {
    name: "Hello World",
    description: "A simple Hello World program",
    code: `       IDENTIFICATION DIVISION.
       PROGRAM-ID. HELLO-WORLD.
       
       PROCEDURE DIVISION.
       DISPLAY 'Hello, World!'.
       STOP RUN.`
  },
  {
    name: "Simple Addition",
    description: "Add two numbers and display result",
    code: `       IDENTIFICATION DIVISION.
       PROGRAM-ID. SIMPLE-ADD.
       
       DATA DIVISION.
       WORKING-STORAGE SECTION.
       01 NUM1 PIC 9(3) VALUE 100.
       01 NUM2 PIC 9(3) VALUE 200.
       01 RESULT PIC 9(4).
       
       PROCEDURE DIVISION.
       ADD NUM1 TO NUM2 GIVING RESULT.
       DISPLAY 'Result: ' RESULT.
       STOP RUN.`
  },
  {
    name: "Employee Record",
    description: "Define and display employee information",
    code: `       IDENTIFICATION DIVISION.
       PROGRAM-ID. EMPLOYEE-RECORD.
       
       DATA DIVISION.
       WORKING-STORAGE SECTION.
       01 EMPLOYEE-RECORD.
          05 EMP-ID PIC 9(5) VALUE 12345.
          05 EMP-NAME PIC X(30) VALUE 'JOHN DOE'.
          05 EMP-SALARY PIC 9(7)V99 VALUE 75000.00.
       
       PROCEDURE DIVISION.
       DISPLAY 'Employee ID: ' EMP-ID.
       DISPLAY 'Employee Name: ' EMP-NAME.
       DISPLAY 'Employee Salary: ' EMP-SALARY.
       STOP RUN.`
  },
  {
    name: "Simple Loop",
    description: "Display numbers 1 to 5 using PERFORM",
    code: `       IDENTIFICATION DIVISION.
       PROGRAM-ID. SIMPLE-LOOP.
       
       DATA DIVISION.
       WORKING-STORAGE SECTION.
       01 COUNTER PIC 9(2) VALUE 1.
       
       PROCEDURE DIVISION.
       PERFORM VARYING COUNTER FROM 1 BY 1 UNTIL COUNTER > 5
           DISPLAY 'Number: ' COUNTER
       END-PERFORM.
       STOP RUN.`
  },
  {
    name: "IF Statement",
    description: "Use conditional logic with IF-ELSE",
    code: `       IDENTIFICATION DIVISION.
       PROGRAM-ID. IF-EXAMPLE.
       
       DATA DIVISION.
       WORKING-STORAGE SECTION.
       01 GRADE PIC 9(3) VALUE 85.
       
       PROCEDURE DIVISION.
       IF GRADE >= 90
           DISPLAY 'Grade A'
       ELSE
           IF GRADE >= 80
               DISPLAY 'Grade B'
           ELSE
               DISPLAY 'Grade C or below'
           END-IF
       END-IF.
       STOP RUN.`
  },
  {
    name: "String Manipulation",
    description: "Concatenate and manipulate strings",
    code: `       IDENTIFICATION DIVISION.
       PROGRAM-ID. STRING-EXAMPLE.
       
       DATA DIVISION.
       WORKING-STORAGE SECTION.
       01 FIRST-NAME PIC X(10) VALUE 'JANE'.
       01 LAST-NAME PIC X(15) VALUE 'SMITH'.
       01 FULL-NAME PIC X(30).
       
       PROCEDURE DIVISION.
       STRING FIRST-NAME DELIMITED BY SPACE
              ' ' DELIMITED BY SIZE
              LAST-NAME DELIMITED BY SPACE
              INTO FULL-NAME
       END-STRING.
       DISPLAY 'Full Name: ' FULL-NAME.
       STOP RUN.`
  },
  {
    name: "Table Processing",
    description: "Work with arrays/tables",
    code: `       IDENTIFICATION DIVISION.
       PROGRAM-ID. TABLE-EXAMPLE.
       
       DATA DIVISION.
       WORKING-STORAGE SECTION.
       01 SALES-TABLE.
          05 SALES-ITEM OCCURS 3 TIMES PIC 9(5).
       01 TOTAL-SALES PIC 9(6) VALUE ZERO.
       01 INDEX-VAR PIC 9(2).
       
       PROCEDURE DIVISION.
       MOVE 1000 TO SALES-ITEM(1).
       MOVE 1500 TO SALES-ITEM(2).
       MOVE 2000 TO SALES-ITEM(3).
       
       PERFORM VARYING INDEX-VAR FROM 1 BY 1 UNTIL INDEX-VAR > 3
           ADD SALES-ITEM(INDEX-VAR) TO TOTAL-SALES
           DISPLAY 'Sales ' INDEX-VAR ': ' SALES-ITEM(INDEX-VAR)
       END-PERFORM.
       
       DISPLAY 'Total Sales: ' TOTAL-SALES.
       STOP RUN.`
  },
  {
    name: "File Input",
    description: "Read data from a file (conceptual)",
    code: `       IDENTIFICATION DIVISION.
       PROGRAM-ID. FILE-INPUT.
       
       ENVIRONMENT DIVISION.
       INPUT-OUTPUT SECTION.
       FILE-CONTROL.
           SELECT INPUT-FILE ASSIGN TO 'DATA.TXT'
           ORGANIZATION IS LINE SEQUENTIAL.
       
       DATA DIVISION.
       FILE SECTION.
       FD INPUT-FILE.
       01 INPUT-RECORD PIC X(80).
       
       WORKING-STORAGE SECTION.
       01 WS-EOF PIC X VALUE 'N'.
       
       PROCEDURE DIVISION.
       OPEN INPUT INPUT-FILE.
       PERFORM UNTIL WS-EOF = 'Y'
           READ INPUT-FILE
               AT END MOVE 'Y' TO WS-EOF
               NOT AT END DISPLAY INPUT-RECORD
           END-READ
       END-PERFORM.
       CLOSE INPUT-FILE.
       STOP RUN.`
  },
  {
    name: "Mathematical Operations",
    description: "Perform various mathematical calculations",
    code: `       IDENTIFICATION DIVISION.
       PROGRAM-ID. MATH-OPERATIONS.
       
       DATA DIVISION.
       WORKING-STORAGE SECTION.
       01 NUM-A PIC 9(3) VALUE 15.
       01 NUM-B PIC 9(3) VALUE 4.
       01 RESULT-ADD PIC 9(4).
       01 RESULT-SUB PIC S9(4).
       01 RESULT-MUL PIC 9(6).
       01 RESULT-DIV PIC 9(3)V99.
       
       PROCEDURE DIVISION.
       ADD NUM-A TO NUM-B GIVING RESULT-ADD.
       SUBTRACT NUM-B FROM NUM-A GIVING RESULT-SUB.
       MULTIPLY NUM-A BY NUM-B GIVING RESULT-MUL.
       DIVIDE NUM-A BY NUM-B GIVING RESULT-DIV.
       
       DISPLAY 'Addition: ' RESULT-ADD.
       DISPLAY 'Subtraction: ' RESULT-SUB.
       DISPLAY 'Multiplication: ' RESULT-MUL.
       DISPLAY 'Division: ' RESULT-DIV.
       STOP RUN.`
  },
  {
    name: "Date and Time",
    description: "Work with system date and time",
    code: `       IDENTIFICATION DIVISION.
       PROGRAM-ID. DATE-TIME.
       
       DATA DIVISION.
       WORKING-STORAGE SECTION.
       01 CURRENT-DATE-TIME.
          05 CURRENT-YEAR PIC 9(4).
          05 CURRENT-MONTH PIC 9(2).
          05 CURRENT-DAY PIC 9(2).
          05 CURRENT-HOUR PIC 9(2).
          05 CURRENT-MINUTE PIC 9(2).
          05 CURRENT-SECOND PIC 9(2).
       
       PROCEDURE DIVISION.
       MOVE FUNCTION CURRENT-DATE TO CURRENT-DATE-TIME.
       
       DISPLAY 'Current Date: ' CURRENT-MONTH '/' 
               CURRENT-DAY '/' CURRENT-YEAR.
       DISPLAY 'Current Time: ' CURRENT-HOUR ':' 
               CURRENT-MINUTE ':' CURRENT-SECOND.
       STOP RUN.`
  },
  {
    name: "Bank Account System",
    description: "Complex banking operations with account management",
    code: `       IDENTIFICATION DIVISION.
       PROGRAM-ID. BANK-ACCOUNT.
       
       DATA DIVISION.
       WORKING-STORAGE SECTION.
       01 ACCOUNT-RECORD.
          05 ACCOUNT-NUMBER PIC 9(8) VALUE 12345678.
          05 ACCOUNT-NAME PIC X(30) VALUE 'SMITH, JOHN'.
          05 ACCOUNT-BALANCE PIC 9(8)V99 VALUE 1500.75.
          05 ACCOUNT-TYPE PIC X(10) VALUE 'CHECKING'.
       
       01 TRANSACTION-RECORD.
          05 TRANS-TYPE PIC X(10).
          05 TRANS-AMOUNT PIC 9(6)V99.
          05 TRANS-DATE PIC 9(8).
       
       01 MENU-CHOICE PIC 9 VALUE 1.
       01 CONTINUE-FLAG PIC X VALUE 'Y'.
       
       PROCEDURE DIVISION.
       MAIN-MENU.
           PERFORM UNTIL CONTINUE-FLAG = 'N'
               DISPLAY 'BANK ACCOUNT SYSTEM'
               DISPLAY '1. Display Account Info'
               DISPLAY '2. Make Deposit'
               DISPLAY '3. Make Withdrawal'
               DISPLAY '4. Exit'
               DISPLAY 'Enter Choice: '
               MOVE 1 TO MENU-CHOICE
               
               EVALUATE MENU-CHOICE
                   WHEN 1 PERFORM DISPLAY-ACCOUNT
                   WHEN 2 PERFORM MAKE-DEPOSIT
                   WHEN 3 PERFORM MAKE-WITHDRAWAL
                   WHEN 4 MOVE 'N' TO CONTINUE-FLAG
                   WHEN OTHER DISPLAY 'Invalid Choice'
               END-EVALUATE
           END-PERFORM.
       
       DISPLAY-ACCOUNT.
           DISPLAY 'Account Number: ' ACCOUNT-NUMBER.
           DISPLAY 'Account Name: ' ACCOUNT-NAME.
           DISPLAY 'Account Type: ' ACCOUNT-TYPE.
           DISPLAY 'Current Balance: $' ACCOUNT-BALANCE.
       
       MAKE-DEPOSIT.
           MOVE 'DEPOSIT' TO TRANS-TYPE.
           MOVE 250.00 TO TRANS-AMOUNT.
           ADD TRANS-AMOUNT TO ACCOUNT-BALANCE.
           DISPLAY 'Deposited: $' TRANS-AMOUNT.
           DISPLAY 'New Balance: $' ACCOUNT-BALANCE.
       
       MAKE-WITHDRAWAL.
           MOVE 'WITHDRAWAL' TO TRANS-TYPE.
           MOVE 100.00 TO TRANS-AMOUNT.
           IF TRANS-AMOUNT <= ACCOUNT-BALANCE
               SUBTRACT TRANS-AMOUNT FROM ACCOUNT-BALANCE
               DISPLAY 'Withdrawn: $' TRANS-AMOUNT
               DISPLAY 'New Balance: $' ACCOUNT-BALANCE
           ELSE
               DISPLAY 'Insufficient funds'
           END-IF.
       
       STOP RUN.`
  },
  {
    name: "Inventory Management",
    description: "Stock management with multiple product categories",
    code: `       IDENTIFICATION DIVISION.
       PROGRAM-ID. INVENTORY-MGT.
       
       DATA DIVISION.
       WORKING-STORAGE SECTION.
       01 INVENTORY-TABLE.
          05 PRODUCT-ENTRY OCCURS 5 TIMES.
             10 PRODUCT-ID PIC 9(4).
             10 PRODUCT-NAME PIC X(20).
             10 PRODUCT-CATEGORY PIC X(15).
             10 QUANTITY-ON-HAND PIC 9(4).
             10 UNIT-PRICE PIC 9(4)V99.
             10 REORDER-LEVEL PIC 9(3).
       
       01 TOTALS.
          05 TOTAL-VALUE PIC 9(8)V99 VALUE ZERO.
          05 TOTAL-ITEMS PIC 9(6) VALUE ZERO.
          05 LOW-STOCK-COUNT PIC 9(3) VALUE ZERO.
       
       01 COUNTERS.
          05 I PIC 9(2).
          05 CATEGORY-COUNT PIC 9(2).
       
       01 CATEGORIES.
          05 CATEGORY-NAME OCCURS 3 TIMES PIC X(15).
       
       PROCEDURE DIVISION.
       INITIALIZE-DATA.
           MOVE 'ELECTRONICS' TO CATEGORY-NAME(1).
           MOVE 'CLOTHING' TO CATEGORY-NAME(2).
           MOVE 'BOOKS' TO CATEGORY-NAME(3).
           
           MOVE 1001 TO PRODUCT-ID(1).
           MOVE 'LAPTOP COMPUTER' TO PRODUCT-NAME(1).
           MOVE 'ELECTRONICS' TO PRODUCT-CATEGORY(1).
           MOVE 25 TO QUANTITY-ON-HAND(1).
           MOVE 899.99 TO UNIT-PRICE(1).
           MOVE 5 TO REORDER-LEVEL(1).
           
           MOVE 2001 TO PRODUCT-ID(2).
           MOVE 'DRESS SHIRT' TO PRODUCT-NAME(2).
           MOVE 'CLOTHING' TO PRODUCT-CATEGORY(2).
           MOVE 150 TO QUANTITY-ON-HAND(2).
           MOVE 45.50 TO UNIT-PRICE(2).
           MOVE 20 TO REORDER-LEVEL(2).
           
           MOVE 3001 TO PRODUCT-ID(3).
           MOVE 'PROGRAMMING BOOK' TO PRODUCT-NAME(3).
           MOVE 'BOOKS' TO PRODUCT-CATEGORY(3).
           MOVE 8 TO QUANTITY-ON-HAND(3).
           MOVE 79.95 TO UNIT-PRICE(3).
           MOVE 10 TO REORDER-LEVEL(3).
           
           MOVE 1002 TO PRODUCT-ID(4).
           MOVE 'SMARTPHONE' TO PRODUCT-NAME(4).
           MOVE 'ELECTRONICS' TO PRODUCT-CATEGORY(4).
           MOVE 50 TO QUANTITY-ON-HAND(4).
           MOVE 699.99 TO UNIT-PRICE(4).
           MOVE 10 TO REORDER-LEVEL(4).
           
           MOVE 2002 TO PRODUCT-ID(5).
           MOVE 'BLUE JEANS' TO PRODUCT-NAME(5).
           MOVE 'CLOTHING' TO PRODUCT-CATEGORY(5).
           MOVE 75 TO QUANTITY-ON-HAND(5).
           MOVE 89.99 TO UNIT-PRICE(5).
           MOVE 15 TO REORDER-LEVEL(5).
       
       PROCESS-INVENTORY.
           DISPLAY 'INVENTORY MANAGEMENT REPORT'.
           DISPLAY '================================'.
           
           PERFORM VARYING I FROM 1 BY 1 UNTIL I > 5
               DISPLAY 'Product ID: ' PRODUCT-ID(I)
               DISPLAY 'Name: ' PRODUCT-NAME(I)
               DISPLAY 'Category: ' PRODUCT-CATEGORY(I)
               DISPLAY 'Quantity: ' QUANTITY-ON-HAND(I)
               DISPLAY 'Unit Price: $' UNIT-PRICE(I)
               
               COMPUTE TOTAL-VALUE = TOTAL-VALUE + 
                   (QUANTITY-ON-HAND(I) * UNIT-PRICE(I))
               ADD QUANTITY-ON-HAND(I) TO TOTAL-ITEMS
               
               IF QUANTITY-ON-HAND(I) <= REORDER-LEVEL(I)
                   DISPLAY '*** LOW STOCK ALERT ***'
                   ADD 1 TO LOW-STOCK-COUNT
               END-IF
               
               DISPLAY ' '
           END-PERFORM.
           
           DISPLAY '================================'.
           DISPLAY 'SUMMARY:'.
           DISPLAY 'Total Inventory Value: $' TOTAL-VALUE.
           DISPLAY 'Total Items in Stock: ' TOTAL-ITEMS.
           DISPLAY 'Items Needing Reorder: ' LOW-STOCK-COUNT.
       
       STOP RUN.`
  },
  {
    name: "Payroll Processing",
    description: "Employee payroll calculation with taxes and deductions",
    code: `       IDENTIFICATION DIVISION.
       PROGRAM-ID. PAYROLL-SYSTEM.
       
       DATA DIVISION.
       WORKING-STORAGE SECTION.
       01 EMPLOYEE-TABLE.
          05 EMPLOYEE-RECORD OCCURS 3 TIMES.
             10 EMP-ID PIC 9(5).
             10 EMP-NAME PIC X(25).
             10 EMP-DEPARTMENT PIC X(15).
             10 HOURLY-RATE PIC 9(3)V99.
             10 HOURS-WORKED PIC 9(3)V99.
             10 OVERTIME-HOURS PIC 9(3)V99.
       
       01 PAYROLL-CALCULATIONS.
          05 GROSS-PAY PIC 9(6)V99.
          05 REGULAR-PAY PIC 9(6)V99.
          05 OVERTIME-PAY PIC 9(6)V99.
          05 FEDERAL-TAX PIC 9(5)V99.
          05 STATE-TAX PIC 9(5)V99.
          05 SOCIAL-SECURITY PIC 9(5)V99.
          05 TOTAL-DEDUCTIONS PIC 9(6)V99.
          05 NET-PAY PIC 9(6)V99.
       
       01 TAX-RATES.
          05 FEDERAL-TAX-RATE PIC V99 VALUE 0.15.
          05 STATE-TAX-RATE PIC V99 VALUE 0.05.
          05 SS-TAX-RATE PIC V999 VALUE 0.062.
          05 OVERTIME-MULTIPLIER PIC V9 VALUE 1.5.
       
       01 COMPANY-TOTALS.
          05 TOTAL-GROSS PIC 9(8)V99 VALUE ZERO.
          05 TOTAL-NET PIC 9(8)V99 VALUE ZERO.
          05 TOTAL-TAXES PIC 9(8)V99 VALUE ZERO.
       
       01 I PIC 9(2).
       
       PROCEDURE DIVISION.
       INITIALIZE-EMPLOYEES.
           MOVE 10001 TO EMP-ID(1).
           MOVE 'JOHNSON, MARY' TO EMP-NAME(1).
           MOVE 'ACCOUNTING' TO EMP-DEPARTMENT(1).
           MOVE 25.50 TO HOURLY-RATE(1).
           MOVE 40.00 TO HOURS-WORKED(1).
           MOVE 5.00 TO OVERTIME-HOURS(1).
           
           MOVE 10002 TO EMP-ID(2).
           MOVE 'SMITH, ROBERT' TO EMP-NAME(2).
           MOVE 'ENGINEERING' TO EMP-DEPARTMENT(2).
           MOVE 35.75 TO HOURLY-RATE(2).
           MOVE 45.00 TO HOURS-WORKED(2).
           MOVE 8.00 TO OVERTIME-HOURS(2).
           
           MOVE 10003 TO EMP-ID(3).
           MOVE 'WILLIAMS, SARAH' TO EMP-NAME(3).
           MOVE 'MARKETING' TO EMP-DEPARTMENT(3).
           MOVE 28.25 TO HOURLY-RATE(3).
           MOVE 38.00 TO HOURS-WORKED(3).
           MOVE 2.00 TO OVERTIME-HOURS(3).
       
       PROCESS-PAYROLL.
           DISPLAY 'PAYROLL PROCESSING SYSTEM'.
           DISPLAY '=========================='.
           
           PERFORM VARYING I FROM 1 BY 1 UNTIL I > 3
               PERFORM CALCULATE-PAY
               PERFORM PRINT-PAYSTUB
           END-PERFORM.
           
           PERFORM PRINT-SUMMARY.
       
       CALCULATE-PAY.
           COMPUTE REGULAR-PAY = HOURLY-RATE(I) * HOURS-WORKED(I).
           COMPUTE OVERTIME-PAY = HOURLY-RATE(I) * 
               OVERTIME-MULTIPLIER * OVERTIME-HOURS(I).
           COMPUTE GROSS-PAY = REGULAR-PAY + OVERTIME-PAY.
           
           COMPUTE FEDERAL-TAX = GROSS-PAY * FEDERAL-TAX-RATE.
           COMPUTE STATE-TAX = GROSS-PAY * STATE-TAX-RATE.
           COMPUTE SOCIAL-SECURITY = GROSS-PAY * SS-TAX-RATE.
           
           COMPUTE TOTAL-DEDUCTIONS = FEDERAL-TAX + STATE-TAX + 
               SOCIAL-SECURITY.
           COMPUTE NET-PAY = GROSS-PAY - TOTAL-DEDUCTIONS.
           
           ADD GROSS-PAY TO TOTAL-GROSS.
           ADD NET-PAY TO TOTAL-NET.
           ADD TOTAL-DEDUCTIONS TO TOTAL-TAXES.
       
       PRINT-PAYSTUB.
           DISPLAY 'Employee: ' EMP-NAME(I).
           DISPLAY 'ID: ' EMP-ID(I) ' Dept: ' EMP-DEPARTMENT(I).
           DISPLAY 'Regular Hours: ' HOURS-WORKED(I) 
               ' @ $' HOURLY-RATE(I).
           DISPLAY 'Overtime Hours: ' OVERTIME-HOURS(I) 
               ' @ $' HOURLY-RATE(I) ' x 1.5'.
           DISPLAY 'Gross Pay: $' GROSS-PAY.
           DISPLAY 'Federal Tax: $' FEDERAL-TAX.
           DISPLAY 'State Tax: $' STATE-TAX.
           DISPLAY 'Social Security: $' SOCIAL-SECURITY.
           DISPLAY 'Total Deductions: $' TOTAL-DEDUCTIONS.
           DISPLAY 'NET PAY: $' NET-PAY.
           DISPLAY '----------------------------'.
       
       PRINT-SUMMARY.
           DISPLAY 'PAYROLL SUMMARY'.
           DISPLAY 'Total Gross Pay: $' TOTAL-GROSS.
           DISPLAY 'Total Deductions: $' TOTAL-TAXES.
           DISPLAY 'Total Net Pay: $' TOTAL-NET.
       
       STOP RUN.`
  },
  {
    name: "Student Grade System",
    description: "Academic grade calculation with GPA computation",
    code: `       IDENTIFICATION DIVISION.
       PROGRAM-ID. GRADE-SYSTEM.
       
       DATA DIVISION.
       WORKING-STORAGE SECTION.
       01 STUDENT-TABLE.
          05 STUDENT-RECORD OCCURS 4 TIMES.
             10 STUDENT-ID PIC 9(6).
             10 STUDENT-NAME PIC X(25).
             10 STUDENT-MAJOR PIC X(20).
             10 COURSE-GRADES.
                15 GRADE OCCURS 5 TIMES PIC 9(3).
             10 CREDIT-HOURS OCCURS 5 TIMES PIC 9(2).
       
       01 GRADE-CALCULATIONS.
          05 TOTAL-POINTS PIC 9(4)V99.
          05 TOTAL-CREDITS PIC 9(3).
          05 GPA PIC 9V99.
          05 GRADE-POINTS PIC 9V99.
          05 LETTER-GRADE PIC X(2).
       
       01 CLASS-STATISTICS.
          05 CLASS-TOTAL-GPA PIC 9(3)V99 VALUE ZERO.
          05 HIGHEST-GPA PIC 9V99 VALUE ZERO.
          05 LOWEST-GPA PIC 9V99 VALUE 4.00.
          05 AVERAGE-GPA PIC 9V99.
       
       01 COURSE-NAMES.
          05 COURSE-NAME OCCURS 5 TIMES PIC X(20).
       
       01 COUNTERS.
          05 I PIC 9(2).
          05 J PIC 9(2).
          05 HONOR-ROLL-COUNT PIC 9(2) VALUE ZERO.
       
       PROCEDURE DIVISION.
       INITIALIZE-DATA.
           MOVE 'MATHEMATICS' TO COURSE-NAME(1).
           MOVE 'ENGLISH' TO COURSE-NAME(2).
           MOVE 'SCIENCE' TO COURSE-NAME(3).
           MOVE 'HISTORY' TO COURSE-NAME(4).
           MOVE 'COMPUTER SCIENCE' TO COURSE-NAME(5).
           
           MOVE 100001 TO STUDENT-ID(1).
           MOVE 'ANDERSON, ALICE' TO STUDENT-NAME(1).
           MOVE 'COMPUTER SCIENCE' TO STUDENT-MAJOR(1).
           MOVE 95 TO GRADE(1, 1).
           MOVE 88 TO GRADE(1, 2).
           MOVE 92 TO GRADE(1, 3).
           MOVE 85 TO GRADE(1, 4).
           MOVE 98 TO GRADE(1, 5).
           MOVE 3 TO CREDIT-HOURS(1, 1).
           MOVE 3 TO CREDIT-HOURS(1, 2).
           MOVE 4 TO CREDIT-HOURS(1, 3).
           MOVE 3 TO CREDIT-HOURS(1, 4).
           MOVE 4 TO CREDIT-HOURS(1, 5).
           
           MOVE 100002 TO STUDENT-ID(2).
           MOVE 'BROWN, BRIAN' TO STUDENT-NAME(2).
           MOVE 'BUSINESS' TO STUDENT-MAJOR(2).
           MOVE 78 TO GRADE(2, 1).
           MOVE 85 TO GRADE(2, 2).
           MOVE 80 TO GRADE(2, 3).
           MOVE 88 TO GRADE(2, 4).
           MOVE 82 TO GRADE(2, 5).
           MOVE 3 TO CREDIT-HOURS(2, 1).
           MOVE 3 TO CREDIT-HOURS(2, 2).
           MOVE 4 TO CREDIT-HOURS(2, 3).
           MOVE 3 TO CREDIT-HOURS(2, 4).
           MOVE 4 TO CREDIT-HOURS(2, 5).
           
           MOVE 100003 TO STUDENT-ID(3).
           MOVE 'DAVIS, CAROL' TO STUDENT-NAME(3).
           MOVE 'ENGINEERING' TO STUDENT-MAJOR(3).
           MOVE 90 TO GRADE(3, 1).
           MOVE 93 TO GRADE(3, 2).
           MOVE 87 TO GRADE(3, 3).
           MOVE 91 TO GRADE(3, 4).
           MOVE 89 TO GRADE(3, 5).
           MOVE 3 TO CREDIT-HOURS(3, 1).
           MOVE 3 TO CREDIT-HOURS(3, 2).
           MOVE 4 TO CREDIT-HOURS(3, 3).
           MOVE 3 TO CREDIT-HOURS(3, 4).
           MOVE 4 TO CREDIT-HOURS(3, 5).
           
           MOVE 100004 TO STUDENT-ID(4).
           MOVE 'EVANS, DAVID' TO STUDENT-NAME(4).
           MOVE 'LIBERAL ARTS' TO STUDENT-MAJOR(4).
           MOVE 72 TO GRADE(4, 1).
           MOVE 78 TO GRADE(4, 2).
           MOVE 75 TO GRADE(4, 3).
           MOVE 80 TO GRADE(4, 4).
           MOVE 77 TO GRADE(4, 5).
           MOVE 3 TO CREDIT-HOURS(4, 1).
           MOVE 3 TO CREDIT-HOURS(4, 2).
           MOVE 4 TO CREDIT-HOURS(4, 3).
           MOVE 3 TO CREDIT-HOURS(4, 4).
           MOVE 4 TO CREDIT-HOURS(4, 5).
       
       PROCESS-GRADES.
           DISPLAY 'STUDENT GRADE REPORT SYSTEM'.
           DISPLAY '============================'.
           
           PERFORM VARYING I FROM 1 BY 1 UNTIL I > 4
               PERFORM CALCULATE-GPA
               PERFORM PRINT-TRANSCRIPT
               PERFORM UPDATE-CLASS-STATS
           END-PERFORM.
           
           PERFORM PRINT-CLASS-SUMMARY.
       
       CALCULATE-GPA.
           MOVE ZERO TO TOTAL-POINTS.
           MOVE ZERO TO TOTAL-CREDITS.
           
           PERFORM VARYING J FROM 1 BY 1 UNTIL J > 5
               PERFORM CONVERT-TO-POINTS
               COMPUTE TOTAL-POINTS = TOTAL-POINTS + 
                   (GRADE-POINTS * CREDIT-HOURS(I, J))
               ADD CREDIT-HOURS(I, J) TO TOTAL-CREDITS
           END-PERFORM.
           
           IF TOTAL-CREDITS > 0
               COMPUTE GPA = TOTAL-POINTS / TOTAL-CREDITS
           ELSE
               MOVE ZERO TO GPA
           END-IF.
       
       CONVERT-TO-POINTS.
           EVALUATE GRADE(I, J)
               WHEN 97 THRU 100
                   MOVE 4.00 TO GRADE-POINTS
                   MOVE 'A+' TO LETTER-GRADE
               WHEN 93 THRU 96
                   MOVE 4.00 TO GRADE-POINTS
                   MOVE 'A' TO LETTER-GRADE
               WHEN 90 THRU 92
                   MOVE 3.70 TO GRADE-POINTS
                   MOVE 'A-' TO LETTER-GRADE
               WHEN 87 THRU 89
                   MOVE 3.30 TO GRADE-POINTS
                   MOVE 'B+' TO LETTER-GRADE
               WHEN 83 THRU 86
                   MOVE 3.00 TO GRADE-POINTS
                   MOVE 'B' TO LETTER-GRADE
               WHEN 80 THRU 82
                   MOVE 2.70 TO GRADE-POINTS
                   MOVE 'B-' TO LETTER-GRADE
               WHEN 77 THRU 79
                   MOVE 2.30 TO GRADE-POINTS
                   MOVE 'C+' TO LETTER-GRADE
               WHEN 73 THRU 76
                   MOVE 2.00 TO GRADE-POINTS
                   MOVE 'C' TO LETTER-GRADE
               WHEN 70 THRU 72
                   MOVE 1.70 TO GRADE-POINTS
                   MOVE 'C-' TO LETTER-GRADE
               WHEN 67 THRU 69
                   MOVE 1.30 TO GRADE-POINTS
                   MOVE 'D+' TO LETTER-GRADE
               WHEN 65 THRU 66
                   MOVE 1.00 TO GRADE-POINTS
                   MOVE 'D' TO LETTER-GRADE
               WHEN OTHER
                   MOVE 0.00 TO GRADE-POINTS
                   MOVE 'F' TO LETTER-GRADE
           END-EVALUATE.
       
       PRINT-TRANSCRIPT.
           DISPLAY 'Student: ' STUDENT-NAME(I).
           DISPLAY 'ID: ' STUDENT-ID(I) ' Major: ' STUDENT-MAJOR(I).
           DISPLAY 'COURSES:'.
           
           PERFORM VARYING J FROM 1 BY 1 UNTIL J > 5
               PERFORM CONVERT-TO-POINTS
               DISPLAY '  ' COURSE-NAME(J) ': ' GRADE(I, J) 
                   ' (' LETTER-GRADE ') ' CREDIT-HOURS(I, J) ' credits'
           END-PERFORM.
           
           DISPLAY 'SEMESTER GPA: ' GPA.
           IF GPA >= 3.50
               DISPLAY '*** HONOR ROLL ***'
           END-IF.
           DISPLAY '----------------------------'.
       
       UPDATE-CLASS-STATS.
           ADD GPA TO CLASS-TOTAL-GPA.
           IF GPA > HIGHEST-GPA
               MOVE GPA TO HIGHEST-GPA
           END-IF.
           IF GPA < LOWEST-GPA
               MOVE GPA TO LOWEST-GPA
           END-IF.
           IF GPA >= 3.50
               ADD 1 TO HONOR-ROLL-COUNT
           END-IF.
       
       PRINT-CLASS-SUMMARY.
           COMPUTE AVERAGE-GPA = CLASS-TOTAL-GPA / 4.
           DISPLAY 'CLASS STATISTICS'.
           DISPLAY 'Average GPA: ' AVERAGE-GPA.
           DISPLAY 'Highest GPA: ' HIGHEST-GPA.
           DISPLAY 'Lowest GPA: ' LOWEST-GPA.
           DISPLAY 'Honor Roll Students: ' HONOR-ROLL-COUNT.
       
       STOP RUN.`
  },
  {
    name: "Library Management",
    description: "Book tracking system with checkout/return functionality",
    code: `       IDENTIFICATION DIVISION.
       PROGRAM-ID. LIBRARY-SYSTEM.
       
       DATA DIVISION.
       WORKING-STORAGE SECTION.
       01 BOOK-CATALOG.
          05 BOOK-RECORD OCCURS 6 TIMES.
             10 BOOK-ID PIC 9(6).
             10 BOOK-TITLE PIC X(30).
             10 BOOK-AUTHOR PIC X(25).
             10 BOOK-CATEGORY PIC X(15).
             10 BOOK-STATUS PIC X(10).
             10 CHECKOUT-DATE PIC 9(8).
             10 DUE-DATE PIC 9(8).
             10 BORROWER-ID PIC 9(6).
       
       01 LIBRARY-STATISTICS.
          05 TOTAL-BOOKS PIC 9(4) VALUE 6.
          05 AVAILABLE-BOOKS PIC 9(4) VALUE ZERO.
          05 CHECKED-OUT-BOOKS PIC 9(4) VALUE ZERO.
          05 OVERDUE-BOOKS PIC 9(4) VALUE ZERO.
       
       01 CATEGORIES.
          05 CATEGORY-COUNT OCCURS 4 TIMES.
             10 CATEGORY-NAME PIC X(15).
             10 CATEGORY-TOTAL PIC 9(3) VALUE ZERO.
       
       01 CURRENT-DATE-NUM PIC 9(8) VALUE 20240315.
       01 TEMP-DATE PIC 9(8).
       01 DAYS-OVERDUE PIC 9(3).
       
       01 COUNTERS.
          05 I PIC 9(2).
          05 J PIC 9(2).
       
       PROCEDURE DIVISION.
       INITIALIZE-LIBRARY.
           MOVE 'FICTION' TO CATEGORY-NAME(1).
           MOVE 'NON-FICTION' TO CATEGORY-NAME(2).
           MOVE 'SCIENCE' TO CATEGORY-NAME(3).
           MOVE 'BIOGRAPHY' TO CATEGORY-NAME(4).
           
           MOVE 100001 TO BOOK-ID(1).
           MOVE 'TO KILL A MOCKINGBIRD' TO BOOK-TITLE(1).
           MOVE 'HARPER LEE' TO BOOK-AUTHOR(1).
           MOVE 'FICTION' TO BOOK-CATEGORY(1).
           MOVE 'AVAILABLE' TO BOOK-STATUS(1).
           MOVE ZERO TO CHECKOUT-DATE(1).
           MOVE ZERO TO DUE-DATE(1).
           MOVE ZERO TO BORROWER-ID(1).
           
           MOVE 100002 TO BOOK-ID(2).
           MOVE 'THE GREAT GATSBY' TO BOOK-TITLE(2).
           MOVE 'F. SCOTT FITZGERALD' TO BOOK-AUTHOR(2).
           MOVE 'FICTION' TO BOOK-CATEGORY(2).
           MOVE 'CHECKED OUT' TO BOOK-STATUS(2).
           MOVE 20240301 TO CHECKOUT-DATE(2).
           MOVE 20240315 TO DUE-DATE(2).
           MOVE 200001 TO BORROWER-ID(2).
           
           MOVE 100003 TO BOOK-ID(3).
           MOVE 'A BRIEF HISTORY OF TIME' TO BOOK-TITLE(3).
           MOVE 'STEPHEN HAWKING' TO BOOK-AUTHOR(3).
           MOVE 'SCIENCE' TO BOOK-CATEGORY(3).
           MOVE 'CHECKED OUT' TO BOOK-STATUS(3).
           MOVE 20240220 TO CHECKOUT-DATE(3).
           MOVE 20240306 TO DUE-DATE(3).
           MOVE 200002 TO BORROWER-ID(3).
           
           MOVE 100004 TO BOOK-ID(4).
           MOVE 'STEVE JOBS' TO BOOK-TITLE(4).
           MOVE 'WALTER ISAACSON' TO BOOK-AUTHOR(4).
           MOVE 'BIOGRAPHY' TO BOOK-CATEGORY(4).
           MOVE 'AVAILABLE' TO BOOK-STATUS(4).
           MOVE ZERO TO CHECKOUT-DATE(4).
           MOVE ZERO TO DUE-DATE(4).
           MOVE ZERO TO BORROWER-ID(4).
           
           MOVE 100005 TO BOOK-ID(5).
           MOVE 'THE LEAN STARTUP' TO BOOK-TITLE(5).
           MOVE 'ERIC RIES' TO BOOK-AUTHOR(5).
           MOVE 'NON-FICTION' TO BOOK-CATEGORY(5).
           MOVE 'AVAILABLE' TO BOOK-STATUS(5).
           MOVE ZERO TO CHECKOUT-DATE(5).
           MOVE ZERO TO DUE-DATE(5).
           MOVE ZERO TO BORROWER-ID(5).
           
           MOVE 100006 TO BOOK-ID(6).
           MOVE 'EINSTEIN: HIS LIFE' TO BOOK-TITLE(6).
           MOVE 'WALTER ISAACSON' TO BOOK-AUTHOR(6).
           MOVE 'BIOGRAPHY' TO BOOK-CATEGORY(6).
           MOVE 'CHECKED OUT' TO BOOK-STATUS(6).
           MOVE 20240310 TO CHECKOUT-DATE(6).
           MOVE 20240324 TO DUE-DATE(6).
           MOVE 200003 TO BORROWER-ID(6).
       
       PROCESS-LIBRARY.
           DISPLAY 'LIBRARY MANAGEMENT SYSTEM'.
           DISPLAY '=========================='.
           
           PERFORM CALCULATE-STATISTICS.
           PERFORM DISPLAY-CATALOG.
           PERFORM DISPLAY-OVERDUE-BOOKS.
           PERFORM DISPLAY-STATISTICS.
           PERFORM DISPLAY-CATEGORY-REPORT.
       
       CALCULATE-STATISTICS.
           PERFORM VARYING I FROM 1 BY 1 UNTIL I > TOTAL-BOOKS
               IF BOOK-STATUS(I) = 'AVAILABLE'
                   ADD 1 TO AVAILABLE-BOOKS
               ELSE
                   ADD 1 TO CHECKED-OUT-BOOKS
                   IF DUE-DATE(I) < CURRENT-DATE-NUM
                       ADD 1 TO OVERDUE-BOOKS
                   END-IF
               END-IF
               
               PERFORM VARYING J FROM 1 BY 1 UNTIL J > 4
                   IF BOOK-CATEGORY(I) = CATEGORY-NAME(J)
                       ADD 1 TO CATEGORY-TOTAL(J)
                   END-IF
               END-PERFORM
           END-PERFORM.
       
       DISPLAY-CATALOG.
           DISPLAY 'BOOK CATALOG'.
           DISPLAY '============'.
           
           PERFORM VARYING I FROM 1 BY 1 UNTIL I > TOTAL-BOOKS
               DISPLAY 'ID: ' BOOK-ID(I)
               DISPLAY 'Title: ' BOOK-TITLE(I)
               DISPLAY 'Author: ' BOOK-AUTHOR(I)
               DISPLAY 'Category: ' BOOK-CATEGORY(I)
               DISPLAY 'Status: ' BOOK-STATUS(I)
               
               IF BOOK-STATUS(I) = 'CHECKED OUT'
                   DISPLAY 'Borrower ID: ' BORROWER-ID(I)
                   DISPLAY 'Checkout Date: ' CHECKOUT-DATE(I)
                   DISPLAY 'Due Date: ' DUE-DATE(I)
                   
                   IF DUE-DATE(I) < CURRENT-DATE-NUM
                       DISPLAY '*** OVERDUE ***'
                   END-IF
               END-IF
               
               DISPLAY '----------------------------'
           END-PERFORM.
       
       DISPLAY-OVERDUE-BOOKS.
           DISPLAY 'OVERDUE BOOKS REPORT'.
           DISPLAY '===================='.
           
           PERFORM VARYING I FROM 1 BY 1 UNTIL I > TOTAL-BOOKS
               IF BOOK-STATUS(I) = 'CHECKED OUT' AND 
                  DUE-DATE(I) < CURRENT-DATE-NUM
                   DISPLAY 'OVERDUE: ' BOOK-TITLE(I)
                   DISPLAY 'Borrower: ' BORROWER-ID(I)
                   DISPLAY 'Due Date: ' DUE-DATE(I)
                   COMPUTE DAYS-OVERDUE = CURRENT-DATE-NUM - DUE-DATE(I)
                   DISPLAY 'Days Overdue: ' DAYS-OVERDUE
                   DISPLAY '----------------------------'
               END-IF
           END-PERFORM.
       
       DISPLAY-STATISTICS.
           DISPLAY 'LIBRARY STATISTICS'.
           DISPLAY '=================='.
           DISPLAY 'Total Books: ' TOTAL-BOOKS.
           DISPLAY 'Available Books: ' AVAILABLE-BOOKS.
           DISPLAY 'Checked Out Books: ' CHECKED-OUT-BOOKS.
           DISPLAY 'Overdue Books: ' OVERDUE-BOOKS.
           DISPLAY ' '.
       
       DISPLAY-CATEGORY-REPORT.
           DISPLAY 'BOOKS BY CATEGORY'.
           DISPLAY '================='.
           
           PERFORM VARYING J FROM 1 BY 1 UNTIL J > 4
               DISPLAY CATEGORY-NAME(J) ': ' CATEGORY-TOTAL(J) ' books'
           END-PERFORM.
       
       STOP RUN.`
  },
  {
    name: "Sales Commission",
    description: "Complex commission calculation with tiered rates",
    code: `       IDENTIFICATION DIVISION.
       PROGRAM-ID. SALES-COMMISSION.
       
       DATA DIVISION.
       WORKING-STORAGE SECTION.
       01 SALESPERSON-TABLE.
          05 SALESPERSON-RECORD OCCURS 4 TIMES.
             10 SALES-ID PIC 9(4).
             10 SALES-NAME PIC X(25).
             10 SALES-REGION PIC X(15).
             10 SALES-AMOUNT PIC 9(7)V99.
             10 YEARS-EXPERIENCE PIC 9(2).
             10 SALES-CATEGORY PIC X(10).
       
       01 COMMISSION-STRUCTURE.
          05 TIER-1-RATE PIC V999 VALUE 0.025.
          05 TIER-2-RATE PIC V999 VALUE 0.035.
          05 TIER-3-RATE PIC V999 VALUE 0.050.
          05 TIER-4-RATE PIC V999 VALUE 0.065.
          05 TIER-1-LIMIT PIC 9(6) VALUE 50000.
          05 TIER-2-LIMIT PIC 9(6) VALUE 100000.
          05 TIER-3-LIMIT PIC 9(6) VALUE 200000.
          05 EXPERIENCE-BONUS PIC V999 VALUE 0.005.
       
       01 COMMISSION-CALCULATIONS.
          05 BASE-COMMISSION PIC 9(6)V99.
          05 TIER-1-COMM PIC 9(6)V99.
          05 TIER-2-COMM PIC 9(6)V99.
          05 TIER-3-COMM PIC 9(6)V99.
          05 TIER-4-COMM PIC 9(6)V99.
          05 EXPERIENCE-BONUS-AMT PIC 9(5)V99.
          05 TOTAL-COMMISSION PIC 9(7)V99.
       
       01 COMPANY-TOTALS.
          05 TOTAL-SALES PIC 9(9)V99 VALUE ZERO.
          05 TOTAL-COMMISSIONS PIC 9(8)V99 VALUE ZERO.
          05 TOP-PERFORMER-SALES PIC 9(7)V99 VALUE ZERO.
          05 TOP-PERFORMER-NAME PIC X(25).
       
       01 REGIONAL-TOTALS.
          05 REGION-ENTRY OCCURS 3 TIMES.
             10 REGION-NAME PIC X(15).
             10 REGION-SALES PIC 9(8)V99 VALUE ZERO.
             10 REGION-COMMISSION PIC 9(7)V99 VALUE ZERO.
       
       01 COUNTERS.
          05 I PIC 9(2).
          05 J PIC 9(2).
          05 TOP-PERFORMER-COUNT PIC 9(2) VALUE ZERO.
       
       PROCEDURE DIVISION.
       INITIALIZE-DATA.
           MOVE 'NORTH' TO REGION-NAME(1).
           MOVE 'SOUTH' TO REGION-NAME(2).
           MOVE 'WEST' TO REGION-NAME(3).
           
           MOVE 1001 TO SALES-ID(1).
           MOVE 'JOHNSON, MICHAEL' TO SALES-NAME(1).
           MOVE 'NORTH' TO SALES-REGION(1).
           MOVE 125000.00 TO SALES-AMOUNT(1).
           MOVE 8 TO YEARS-EXPERIENCE(1).
           MOVE 'SENIOR' TO SALES-CATEGORY(1).
           
           MOVE 1002 TO SALES-ID(2).
           MOVE 'WILLIAMS, SARAH' TO SALES-NAME(2).
           MOVE 'SOUTH' TO SALES-REGION(2).
           MOVE 87500.00 TO SALES-AMOUNT(2).
           MOVE 5 TO YEARS-EXPERIENCE(2).
           MOVE 'REGULAR' TO SALES-CATEGORY(2).
           
           MOVE 1003 TO SALES-ID(3).
           MOVE 'BROWN, DAVID' TO SALES-NAME(3).
           MOVE 'WEST' TO SALES-REGION(3).
           MOVE 245000.00 TO SALES-AMOUNT(3).
           MOVE 12 TO YEARS-EXPERIENCE(3).
           MOVE 'SENIOR' TO SALES-CATEGORY(3).
           
           MOVE 1004 TO SALES-ID(4).
           MOVE 'DAVIS, JENNIFER' TO SALES-NAME(4).
           MOVE 'NORTH' TO SALES-REGION(4).
           MOVE 156000.00 TO SALES-AMOUNT(4).
           MOVE 7 TO YEARS-EXPERIENCE(4).
           MOVE 'REGULAR' TO SALES-CATEGORY(4).
       
       PROCESS-COMMISSIONS.
           DISPLAY 'SALES COMMISSION REPORT'.
           DISPLAY '======================='.
           
           PERFORM VARYING I FROM 1 BY 1 UNTIL I > 4
               PERFORM CALCULATE-COMMISSION
               PERFORM DISPLAY-SALESPERSON-REPORT
               PERFORM UPDATE-TOTALS
           END-PERFORM.
           
           PERFORM DISPLAY-SUMMARY-REPORTS.
       
       CALCULATE-COMMISSION.
           MOVE ZERO TO BASE-COMMISSION.
           MOVE ZERO TO TIER-1-COMM.
           MOVE ZERO TO TIER-2-COMM.
           MOVE ZERO TO TIER-3-COMM.
           MOVE ZERO TO TIER-4-COMM.
           MOVE ZERO TO EXPERIENCE-BONUS-AMT.
           
           IF SALES-AMOUNT(I) <= TIER-1-LIMIT
               COMPUTE TIER-1-COMM = SALES-AMOUNT(I) * TIER-1-RATE
           ELSE
               COMPUTE TIER-1-COMM = TIER-1-LIMIT * TIER-1-RATE
               
               IF SALES-AMOUNT(I) <= TIER-2-LIMIT
                   COMPUTE TIER-2-COMM = (SALES-AMOUNT(I) - TIER-1-LIMIT) 
                       * TIER-2-RATE
               ELSE
                   COMPUTE TIER-2-COMM = (TIER-2-LIMIT - TIER-1-LIMIT) 
                       * TIER-2-RATE
                   
                   IF SALES-AMOUNT(I) <= TIER-3-LIMIT
                       COMPUTE TIER-3-COMM = (SALES-AMOUNT(I) - TIER-2-LIMIT) 
                           * TIER-3-RATE
                   ELSE
                       COMPUTE TIER-3-COMM = (TIER-3-LIMIT - TIER-2-LIMIT) 
                           * TIER-3-RATE
                       COMPUTE TIER-4-COMM = (SALES-AMOUNT(I) - TIER-3-LIMIT) 
                           * TIER-4-RATE
                   END-IF
               END-IF
           END-IF.
           
           COMPUTE BASE-COMMISSION = TIER-1-COMM + TIER-2-COMM + 
               TIER-3-COMM + TIER-4-COMM.
           
           IF YEARS-EXPERIENCE(I) >= 5
               COMPUTE EXPERIENCE-BONUS-AMT = SALES-AMOUNT(I) * 
                   EXPERIENCE-BONUS
           END-IF.
           
           COMPUTE TOTAL-COMMISSION = BASE-COMMISSION + 
               EXPERIENCE-BONUS-AMT.
       
       DISPLAY-SALESPERSON-REPORT.
           DISPLAY 'Salesperson: ' SALES-NAME(I).
           DISPLAY 'ID: ' SALES-ID(I) ' Region: ' SALES-REGION(I).
           DISPLAY 'Category: ' SALES-CATEGORY(I) 
               ' Experience: ' YEARS-EXPERIENCE(I) ' years'.
           DISPLAY 'Total Sales: $' SALES-AMOUNT(I).
           DISPLAY 'Commission Breakdown:'.
           DISPLAY '  Tier 1 (2.5%): $' TIER-1-COMM.
           DISPLAY '  Tier 2 (3.5%): $' TIER-2-COMM.
           DISPLAY '  Tier 3 (5.0%): $' TIER-3-COMM.
           DISPLAY '  Tier 4 (6.5%): $' TIER-4-COMM.
           DISPLAY '  Experience Bonus: $' EXPERIENCE-BONUS-AMT.
           DISPLAY 'TOTAL COMMISSION: $' TOTAL-COMMISSION.
           DISPLAY '----------------------------'.
       
       UPDATE-TOTALS.
           ADD SALES-AMOUNT(I) TO TOTAL-SALES.
           ADD TOTAL-COMMISSION TO TOTAL-COMMISSIONS.
           
           IF SALES-AMOUNT(I) > TOP-PERFORMER-SALES
               MOVE SALES-AMOUNT(I) TO TOP-PERFORMER-SALES
               MOVE SALES-NAME(I) TO TOP-PERFORMER-NAME
           END-IF.
           
           PERFORM VARYING J FROM 1 BY 1 UNTIL J > 3
               IF SALES-REGION(I) = REGION-NAME(J)
                   ADD SALES-AMOUNT(I) TO REGION-SALES(J)
                   ADD TOTAL-COMMISSION TO REGION-COMMISSION(J)
               END-IF
           END-PERFORM.
           
           IF SALES-AMOUNT(I) >= 150000
               ADD 1 TO TOP-PERFORMER-COUNT
           END-IF.
       
       DISPLAY-SUMMARY-REPORTS.
           DISPLAY 'COMPANY SUMMARY'.
           DISPLAY '==============='.
           DISPLAY 'Total Company Sales: $' TOTAL-SALES.
           DISPLAY 'Total Commissions Paid: $' TOTAL-COMMISSIONS.
           DISPLAY 'Top Performer: ' TOP-PERFORMER-NAME.
           DISPLAY 'Top Sales Amount: $' TOP-PERFORMER-SALES.
           DISPLAY 'High Performers (>$150K): ' TOP-PERFORMER-COUNT.
           DISPLAY ' '.
           
           DISPLAY 'REGIONAL BREAKDOWN'.
           DISPLAY '=================='.
           
           PERFORM VARYING J FROM 1 BY 1 UNTIL J > 3
               DISPLAY 'Region: ' REGION-NAME(J)
               DISPLAY '  Sales: $' REGION-SALES(J)
               DISPLAY '  Commissions: $' REGION-COMMISSION(J)
               IF REGION-SALES(J) > 0
                   COMPUTE BASE-COMMISSION = 
                       (REGION-COMMISSION(J) / REGION-SALES(J)) * 100
                   DISPLAY '  Commission Rate: ' BASE-COMMISSION '%'
               END-IF
               DISPLAY ' '
           END-PERFORM.
       
       STOP RUN.`
  },
  {
    name: "Hospital Patient System",
    description: "Patient management with medical records and billing",
    code: `       IDENTIFICATION DIVISION.
       PROGRAM-ID. HOSPITAL-SYSTEM.
       
       DATA DIVISION.
       WORKING-STORAGE SECTION.
       01 PATIENT-RECORDS.
          05 PATIENT-ENTRY OCCURS 5 TIMES.
             10 PATIENT-ID PIC 9(6).
             10 PATIENT-NAME PIC X(30).
             10 PATIENT-AGE PIC 9(3).
             10 PATIENT-GENDER PIC X(1).
             10 ADMISSION-DATE PIC 9(8).
             10 DISCHARGE-DATE PIC 9(8).
             10 ROOM-NUMBER PIC 9(3).
             10 DEPARTMENT PIC X(20).
             10 ATTENDING-DOCTOR PIC X(25).
             10 DIAGNOSIS PIC X(40).
             10 TREATMENT-COST PIC 9(6)V99.
             10 INSURANCE-COVERAGE PIC V99.
             10 PATIENT-STATUS PIC X(15).
       
       01 BILLING-CALCULATIONS.
          05 GROSS-CHARGES PIC 9(7)V99.
          05 INSURANCE-PAYMENT PIC 9(7)V99.
          05 PATIENT-RESPONSIBILITY PIC 9(7)V99.
          05 ROOM-CHARGES PIC 9(5)V99.
          05 DAYS-IN-HOSPITAL PIC 9(3).
          05 DAILY-ROOM-RATE PIC 9(4)V99 VALUE 350.00.
       
       01 DEPARTMENT-STATISTICS.
          05 DEPT-ENTRY OCCURS 4 TIMES.
             10 DEPT-NAME PIC X(20).
             10 DEPT-PATIENT-COUNT PIC 9(3) VALUE ZERO.
             10 DEPT-TOTAL-CHARGES PIC 9(8)V99 VALUE ZERO.
             10 DEPT-AVG-STAY PIC 9(3)V99 VALUE ZERO.
       
       01 HOSPITAL-TOTALS.
          05 TOTAL-PATIENTS PIC 9(4) VALUE 5.
          05 ACTIVE-PATIENTS PIC 9(4) VALUE ZERO.
          05 DISCHARGED-PATIENTS PIC 9(4) VALUE ZERO.
          05 TOTAL-REVENUE PIC 9(9)V99 VALUE ZERO.
          05 TOTAL-INSURANCE-PAID PIC 9(9)V99 VALUE ZERO.
          05 TOTAL-PATIENT-OWED PIC 9(9)V99 VALUE ZERO.
       
       01 AGE-DEMOGRAPHICS.
          05 PEDIATRIC-COUNT PIC 9(3) VALUE ZERO.
          05 ADULT-COUNT PIC 9(3) VALUE ZERO.
          05 SENIOR-COUNT PIC 9(3) VALUE ZERO.
       
       01 CURRENT-DATE-NUM PIC 9(8) VALUE 20240315.
       01 TEMP-CALCULATIONS PIC 9(8).
       
       01 COUNTERS.
          05 I PIC 9(2).
          05 J PIC 9(2).
          05 LONG-STAY-COUNT PIC 9(2) VALUE ZERO.
       
       PROCEDURE DIVISION.
       INITIALIZE-HOSPITAL.
           MOVE 'CARDIOLOGY' TO DEPT-NAME(1).
           MOVE 'EMERGENCY' TO DEPT-NAME(2).
           MOVE 'ORTHOPEDICS' TO DEPT-NAME(3).
           MOVE 'PEDIATRICS' TO DEPT-NAME(4).
           
           MOVE 100001 TO PATIENT-ID(1).
           MOVE 'ANDERSON, MARY ELIZABETH' TO PATIENT-NAME(1).
           MOVE 67 TO PATIENT-AGE(1).
           MOVE 'F' TO PATIENT-GENDER(1).
           MOVE 20240310 TO ADMISSION-DATE(1).
           MOVE 20240318 TO DISCHARGE-DATE(1).
           MOVE 201 TO ROOM-NUMBER(1).
           MOVE 'CARDIOLOGY' TO DEPARTMENT(1).
           MOVE 'DR. SMITH, ROBERT' TO ATTENDING-DOCTOR(1).
           MOVE 'CORONARY ARTERY DISEASE' TO DIAGNOSIS(1).
           MOVE 12500.00 TO TREATMENT-COST(1).
           MOVE 0.80 TO INSURANCE-COVERAGE(1).
           MOVE 'DISCHARGED' TO PATIENT-STATUS(1).
           
           MOVE 100002 TO PATIENT-ID(2).
           MOVE 'JOHNSON, WILLIAM JAMES' TO PATIENT-NAME(2).
           MOVE 45 TO PATIENT-AGE(2).
           MOVE 'M' TO PATIENT-GENDER(2).
           MOVE 20240312 TO ADMISSION-DATE(2).
           MOVE 20240000 TO DISCHARGE-DATE(2).
           MOVE 305 TO ROOM-NUMBER(2).
           MOVE 'ORTHOPEDICS' TO DEPARTMENT(2).
           MOVE 'DR. WILLIAMS, SARAH' TO ATTENDING-DOCTOR(2).
           MOVE 'FRACTURED FEMUR' TO DIAGNOSIS(2).
           MOVE 8750.00 TO TREATMENT-COST(2).
           MOVE 0.90 TO INSURANCE-COVERAGE(2).
           MOVE 'ADMITTED' TO PATIENT-STATUS(2).
           
           MOVE 100003 TO PATIENT-ID(3).
           MOVE 'BROWN, JENNIFER ANN' TO PATIENT-NAME(3).
           MOVE 8 TO PATIENT-AGE(3).
           MOVE 'F' TO PATIENT-GENDER(3).
           MOVE 20240314 TO ADMISSION-DATE(3).
           MOVE 20240316 TO DISCHARGE-DATE(3).
           MOVE 410 TO ROOM-NUMBER(3).
           MOVE 'PEDIATRICS' TO DEPARTMENT(3).
           MOVE 'DR. DAVIS, MICHAEL' TO ATTENDING-DOCTOR(3).
           MOVE 'PNEUMONIA' TO DIAGNOSIS(3).
           MOVE 4200.00 TO TREATMENT-COST(3).
           MOVE 0.95 TO INSURANCE-COVERAGE(3).
           MOVE 'DISCHARGED' TO PATIENT-STATUS(3).
           
           MOVE 100004 TO PATIENT-ID(4).
           MOVE 'WILSON, CHARLES EDWARD' TO PATIENT-NAME(4).
           MOVE 72 TO PATIENT-AGE(4).
           MOVE 'M' TO PATIENT-GENDER(4).
           MOVE 20240308 TO ADMISSION-DATE(4).
           MOVE 20240000 TO DISCHARGE-DATE(4).
           MOVE 150 TO ROOM-NUMBER(4).
           MOVE 'EMERGENCY' TO DEPARTMENT(4).
           MOVE 'DR. TAYLOR, LISA' TO ATTENDING-DOCTOR(4).
           MOVE 'ACUTE MYOCARDIAL INFARCTION' TO DIAGNOSIS(4).
           MOVE 18900.00 TO TREATMENT-COST(4).
           MOVE 0.75 TO INSURANCE-COVERAGE(4).
           MOVE 'ADMITTED' TO PATIENT-STATUS(4).
           
           MOVE 100005 TO PATIENT-ID(5).
           MOVE 'GARCIA, MARIA ISABEL' TO PATIENT-NAME(5).
           MOVE 34 TO PATIENT-AGE(5).
           MOVE 'F' TO PATIENT-GENDER(5).
           MOVE 20240313 TO ADMISSION-DATE(5).
           MOVE 20240315 TO DISCHARGE-DATE(5).
           MOVE 275 TO ROOM-NUMBER(5).
           MOVE 'ORTHOPEDICS' TO DEPARTMENT(5).
           MOVE 'DR. MARTINEZ, CARLOS' TO ATTENDING-DOCTOR(5).
           MOVE 'TORN ACL REPAIR' TO DIAGNOSIS(5).
           MOVE 6800.00 TO TREATMENT-COST(5).
           MOVE 0.85 TO INSURANCE-COVERAGE(5).
           MOVE 'DISCHARGED' TO PATIENT-STATUS(5).
       
       PROCESS-HOSPITAL-DATA.
           DISPLAY 'HOSPITAL PATIENT MANAGEMENT SYSTEM'.
           DISPLAY '==================================='.
           
           PERFORM VARYING I FROM 1 BY 1 UNTIL I > TOTAL-PATIENTS
               PERFORM CALCULATE-PATIENT-BILLING
               PERFORM DISPLAY-PATIENT-REPORT
               PERFORM UPDATE-STATISTICS
           END-PERFORM.
           
           PERFORM CALCULATE-DEPARTMENT-AVERAGES.
           PERFORM DISPLAY-HOSPITAL-SUMMARY.
       
       CALCULATE-PATIENT-BILLING.
           MOVE TREATMENT-COST(I) TO GROSS-CHARGES.
           
           IF DISCHARGE-DATE(I) = 0
               COMPUTE DAYS-IN-HOSPITAL = CURRENT-DATE-NUM - 
                   ADMISSION-DATE(I)
           ELSE
               COMPUTE DAYS-IN-HOSPITAL = DISCHARGE-DATE(I) - 
                   ADMISSION-DATE(I)
           END-IF.
           
           COMPUTE ROOM-CHARGES = DAYS-IN-HOSPITAL * DAILY-ROOM-RATE.
           ADD ROOM-CHARGES TO GROSS-CHARGES.
           
           COMPUTE INSURANCE-PAYMENT = GROSS-CHARGES * 
               INSURANCE-COVERAGE(I).
           COMPUTE PATIENT-RESPONSIBILITY = GROSS-CHARGES - 
               INSURANCE-PAYMENT.
       
       DISPLAY-PATIENT-REPORT.
           DISPLAY 'Patient: ' PATIENT-NAME(I).
           DISPLAY 'ID: ' PATIENT-ID(I) ' Age: ' PATIENT-AGE(I) 
               ' Gender: ' PATIENT-GENDER(I).
           DISPLAY 'Room: ' ROOM-NUMBER(I) ' Department: ' DEPARTMENT(I).
           DISPLAY 'Attending: ' ATTENDING-DOCTOR(I).
           DISPLAY 'Diagnosis: ' DIAGNOSIS(I).
           DISPLAY 'Status: ' PATIENT-STATUS(I).
           DISPLAY 'Admission Date: ' ADMISSION-DATE(I).
           
           IF DISCHARGE-DATE(I) NOT = 0
               DISPLAY 'Discharge Date: ' DISCHARGE-DATE(I)
           ELSE
               DISPLAY 'Still Admitted'
           END-IF.
           
           DISPLAY 'Length of Stay: ' DAYS-IN-HOSPITAL ' days'.
           DISPLAY 'BILLING INFORMATION:'.
           DISPLAY '  Treatment Costs: $' TREATMENT-COST(I).
           DISPLAY '  Room Charges: $' ROOM-CHARGES.
           DISPLAY '  Total Charges: $' GROSS-CHARGES.
           DISPLAY '  Insurance Coverage: ' 
               INSURANCE-COVERAGE(I) * 100 '%'.
           DISPLAY '  Insurance Payment: $' INSURANCE-PAYMENT.
           DISPLAY '  Patient Owes: $' PATIENT-RESPONSIBILITY.
           
           IF DAYS-IN-HOSPITAL > 7
               DISPLAY '  *** EXTENDED STAY ***'
           END-IF.
           
           DISPLAY '-----------------------------------'.
       
       UPDATE-STATISTICS.
           IF PATIENT-STATUS(I) = 'ADMITTED'
               ADD 1 TO ACTIVE-PATIENTS
           ELSE
               ADD 1 TO DISCHARGED-PATIENTS
           END-IF.
           
           ADD GROSS-CHARGES TO TOTAL-REVENUE.
           ADD INSURANCE-PAYMENT TO TOTAL-INSURANCE-PAID.
           ADD PATIENT-RESPONSIBILITY TO TOTAL-PATIENT-OWED.
           
           PERFORM VARYING J FROM 1 BY 1 UNTIL J > 4
               IF DEPARTMENT(I) = DEPT-NAME(J)
                   ADD 1 TO DEPT-PATIENT-COUNT(J)
                   ADD GROSS-CHARGES TO DEPT-TOTAL-CHARGES(J)
                   ADD DAYS-IN-HOSPITAL TO DEPT-AVG-STAY(J)
               END-IF
           END-PERFORM.
           
           EVALUATE PATIENT-AGE(I)
               WHEN 0 THRU 17
                   ADD 1 TO PEDIATRIC-COUNT
               WHEN 18 THRU 64
                   ADD 1 TO ADULT-COUNT
               WHEN 65 THRU 120
                   ADD 1 TO SENIOR-COUNT
           END-EVALUATE.
           
           IF DAYS-IN-HOSPITAL > 10
               ADD 1 TO LONG-STAY-COUNT
           END-IF.
       
       CALCULATE-DEPARTMENT-AVERAGES.
           PERFORM VARYING J FROM 1 BY 1 UNTIL J > 4
               IF DEPT-PATIENT-COUNT(J) > 0
                   COMPUTE DEPT-AVG-STAY(J) = DEPT-AVG-STAY(J) / 
                       DEPT-PATIENT-COUNT(J)
               END-IF
           END-PERFORM.
       
       DISPLAY-HOSPITAL-SUMMARY.
           DISPLAY 'HOSPITAL STATISTICS SUMMARY'.
           DISPLAY '============================'.
           DISPLAY 'Total Patients: ' TOTAL-PATIENTS.
           DISPLAY 'Currently Admitted: ' ACTIVE-PATIENTS.
           DISPLAY 'Discharged: ' DISCHARGED-PATIENTS.
           DISPLAY 'Extended Stays (>10 days): ' LONG-STAY-COUNT.
           DISPLAY ' '.
           
           DISPLAY 'FINANCIAL SUMMARY'.
           DISPLAY '================='.
           DISPLAY 'Total Revenue: $' TOTAL-REVENUE.
           DISPLAY 'Insurance Payments: $' TOTAL-INSURANCE-PAID.
           DISPLAY 'Patient Responsibility: $' TOTAL-PATIENT-OWED.
           DISPLAY ' '.
           
           DISPLAY 'AGE DEMOGRAPHICS'.
           DISPLAY '================'.
           DISPLAY 'Pediatric (0-17): ' PEDIATRIC-COUNT.
           DISPLAY 'Adult (18-64): ' ADULT-COUNT.
           DISPLAY 'Senior (65+): ' SENIOR-COUNT.
           DISPLAY ' '.
           
           DISPLAY 'DEPARTMENT STATISTICS'.
           DISPLAY '====================='.
           
           PERFORM VARYING J FROM 1 BY 1 UNTIL J > 4
               IF DEPT-PATIENT-COUNT(J) > 0
                   DISPLAY 'Department: ' DEPT-NAME(J)
                   DISPLAY '  Patients: ' DEPT-PATIENT-COUNT(J)
                   DISPLAY '  Total Charges: $' DEPT-TOTAL-CHARGES(J)
                   DISPLAY '  Average Stay: ' DEPT-AVG-STAY(J) ' days'
                   DISPLAY ' '
               END-IF
           END-PERFORM.
       
       STOP RUN.`
  },
  {
    name: "Complex Sorting Algorithm",
    description: "Bubble sort implementation with performance metrics",
    code: `       IDENTIFICATION DIVISION.
       PROGRAM-ID. BUBBLE-SORT.
       
       DATA DIVISION.
       WORKING-STORAGE SECTION.
       01 SORT-ARRAY.
          05 ARRAY-SIZE PIC 9(3) VALUE 15.
          05 SORT-ELEMENT OCCURS 15 TIMES PIC S9(5).
       
       01 ORIGINAL-ARRAY.
          05 ORIG-ELEMENT OCCURS 15 TIMES PIC S9(5).
       
       01 SORT-STATISTICS.
          05 COMPARISONS PIC 9(6) VALUE ZERO.
          05 SWAPS PIC 9(6) VALUE ZERO.
          05 PASSES PIC 9(4) VALUE ZERO.
          05 SORT-START-TIME PIC 9(8).
          05 SORT-END-TIME PIC 9(8).
          05 SORT-DURATION PIC 9(6).
       
       01 SORT-CONTROL.
          05 OUTER-INDEX PIC 9(3).
          05 INNER-INDEX PIC 9(3).
          05 TEMP-VALUE PIC S9(5).
          05 SWAPPED-FLAG PIC X VALUE 'N'.
          05 SORT-DIRECTION PIC X VALUE 'A'.
       
       01 DISPLAY-CONTROL.
          05 ELEMENTS-PER-LINE PIC 9(2) VALUE 5.
          05 LINE-COUNT PIC 9(2).
          05 ELEMENT-COUNT PIC 9(3).
       
       01 ANALYSIS-DATA.
          05 MIN-VALUE PIC S9(5).
          05 MAX-VALUE PIC S9(5).
          05 TOTAL-SUM PIC S9(8) VALUE ZERO.
          05 AVERAGE-VALUE PIC S9(5)V99.
          05 RANGE-VALUE PIC 9(5).
          05 NEGATIVE-COUNT PIC 9(3) VALUE ZERO.
          05 POSITIVE-COUNT PIC 9(3) VALUE ZERO.
          05 ZERO-COUNT PIC 9(3) VALUE ZERO.
       
       PROCEDURE DIVISION.
       MAIN-PROGRAM.
           PERFORM INITIALIZE-ARRAY.
           PERFORM DISPLAY-ORIGINAL-ARRAY.
           PERFORM ANALYZE-ORIGINAL-DATA.
           PERFORM BUBBLE-SORT-ASCENDING.
           PERFORM DISPLAY-SORTED-ARRAY.
           PERFORM DISPLAY-SORT-STATISTICS.
           PERFORM DEMONSTRATE-DESCENDING-SORT.
           STOP RUN.
       
       INITIALIZE-ARRAY.
           DISPLAY 'INITIALIZING ARRAY WITH SAMPLE DATA'.
           DISPLAY '===================================='.
           
           MOVE 42 TO SORT-ELEMENT(1).
           MOVE -15 TO SORT-ELEMENT(2).
           MOVE 78 TO SORT-ELEMENT(3).
           MOVE 23 TO SORT-ELEMENT(4).
           MOVE -8 TO SORT-ELEMENT(5).
           MOVE 156 TO SORT-ELEMENT(6).
           MOVE 0 TO SORT-ELEMENT(7).
           MOVE 91 TO SORT-ELEMENT(8).
           MOVE -34 TO SORT-ELEMENT(9).
           MOVE 67 TO SORT-ELEMENT(10).
           MOVE 12 TO SORT-ELEMENT(11).
           MOVE -99 TO SORT-ELEMENT(12).
           MOVE 234 TO SORT-ELEMENT(13).
           MOVE 5 TO SORT-ELEMENT(14).
           MOVE -45 TO SORT-ELEMENT(15).
           
           PERFORM VARYING OUTER-INDEX FROM 1 BY 1 
               UNTIL OUTER-INDEX > ARRAY-SIZE
               MOVE SORT-ELEMENT(OUTER-INDEX) TO 
                   ORIG-ELEMENT(OUTER-INDEX)
           END-PERFORM.
       
       DISPLAY-ORIGINAL-ARRAY.
           DISPLAY 'ORIGINAL ARRAY:'.
           MOVE 0 TO LINE-COUNT.
           MOVE 0 TO ELEMENT-COUNT.
           
           PERFORM VARYING OUTER-INDEX FROM 1 BY 1 
               UNTIL OUTER-INDEX > ARRAY-SIZE
               ADD 1 TO ELEMENT-COUNT
               DISPLAY SORT-ELEMENT(OUTER-INDEX) ' ' WITH NO ADVANCING
               
               IF ELEMENT-COUNT = ELEMENTS-PER-LINE
                   DISPLAY ' '
                   MOVE 0 TO ELEMENT-COUNT
                   ADD 1 TO LINE-COUNT
               END-IF
           END-PERFORM.
           
           IF ELEMENT-COUNT NOT = 0
               DISPLAY ' '
           END-IF.
           DISPLAY ' '.
       
       ANALYZE-ORIGINAL-DATA.
           MOVE SORT-ELEMENT(1) TO MIN-VALUE.
           MOVE SORT-ELEMENT(1) TO MAX-VALUE.
           MOVE ZERO TO TOTAL-SUM.
           MOVE ZERO TO NEGATIVE-COUNT.
           MOVE ZERO TO POSITIVE-COUNT.
           MOVE ZERO TO ZERO-COUNT.
           
           PERFORM VARYING OUTER-INDEX FROM 1 BY 1 
               UNTIL OUTER-INDEX > ARRAY-SIZE
               IF SORT-ELEMENT(OUTER-INDEX) < MIN-VALUE
                   MOVE SORT-ELEMENT(OUTER-INDEX) TO MIN-VALUE
               END-IF
               
               IF SORT-ELEMENT(OUTER-INDEX) > MAX-VALUE
                   MOVE SORT-ELEMENT(OUTER-INDEX) TO MAX-VALUE
               END-IF
               
               ADD SORT-ELEMENT(OUTER-INDEX) TO TOTAL-SUM
               
               EVALUATE SORT-ELEMENT(OUTER-INDEX)
                   WHEN ZERO
                       ADD 1 TO ZERO-COUNT
                   WHEN POSITIVE
                       ADD 1 TO POSITIVE-COUNT
                   WHEN NEGATIVE
                       ADD 1 TO NEGATIVE-COUNT
               END-EVALUATE
           END-PERFORM.
           
           COMPUTE AVERAGE-VALUE = TOTAL-SUM / ARRAY-SIZE.
           COMPUTE RANGE-VALUE = MAX-VALUE - MIN-VALUE.
           
           DISPLAY 'ORIGINAL DATA ANALYSIS:'.
           DISPLAY 'Minimum Value: ' MIN-VALUE.
           DISPLAY 'Maximum Value: ' MAX-VALUE.
           DISPLAY 'Range: ' RANGE-VALUE.
           DISPLAY 'Sum: ' TOTAL-SUM.
           DISPLAY 'Average: ' AVERAGE-VALUE.
           DISPLAY 'Positive Numbers: ' POSITIVE-COUNT.
           DISPLAY 'Negative Numbers: ' NEGATIVE-COUNT.
           DISPLAY 'Zero Values: ' ZERO-COUNT.
           DISPLAY ' '.
       
       BUBBLE-SORT-ASCENDING.
           DISPLAY 'PERFORMING BUBBLE SORT (ASCENDING)'.
           DISPLAY '==================================='.
           
           MOVE ZERO TO COMPARISONS.
           MOVE ZERO TO SWAPS.
           MOVE ZERO TO PASSES.
           MOVE 'A' TO SORT-DIRECTION.
           
           PERFORM VARYING OUTER-INDEX FROM 1 BY 1 
               UNTIL OUTER-INDEX >= ARRAY-SIZE
               MOVE 'N' TO SWAPPED-FLAG
               ADD 1 TO PASSES
               
               PERFORM VARYING INNER-INDEX FROM 1 BY 1 
                   UNTIL INNER-INDEX >= (ARRAY-SIZE - OUTER-INDEX + 1)
                   ADD 1 TO COMPARISONS
                   
                   IF SORT-ELEMENT(INNER-INDEX) > 
                      SORT-ELEMENT(INNER-INDEX + 1)
                       MOVE SORT-ELEMENT(INNER-INDEX) TO TEMP-VALUE
                       MOVE SORT-ELEMENT(INNER-INDEX + 1) TO 
                           SORT-ELEMENT(INNER-INDEX)
                       MOVE TEMP-VALUE TO SORT-ELEMENT(INNER-INDEX + 1)
                       ADD 1 TO SWAPS
                       MOVE 'Y' TO SWAPPED-FLAG
                   END-IF
               END-PERFORM
               
               IF SWAPPED-FLAG = 'N'
                   EXIT PERFORM
               END-IF
           END-PERFORM.
       
       DISPLAY-SORTED-ARRAY.
           DISPLAY 'SORTED ARRAY (ASCENDING):'.
           MOVE 0 TO LINE-COUNT.
           MOVE 0 TO ELEMENT-COUNT.
           
           PERFORM VARYING OUTER-INDEX FROM 1 BY 1 
               UNTIL OUTER-INDEX > ARRAY-SIZE
               ADD 1 TO ELEMENT-COUNT
               DISPLAY SORT-ELEMENT(OUTER-INDEX) ' ' WITH NO ADVANCING
               
               IF ELEMENT-COUNT = ELEMENTS-PER-LINE
                   DISPLAY ' '
                   MOVE 0 TO ELEMENT-COUNT
                   ADD 1 TO LINE-COUNT
               END-IF
           END-PERFORM.
           
           IF ELEMENT-COUNT NOT = 0
               DISPLAY ' '
           END-IF.
           DISPLAY ' '.
       
       DISPLAY-SORT-STATISTICS.
           DISPLAY 'SORT PERFORMANCE STATISTICS'.
           DISPLAY '============================'.
           DISPLAY 'Array Size: ' ARRAY-SIZE ' elements'.
           DISPLAY 'Total Comparisons: ' COMPARISONS.
           DISPLAY 'Total Swaps: ' SWAPS.
           DISPLAY 'Number of Passes: ' PASSES.
           
           COMPUTE TEMP-VALUE = (ARRAY-SIZE * (ARRAY-SIZE - 1)) / 2.
           DISPLAY 'Theoretical Max Comparisons: ' TEMP-VALUE.
           
           IF TEMP-VALUE > 0
               COMPUTE AVERAGE-VALUE = (COMPARISONS * 100) / TEMP-VALUE
               DISPLAY 'Efficiency: ' AVERAGE-VALUE '%'
           END-IF.
           
           DISPLAY ' '.
       
       DEMONSTRATE-DESCENDING-SORT.
           DISPLAY 'DEMONSTRATING DESCENDING SORT'.
           DISPLAY '=============================='.
           
           PERFORM VARYING OUTER-INDEX FROM 1 BY 1 
               UNTIL OUTER-INDEX > ARRAY-SIZE
               MOVE ORIG-ELEMENT(OUTER-INDEX) TO 
                   SORT-ELEMENT(OUTER-INDEX)
           END-PERFORM.
           
           MOVE ZERO TO COMPARISONS.
           MOVE ZERO TO SWAPS.
           MOVE ZERO TO PASSES.
           MOVE 'D' TO SORT-DIRECTION.
           
           PERFORM VARYING OUTER-INDEX FROM 1 BY 1 
               UNTIL OUTER-INDEX >= ARRAY-SIZE
               MOVE 'N' TO SWAPPED-FLAG
               ADD 1 TO PASSES
               
               PERFORM VARYING INNER-INDEX FROM 1 BY 1 
                   UNTIL INNER-INDEX >= (ARRAY-SIZE - OUTER-INDEX + 1)
                   ADD 1 TO COMPARISONS
                   
                   IF SORT-ELEMENT(INNER-INDEX) < 
                      SORT-ELEMENT(INNER-INDEX + 1)
                       MOVE SORT-ELEMENT(INNER-INDEX) TO TEMP-VALUE
                       MOVE SORT-ELEMENT(INNER-INDEX + 1) TO 
                           SORT-ELEMENT(INNER-INDEX)
                       MOVE TEMP-VALUE TO SORT-ELEMENT(INNER-INDEX + 1)
                       ADD 1 TO SWAPS
                       MOVE 'Y' TO SWAPPED-FLAG
                   END-IF
               END-PERFORM
               
               IF SWAPPED-FLAG = 'N'
                   EXIT PERFORM
               END-IF
           END-PERFORM.
           
           DISPLAY 'SORTED ARRAY (DESCENDING):'.
           MOVE 0 TO ELEMENT-COUNT.
           
           PERFORM VARYING OUTER-INDEX FROM 1 BY 1 
               UNTIL OUTER-INDEX > ARRAY-SIZE
               ADD 1 TO ELEMENT-COUNT
               DISPLAY SORT-ELEMENT(OUTER-INDEX) ' ' WITH NO ADVANCING
               
               IF ELEMENT-COUNT = ELEMENTS-PER-LINE
                   DISPLAY ' '
                   MOVE 0 TO ELEMENT-COUNT
               END-IF
           END-PERFORM.
           
           IF ELEMENT-COUNT NOT = 0
               DISPLAY ' '
           END-IF.
           
           DISPLAY 'Descending Sort - Comparisons: ' COMPARISONS 
               ' Swaps: ' SWAPS ' Passes: ' PASSES.
       
       STOP RUN.`
  },
  {
    name: "Weather Data Analysis",
    description: "Process weather data with statistical calculations",
    code: `       IDENTIFICATION DIVISION.
       PROGRAM-ID. WEATHER-ANALYSIS.
       
       DATA DIVISION.
       WORKING-STORAGE SECTION.
       01 WEATHER-DATA.
          05 DAILY-WEATHER OCCURS 30 TIMES.
             10 WEATHER-DATE PIC 9(8).
             10 HIGH-TEMP PIC S9(3).
             10 LOW-TEMP PIC S9(3).
             10 HUMIDITY PIC 9(3).
             10 PRECIPITATION PIC 9(2)V99.
             10 WIND-SPEED PIC 9(2).
             10 WEATHER-CONDITION PIC X(15).
             10 BAROMETRIC-PRESSURE PIC 9(2)V99.
       
       01 MONTHLY-STATISTICS.
          05 TOTAL-DAYS PIC 9(2) VALUE 30.
          05 AVG-HIGH-TEMP PIC S9(3)V99.
          05 AVG-LOW-TEMP PIC S9(3)V99.
          05 AVG-HUMIDITY PIC 9(3)V99.
          05 TOTAL-PRECIPITATION PIC 9(4)V99.
          05 AVG-WIND-SPEED PIC 9(2)V99.
          05 AVG-PRESSURE PIC 9(2)V99.
       
       01 EXTREME-VALUES.
          05 HIGHEST-TEMP PIC S9(3).
          05 HIGHEST-TEMP-DATE PIC 9(8).
          05 LOWEST-TEMP PIC S9(3).
          05 LOWEST-TEMP-DATE PIC 9(8).
          05 MAX-PRECIPITATION PIC 9(2)V99.
          05 MAX-PRECIP-DATE PIC 9(8).
          05 MAX-WIND-SPEED PIC 9(2).
          05 MAX-WIND-DATE PIC 9(8).
       
       01 WEATHER-PATTERNS.
          05 SUNNY-DAYS PIC 9(2) VALUE ZERO.
          05 CLOUDY-DAYS PIC 9(2) VALUE ZERO.
          05 RAINY-DAYS PIC 9(2) VALUE ZERO.
          05 STORMY-DAYS PIC 9(2) VALUE ZERO.
          05 SNOW-DAYS PIC 9(2) VALUE ZERO.
       
       01 TEMPERATURE-RANGES.
          05 FREEZING-DAYS PIC 9(2) VALUE ZERO.
          05 COOL-DAYS PIC 9(2) VALUE ZERO.
          05 WARM-DAYS PIC 9(2) VALUE ZERO.
          05 HOT-DAYS PIC 9(2) VALUE ZERO.
       
       01 COMFORT-INDEX.
          05 COMFORT-LEVEL PIC 9(2)V99.
          05 HEAT-INDEX PIC 9(3)V99.
          05 UNCOMFORTABLE-DAYS PIC 9(2) VALUE ZERO.
       
       01 CALCULATIONS.
          05 TEMP-SUM PIC S9(6) VALUE ZERO.
          05 HUMIDITY-SUM PIC 9(6) VALUE ZERO.
          05 WIND-SUM PIC 9(4) VALUE ZERO.
          05 PRESSURE-SUM PIC 9(5)V99 VALUE ZERO.
          05 DAILY-MEAN-TEMP PIC S9(3)V99.
          05 TEMP-VARIANCE PIC 9(6)V99.
          05 TEMP-DIFFERENCE PIC S9(3).
       
       01 COUNTERS.
          05 I PIC 9(2).
          05 RAINY-STREAK PIC 9(2) VALUE ZERO.
          05 LONGEST-STREAK PIC 9(2) VALUE ZERO.
          05 CURRENT-STREAK PIC 9(2) VALUE ZERO.
       
       PROCEDURE DIVISION.
       MAIN-WEATHER-ANALYSIS.
           PERFORM INITIALIZE-WEATHER-DATA.
           PERFORM CALCULATE-BASIC-STATISTICS.
           PERFORM FIND-EXTREME-VALUES.
           PERFORM ANALYZE-WEATHER-PATTERNS.
           PERFORM CALCULATE-COMFORT-INDICES.
           PERFORM DISPLAY-WEATHER-REPORT.
           STOP RUN.
       
       INITIALIZE-WEATHER-DATA.
           DISPLAY 'INITIALIZING WEATHER DATA FOR MARCH 2024'.
           DISPLAY '========================================='.
           
           MOVE 20240301 TO WEATHER-DATE(1).
           MOVE 65 TO HIGH-TEMP(1).
           MOVE 42 TO LOW-TEMP(1).
           MOVE 68 TO HUMIDITY(1).
           MOVE 0.00 TO PRECIPITATION(1).
           MOVE 12 TO WIND-SPEED(1).
           MOVE 'SUNNY' TO WEATHER-CONDITION(1).
           MOVE 30.15 TO BAROMETRIC-PRESSURE(1).
           
           MOVE 20240302 TO WEATHER-DATE(2).
           MOVE 58 TO HIGH-TEMP(2).
           MOVE 39 TO LOW-TEMP(2).
           MOVE 72 TO HUMIDITY(2).
           MOVE 0.25 TO PRECIPITATION(2).
           MOVE 8 TO WIND-SPEED(2).
           MOVE 'CLOUDY' TO WEATHER-CONDITION(2).
           MOVE 29.98 TO BAROMETRIC-PRESSURE(2).
           
           MOVE 20240303 TO WEATHER-DATE(3).
           MOVE 71 TO HIGH-TEMP(3).
           MOVE 48 TO LOW-TEMP(3).
           MOVE 65 TO HUMIDITY(3).
           MOVE 0.00 TO PRECIPITATION(3).
           MOVE 15 TO WIND-SPEED(3).
           MOVE 'SUNNY' TO WEATHER-CONDITION(3).
           MOVE 30.25 TO BAROMETRIC-PRESSURE(3).
           
           MOVE 20240304 TO WEATHER-DATE(4).
           MOVE 45 TO HIGH-TEMP(4).
           MOVE 28 TO LOW-TEMP(4).
           MOVE 85 TO HUMIDITY(4).
           MOVE 1.50 TO PRECIPITATION(4).
           MOVE 22 TO WIND-SPEED(4).
           MOVE 'RAINY' TO WEATHER-CONDITION(4).
           MOVE 29.45 TO BAROMETRIC-PRESSURE(4).
           
           MOVE 20240305 TO WEATHER-DATE(5).
           MOVE 52 TO HIGH-TEMP(5).
           MOVE 35 TO LOW-TEMP(5).
           MOVE 78 TO HUMIDITY(5).
           MOVE 0.75 TO PRECIPITATION(5).
           MOVE 18 TO WIND-SPEED(5).
           MOVE 'RAINY' TO WEATHER-CONDITION(5).
           MOVE 29.78 TO BAROMETRIC-PRESSURE(5).
           
           PERFORM VARYING I FROM 6 BY 1 UNTIL I > 30
               COMPUTE WEATHER-DATE(I) = 20240300 + I
               COMPUTE HIGH-TEMP(I) = 45 + (I * 2)
               COMPUTE LOW-TEMP(I) = 25 + I
               COMPUTE HUMIDITY(I) = 60 + (I * 1.5)
               COMPUTE PRECIPITATION(I) = (I * 0.1)
               COMPUTE WIND-SPEED(I) = 8 + (I / 3)
               
               EVALUATE I
                   WHEN 6 THRU 10
                       MOVE 'CLOUDY' TO WEATHER-CONDITION(I)
                   WHEN 11 THRU 15
                       MOVE 'SUNNY' TO WEATHER-CONDITION(I)
                   WHEN 16 THRU 20
                       MOVE 'RAINY' TO WEATHER-CONDITION(I)
                   WHEN 21 THRU 25
                       MOVE 'CLOUDY' TO WEATHER-CONDITION(I)
                   WHEN OTHER
                       MOVE 'SUNNY' TO WEATHER-CONDITION(I)
               END-EVALUATE
               
               COMPUTE BAROMETRIC-PRESSURE(I) = 29.50 + (I * 0.02)
           END-PERFORM.
       
       CALCULATE-BASIC-STATISTICS.
           MOVE ZERO TO TEMP-SUM.
           MOVE ZERO TO HUMIDITY-SUM.
           MOVE ZERO TO WIND-SUM.
           MOVE ZERO TO PRESSURE-SUM.
           MOVE ZERO TO TOTAL-PRECIPITATION.
           
           PERFORM VARYING I FROM 1 BY 1 UNTIL I > TOTAL-DAYS
               ADD HIGH-TEMP(I) TO TEMP-SUM
               ADD LOW-TEMP(I) TO TEMP-SUM
               ADD HUMIDITY(I) TO HUMIDITY-SUM
               ADD WIND-SPEED(I) TO WIND-SUM
               ADD BAROMETRIC-PRESSURE(I) TO PRESSURE-SUM
               ADD PRECIPITATION(I) TO TOTAL-PRECIPITATION
           END-PERFORM.
           
           COMPUTE AVG-HIGH-TEMP = TEMP-SUM / (TOTAL-DAYS * 2).
           COMPUTE AVG-LOW-TEMP = TEMP-SUM / (TOTAL-DAYS * 2).
           COMPUTE AVG-HUMIDITY = HUMIDITY-SUM / TOTAL-DAYS.
           COMPUTE AVG-WIND-SPEED = WIND-SUM / TOTAL-DAYS.
           COMPUTE AVG-PRESSURE = PRESSURE-SUM / TOTAL-DAYS.
       
       FIND-EXTREME-VALUES.
           MOVE HIGH-TEMP(1) TO HIGHEST-TEMP.
           MOVE WEATHER-DATE(1) TO HIGHEST-TEMP-DATE.
           MOVE LOW-TEMP(1) TO LOWEST-TEMP.
           MOVE WEATHER-DATE(1) TO LOWEST-TEMP-DATE.
           MOVE PRECIPITATION(1) TO MAX-PRECIPITATION.
           MOVE WEATHER-DATE(1) TO MAX-PRECIP-DATE.
           MOVE WIND-SPEED(1) TO MAX-WIND-SPEED.
           MOVE WEATHER-DATE(1) TO MAX-WIND-DATE.
           
           PERFORM VARYING I FROM 2 BY 1 UNTIL I > TOTAL-DAYS
               IF HIGH-TEMP(I) > HIGHEST-TEMP
                   MOVE HIGH-TEMP(I) TO HIGHEST-TEMP
                   MOVE WEATHER-DATE(I) TO HIGHEST-TEMP-DATE
               END-IF
               
               IF LOW-TEMP(I) < LOWEST-TEMP
                   MOVE LOW-TEMP(I) TO LOWEST-TEMP
                   MOVE WEATHER-DATE(I) TO LOWEST-TEMP-DATE
               END-IF
               
               IF PRECIPITATION(I) > MAX-PRECIPITATION
                   MOVE PRECIPITATION(I) TO MAX-PRECIPITATION
                   MOVE WEATHER-DATE(I) TO MAX-PRECIP-DATE
               END-IF
               
               IF WIND-SPEED(I) > MAX-WIND-SPEED
                   MOVE WIND-SPEED(I) TO MAX-WIND-SPEED
                   MOVE WEATHER-DATE(I) TO MAX-WIND-DATE
               END-IF
           END-PERFORM.
       
       ANALYZE-WEATHER-PATTERNS.
           MOVE ZERO TO CURRENT-STREAK.
           MOVE ZERO TO LONGEST-STREAK.
           
           PERFORM VARYING I FROM 1 BY 1 UNTIL I > TOTAL-DAYS
               EVALUATE WEATHER-CONDITION(I)
                   WHEN 'SUNNY'
                       ADD 1 TO SUNNY-DAYS
                       MOVE ZERO TO CURRENT-STREAK
                   WHEN 'CLOUDY'
                       ADD 1 TO CLOUDY-DAYS
                       MOVE ZERO TO CURRENT-STREAK
                   WHEN 'RAINY'
                       ADD 1 TO RAINY-DAYS
                       ADD 1 TO CURRENT-STREAK
                       IF CURRENT-STREAK > LONGEST-STREAK
                           MOVE CURRENT-STREAK TO LONGEST-STREAK
                       END-IF
                   WHEN 'STORMY'
                       ADD 1 TO STORMY-DAYS
                       MOVE ZERO TO CURRENT-STREAK
                   WHEN 'SNOW'
                       ADD 1 TO SNOW-DAYS
                       MOVE ZERO TO CURRENT-STREAK
               END-EVALUATE
               
               COMPUTE DAILY-MEAN-TEMP = (HIGH-TEMP(I) + LOW-TEMP(I)) / 2
               
               EVALUATE DAILY-MEAN-TEMP
                   WHEN LESS THAN 32
                       ADD 1 TO FREEZING-DAYS
                   WHEN 32 THRU 59
                       ADD 1 TO COOL-DAYS
                   WHEN 60 THRU 79
                       ADD 1 TO WARM-DAYS
                   WHEN 80 THRU 150
                       ADD 1 TO HOT-DAYS
               END-EVALUATE
           END-PERFORM.
       
       CALCULATE-COMFORT-INDICES.
           PERFORM VARYING I FROM 1 BY 1 UNTIL I > TOTAL-DAYS
               COMPUTE DAILY-MEAN-TEMP = (HIGH-TEMP(I) + LOW-TEMP(I)) / 2
               
               IF HIGH-TEMP(I) >= 80 AND HUMIDITY(I) >= 60
                   COMPUTE HEAT-INDEX = HIGH-TEMP(I) + 
                       (HUMIDITY(I) * 0.5)
                   IF HEAT-INDEX > 105
                       ADD 1 TO UNCOMFORTABLE-DAYS
                   END-IF
               END-IF
               
               IF WIND-SPEED(I) > 25 OR PRECIPITATION(I) > 1.0
                   ADD 1 TO UNCOMFORTABLE-DAYS
               END-IF
           END-PERFORM.
       
       DISPLAY-WEATHER-REPORT.
           DISPLAY 'COMPREHENSIVE WEATHER ANALYSIS REPORT'.
           DISPLAY '======================================'.
           DISPLAY ' '.
           
           DISPLAY 'BASIC STATISTICS (30 DAYS):'.
           DISPLAY '============================'.
           DISPLAY 'Average High Temperature: ' AVG-HIGH-TEMP '°F'.
           DISPLAY 'Average Low Temperature: ' AVG-LOW-TEMP '°F'.
           DISPLAY 'Average Humidity: ' AVG-HUMIDITY '%'.
           DISPLAY 'Average Wind Speed: ' AVG-WIND-SPEED ' mph'.
           DISPLAY 'Average Barometric Pressure: ' AVG-PRESSURE ' in'.
           DISPLAY 'Total Precipitation: ' TOTAL-PRECIPITATION ' inches'.
           DISPLAY ' '.
           
           DISPLAY 'EXTREME VALUES:'.
           DISPLAY '==============='.
           DISPLAY 'Highest Temperature: ' HIGHEST-TEMP '°F on ' 
               HIGHEST-TEMP-DATE.
           DISPLAY 'Lowest Temperature: ' LOWEST-TEMP '°F on ' 
               LOWEST-TEMP-DATE.
           DISPLAY 'Maximum Precipitation: ' MAX-PRECIPITATION 
               ' inches on ' MAX-PRECIP-DATE.
           DISPLAY 'Maximum Wind Speed: ' MAX-WIND-SPEED 
               ' mph on ' MAX-WIND-DATE.
           DISPLAY ' '.
           
           DISPLAY 'WEATHER PATTERNS:'.
           DISPLAY '================='.
           DISPLAY 'Sunny Days: ' SUNNY-DAYS.
           DISPLAY 'Cloudy Days: ' CLOUDY-DAYS.
           DISPLAY 'Rainy Days: ' RAINY-DAYS.
           DISPLAY 'Stormy Days: ' STORMY-DAYS.
           DISPLAY 'Snow Days: ' SNOW-DAYS.
           DISPLAY 'Longest Rainy Streak: ' LONGEST-STREAK ' days'.
           DISPLAY ' '.
           
           DISPLAY 'TEMPERATURE DISTRIBUTION:'.
           DISPLAY '========================='.
           DISPLAY 'Freezing Days (<32°F): ' FREEZING-DAYS.
           DISPLAY 'Cool Days (32-59°F): ' COOL-DAYS.
           DISPLAY 'Warm Days (60-79°F): ' WARM-DAYS.
           DISPLAY 'Hot Days (80°F+): ' HOT-DAYS.
           DISPLAY ' '.
           
           DISPLAY 'COMFORT INDEX:'.
           DISPLAY '=============='.
           DISPLAY 'Uncomfortable Days: ' UNCOMFORTABLE-DAYS.
           COMPUTE COMFORT-LEVEL = 
               ((TOTAL-DAYS - UNCOMFORTABLE-DAYS) / TOTAL-DAYS) * 100.
           DISPLAY 'Comfort Level: ' COMFORT-LEVEL '%'.
       
       STOP RUN.`
  },
  {
    name: "Text Processing Engine",
    description: "Advanced string manipulation and text analysis",
    code: `       IDENTIFICATION DIVISION.
       PROGRAM-ID. TEXT-PROCESSOR.
       
       DATA DIVISION.
       WORKING-STORAGE SECTION.
       01 TEXT-INPUT.
          05 INPUT-BUFFER PIC X(500).
          05 BUFFER-LENGTH PIC 9(3).
       
       01 TEXT-ANALYSIS.
          05 CHAR-COUNT PIC 9(4) VALUE ZERO.
          05 WORD-COUNT PIC 9(4) VALUE ZERO.
          05 SENTENCE-COUNT PIC 9(3) VALUE ZERO.
          05 PARAGRAPH-COUNT PIC 9(3) VALUE ZERO.
          05 VOWEL-COUNT PIC 9(4) VALUE ZERO.
          05 CONSONANT-COUNT PIC 9(4) VALUE ZERO.
          05 DIGIT-COUNT PIC 9(4) VALUE ZERO.
          05 SPACE-COUNT PIC 9(4) VALUE ZERO.
          05 SPECIAL-CHAR-COUNT PIC 9(4) VALUE ZERO.
       
       01 WORD-FREQUENCY.
          05 UNIQUE-WORDS OCCURS 50 TIMES.
             10 WORD-TEXT PIC X(20).
             10 WORD-FREQ PIC 9(3) VALUE ZERO.
          05 UNIQUE-WORD-COUNT PIC 9(3) VALUE ZERO.
       
       01 PROCESSING-CONTROLS.
          05 CURRENT-CHAR PIC X.
          05 CURRENT-POSITION PIC 9(3).
          05 IN-WORD-FLAG PIC X VALUE 'N'.
          05 CURRENT-WORD PIC X(20).
          05 WORD-LENGTH PIC 9(2).
          05 WORD-START PIC 9(3).
       
       01 STATISTICS.
          05 LONGEST-WORD PIC X(20).
          05 LONGEST-WORD-LENGTH PIC 9(2) VALUE ZERO.
          05 SHORTEST-WORD PIC X(20).
          05 SHORTEST-WORD-LENGTH PIC 9(2) VALUE 99.
          05 AVERAGE-WORD-LENGTH PIC 9(2)V99.
          05 TOTAL-WORD-CHARS PIC 9(5) VALUE ZERO.
       
       01 SENTENCE-ANALYSIS.
          05 SENTENCE-LENGTHS OCCURS 20 TIMES PIC 9(3).
          05 SENTENCE-WORDS OCCURS 20 TIMES PIC 9(2).
          05 CURRENT-SENTENCE-LENGTH PIC 9(3).
          05 CURRENT-SENTENCE-WORDS PIC 9(2).
          05 LONGEST-SENTENCE PIC 9(3) VALUE ZERO.
          05 SHORTEST-SENTENCE PIC 9(3) VALUE 999.
          05 AVG-SENTENCE-LENGTH PIC 9(3)V99.
       
       01 CHARACTER-FREQUENCY.
          05 CHAR-FREQ OCCURS 26 TIMES PIC 9(3) VALUE ZERO.
          05 MOST-FREQUENT-CHAR PIC X.
          05 HIGHEST-CHAR-FREQ PIC 9(3) VALUE ZERO.
       
       01 TEXT-TRANSFORMATIONS.
          05 UPPERCASE-BUFFER PIC X(500).
          05 LOWERCASE-BUFFER PIC X(500).
          05 TITLE-CASE-BUFFER PIC X(500).
          05 REVERSED-BUFFER PIC X(500).
       
       01 COUNTERS.
          05 I PIC 9(3).
          05 J PIC 9(3).
          05 K PIC 9(2).
          05 CHAR-INDEX PIC 9(2).
       
       PROCEDURE DIVISION.
       MAIN-TEXT-PROCESSING.
           PERFORM INITIALIZE-TEXT-DATA.
           PERFORM ANALYZE-TEXT-STRUCTURE.
           PERFORM COUNT-CHARACTERS.
           PERFORM EXTRACT-WORDS.
           PERFORM ANALYZE-SENTENCES.
           PERFORM CALCULATE-STATISTICS.
           PERFORM TRANSFORM-TEXT.
           PERFORM DISPLAY-COMPREHENSIVE-REPORT.
           STOP RUN.
       
       INITIALIZE-TEXT-DATA.
           MOVE 'The quick brown fox jumps over the lazy dog. ' TO 
               INPUT-BUFFER.
           STRING INPUT-BUFFER DELIMITED BY SIZE
               'This sentence contains every letter of the alphabet. '
               DELIMITED BY SIZE
               'Text processing is a fundamental skill in programming. '
               DELIMITED BY SIZE
               'COBOL provides excellent string manipulation capabilities. '
               DELIMITED BY SIZE
               'Numbers like 123, 456, and 789 are also processed. '
               DELIMITED BY SIZE
               'Special characters: !@#$%^&*()_+-=[]{}|;:,.<>? '
               DELIMITED BY SIZE
               'Multiple sentences can be analyzed for patterns. '
               DELIMITED BY SIZE
               INTO INPUT-BUFFER
           END-STRING.
           
           INSPECT INPUT-BUFFER TALLYING BUFFER-LENGTH FOR CHARACTERS.
           DISPLAY 'TEXT PROCESSING ENGINE INITIALIZED'.
           DISPLAY '=================================='.
           DISPLAY 'Input Text Length: ' BUFFER-LENGTH ' characters'.
           DISPLAY ' '.
       
       ANALYZE-TEXT-STRUCTURE.
           MOVE ZERO TO CHAR-COUNT.
           MOVE ZERO TO SENTENCE-COUNT.
           MOVE ZERO TO PARAGRAPH-COUNT.
           
           PERFORM VARYING I FROM 1 BY 1 UNTIL I > BUFFER-LENGTH
               MOVE INPUT-BUFFER(I:1) TO CURRENT-CHAR
               
               IF CURRENT-CHAR NOT = SPACE
                   ADD 1 TO CHAR-COUNT
               END-IF
               
               EVALUATE CURRENT-CHAR
                   WHEN '.' OR '!' OR '?'
                       ADD 1 TO SENTENCE-COUNT
                   WHEN X'0A' OR X'0D'
                       ADD 1 TO PARAGRAPH-COUNT
               END-EVALUATE
           END-PERFORM.
           
           IF PARAGRAPH-COUNT = ZERO
               MOVE 1 TO PARAGRAPH-COUNT
           END-IF.
       
       COUNT-CHARACTERS.
           MOVE ZERO TO VOWEL-COUNT.
           MOVE ZERO TO CONSONANT-COUNT.
           MOVE ZERO TO DIGIT-COUNT.
           MOVE ZERO TO SPACE-COUNT.
           MOVE ZERO TO SPECIAL-CHAR-COUNT.
           
           PERFORM VARYING I FROM 1 BY 1 UNTIL I > 26
               MOVE ZERO TO CHAR-FREQ(I)
           END-PERFORM.
           
           PERFORM VARYING I FROM 1 BY 1 UNTIL I > BUFFER-LENGTH
               MOVE INPUT-BUFFER(I:1) TO CURRENT-CHAR
               
               EVALUATE CURRENT-CHAR
                   WHEN 'A' OR 'E' OR 'I' OR 'O' OR 'U'
                   WHEN 'a' OR 'e' OR 'i' OR 'o' OR 'u'
                       ADD 1 TO VOWEL-COUNT
                       PERFORM UPDATE-CHAR-FREQUENCY
                   WHEN 'B' THRU 'Z'
                   WHEN 'b' THRU 'z'
                       ADD 1 TO CONSONANT-COUNT
                       PERFORM UPDATE-CHAR-FREQUENCY
                   WHEN '0' THRU '9'
                       ADD 1 TO DIGIT-COUNT
                   WHEN SPACE
                       ADD 1 TO SPACE-COUNT
                   WHEN OTHER
                       ADD 1 TO SPECIAL-CHAR-COUNT
               END-EVALUATE
           END-PERFORM.
           
           PERFORM FIND-MOST-FREQUENT-CHAR.
       
       UPDATE-CHAR-FREQUENCY.
           IF CURRENT-CHAR >= 'A' AND CURRENT-CHAR <= 'Z'
               COMPUTE CHAR-INDEX = FUNCTION ORD(CURRENT-CHAR) - 
                   FUNCTION ORD('A') + 1
               ADD 1 TO CHAR-FREQ(CHAR-INDEX)
           END-IF.
           
           IF CURRENT-CHAR >= 'a' AND CURRENT-CHAR <= 'z'
               COMPUTE CHAR-INDEX = FUNCTION ORD(CURRENT-CHAR) - 
                   FUNCTION ORD('a') + 1
               ADD 1 TO CHAR-FREQ(CHAR-INDEX)
           END-IF.
       
       FIND-MOST-FREQUENT-CHAR.
           MOVE ZERO TO HIGHEST-CHAR-FREQ.
           MOVE 'A' TO MOST-FREQUENT-CHAR.
           
           PERFORM VARYING I FROM 1 BY 1 UNTIL I > 26
               IF CHAR-FREQ(I) > HIGHEST-CHAR-FREQ
                   MOVE CHAR-FREQ(I) TO HIGHEST-CHAR-FREQ
                   COMPUTE CHAR-INDEX = I + FUNCTION ORD('A') - 1
                   MOVE FUNCTION CHAR(CHAR-INDEX) TO MOST-FREQUENT-CHAR
               END-IF
           END-PERFORM.
       
       EXTRACT-WORDS.
           MOVE ZERO TO WORD-COUNT.
           MOVE ZERO TO UNIQUE-WORD-COUNT.
           MOVE ZERO TO TOTAL-WORD-CHARS.
           MOVE 'N' TO IN-WORD-FLAG.
           MOVE SPACES TO CURRENT-WORD.
           MOVE ZERO TO WORD-LENGTH.
           
           PERFORM VARYING I FROM 1 BY 1 UNTIL I > BUFFER-LENGTH
               MOVE INPUT-BUFFER(I:1) TO CURRENT-CHAR
               
               IF CURRENT-CHAR >= 'A' AND CURRENT-CHAR <= 'Z' OR
                  CURRENT-CHAR >= 'a' AND CURRENT-CHAR <= 'z'
                   IF IN-WORD-FLAG = 'N'
                       MOVE 'Y' TO IN-WORD-FLAG
                       MOVE I TO WORD-START
                       MOVE SPACES TO CURRENT-WORD
                       MOVE ZERO TO WORD-LENGTH
                   END-IF
                   ADD 1 TO WORD-LENGTH
                   MOVE CURRENT-CHAR TO CURRENT-WORD(WORD-LENGTH:1)
               ELSE
                   IF IN-WORD-FLAG = 'Y'
                       MOVE 'N' TO IN-WORD-FLAG
                       ADD 1 TO WORD-COUNT
                       ADD WORD-LENGTH TO TOTAL-WORD-CHARS
                       PERFORM PROCESS-WORD
                   END-IF
               END-IF
           END-PERFORM.
           
           IF IN-WORD-FLAG = 'Y'
               ADD 1 TO WORD-COUNT
               ADD WORD-LENGTH TO TOTAL-WORD-CHARS
               PERFORM PROCESS-WORD
           END-IF.
       
       PROCESS-WORD.
           IF WORD-LENGTH > LONGEST-WORD-LENGTH
               MOVE WORD-LENGTH TO LONGEST-WORD-LENGTH
               MOVE CURRENT-WORD TO LONGEST-WORD
           END-IF.
           
           IF WORD-LENGTH < SHORTEST-WORD-LENGTH
               MOVE WORD-LENGTH TO SHORTEST-WORD-LENGTH
               MOVE CURRENT-WORD TO SHORTEST-WORD
           END-IF.
           
           PERFORM VARYING J FROM 1 BY 1 UNTIL J > UNIQUE-WORD-COUNT
               IF CURRENT-WORD = WORD-TEXT(J)
                   ADD 1 TO WORD-FREQ(J)
                   EXIT PERFORM
               END-IF
           END-PERFORM.
           
           IF J > UNIQUE-WORD-COUNT AND UNIQUE-WORD-COUNT < 50
               ADD 1 TO UNIQUE-WORD-COUNT
               MOVE CURRENT-WORD TO WORD-TEXT(UNIQUE-WORD-COUNT)
               MOVE 1 TO WORD-FREQ(UNIQUE-WORD-COUNT)
           END-IF.
       
       ANALYZE-SENTENCES.
           MOVE ZERO TO CURRENT-SENTENCE-LENGTH.
           MOVE ZERO TO CURRENT-SENTENCE-WORDS.
           MOVE ZERO TO J.
           
           PERFORM VARYING I FROM 1 BY 1 UNTIL I > BUFFER-LENGTH
               MOVE INPUT-BUFFER(I:1) TO CURRENT-CHAR
               ADD 1 TO CURRENT-SENTENCE-LENGTH
               
               IF CURRENT-CHAR >= 'A' AND CURRENT-CHAR <= 'Z' OR
                  CURRENT-CHAR >= 'a' AND CURRENT-CHAR <= 'z'
                   IF I = 1 OR INPUT-BUFFER(I-1:1) = SPACE
                       ADD 1 TO CURRENT-SENTENCE-WORDS
                   END-IF
               END-IF
               
               EVALUATE CURRENT-CHAR
                   WHEN '.' OR '!' OR '?'
                       ADD 1 TO J
                       IF J <= 20
                           MOVE CURRENT-SENTENCE-LENGTH TO 
                               SENTENCE-LENGTHS(J)
                           MOVE CURRENT-SENTENCE-WORDS TO 
                               SENTENCE-WORDS(J)
                           
                           IF CURRENT-SENTENCE-LENGTH > LONGEST-SENTENCE
                               MOVE CURRENT-SENTENCE-LENGTH TO 
                                   LONGEST-SENTENCE
                           END-IF
                           
                           IF CURRENT-SENTENCE-LENGTH < SHORTEST-SENTENCE
                               MOVE CURRENT-SENTENCE-LENGTH TO 
                                   SHORTEST-SENTENCE
                           END-IF
                       END-IF
                       
                       MOVE ZERO TO CURRENT-SENTENCE-LENGTH
                       MOVE ZERO TO CURRENT-SENTENCE-WORDS
               END-EVALUATE
           END-PERFORM.
       
       CALCULATE-STATISTICS.
           IF WORD-COUNT > 0
               COMPUTE AVERAGE-WORD-LENGTH = TOTAL-WORD-CHARS / WORD-COUNT
           END-IF.
           
           IF SENTENCE-COUNT > 0
               COMPUTE AVG-SENTENCE-LENGTH = BUFFER-LENGTH / SENTENCE-COUNT
           END-IF.
       
       TRANSFORM-TEXT.
           MOVE SPACES TO UPPERCASE-BUFFER.
           MOVE SPACES TO LOWERCASE-BUFFER.
           MOVE SPACES TO TITLE-CASE-BUFFER.
           MOVE SPACES TO REVERSED-BUFFER.
           
           PERFORM VARYING I FROM 1 BY 1 UNTIL I > BUFFER-LENGTH
               MOVE INPUT-BUFFER(I:1) TO CURRENT-CHAR
               
               MOVE FUNCTION UPPER-CASE(CURRENT-CHAR) TO 
                   UPPERCASE-BUFFER(I:1)
               MOVE FUNCTION LOWER-CASE(CURRENT-CHAR) TO 
                   LOWERCASE-BUFFER(I:1)
               
               COMPUTE J = BUFFER-LENGTH - I + 1
               MOVE CURRENT-CHAR TO REVERSED-BUFFER(J:1)
           END-PERFORM.
           
           MOVE 'Y' TO IN-WORD-FLAG.
           PERFORM VARYING I FROM 1 BY 1 UNTIL I > BUFFER-LENGTH
               MOVE INPUT-BUFFER(I:1) TO CURRENT-CHAR
               
               IF CURRENT-CHAR >= 'A' AND CURRENT-CHAR <= 'Z' OR
                  CURRENT-CHAR >= 'a' AND CURRENT-CHAR <= 'z'
                   IF IN-WORD-FLAG = 'Y'
                       MOVE FUNCTION UPPER-CASE(CURRENT-CHAR) TO 
                           TITLE-CASE-BUFFER(I:1)
                       MOVE 'N' TO IN-WORD-FLAG
                   ELSE
                       MOVE FUNCTION LOWER-CASE(CURRENT-CHAR) TO 
                           TITLE-CASE-BUFFER(I:1)
                   END-IF
               ELSE
                   MOVE CURRENT-CHAR TO TITLE-CASE-BUFFER(I:1)
                   MOVE 'Y' TO IN-WORD-FLAG
               END-IF
           END-PERFORM.
       
       DISPLAY-COMPREHENSIVE-REPORT.
           DISPLAY 'TEXT PROCESSING ANALYSIS REPORT'.
           DISPLAY '================================'.
           DISPLAY ' '.
           
           DISPLAY 'BASIC STATISTICS:'.
           DISPLAY '================='.
           DISPLAY 'Total Characters: ' CHAR-COUNT.
           DISPLAY 'Total Words: ' WORD-COUNT.
           DISPLAY 'Total Sentences: ' SENTENCE-COUNT.
           DISPLAY 'Total Paragraphs: ' PARAGRAPH-COUNT.
           DISPLAY 'Unique Words: ' UNIQUE-WORD-COUNT.
           DISPLAY ' '.
           
           DISPLAY 'CHARACTER ANALYSIS:'.
           DISPLAY '==================='.
           DISPLAY 'Vowels: ' VOWEL-COUNT.
           DISPLAY 'Consonants: ' CONSONANT-COUNT.
           DISPLAY 'Digits: ' DIGIT-COUNT.
           DISPLAY 'Spaces: ' SPACE-COUNT.
           DISPLAY 'Special Characters: ' SPECIAL-CHAR-COUNT.
           DISPLAY 'Most Frequent Letter: ' MOST-FREQUENT-CHAR 
               ' (' HIGHEST-CHAR-FREQ ' times)'.
           DISPLAY ' '.
           
           DISPLAY 'WORD STATISTICS:'.
           DISPLAY '================'.
           DISPLAY 'Longest Word: ' LONGEST-WORD 
               ' (' LONGEST-WORD-LENGTH ' characters)'.
           DISPLAY 'Shortest Word: ' SHORTEST-WORD 
               ' (' SHORTEST-WORD-LENGTH ' characters)'.
           DISPLAY 'Average Word Length: ' AVERAGE-WORD-LENGTH 
               ' characters'.
           DISPLAY ' '.
           
           DISPLAY 'SENTENCE ANALYSIS:'.
           DISPLAY '=================='.
           DISPLAY 'Longest Sentence: ' LONGEST-SENTENCE ' characters'.
           DISPLAY 'Shortest Sentence: ' SHORTEST-SENTENCE ' characters'.
           DISPLAY 'Average Sentence Length: ' AVG-SENTENCE-LENGTH 
               ' characters'.
           DISPLAY ' '.
           
           DISPLAY 'TOP 5 MOST FREQUENT WORDS:'.
           DISPLAY '==========================='.
           PERFORM VARYING I FROM 1 BY 1 UNTIL I > 5
               IF I <= UNIQUE-WORD-COUNT
                   DISPLAY I '. ' WORD-TEXT(I) ' (' WORD-FREQ(I) ' times)'
               END-IF
           END-PERFORM.
           DISPLAY ' '.
           
           DISPLAY 'TEXT TRANSFORMATIONS:'.
           DISPLAY '====================='.
           DISPLAY 'Original: ' INPUT-BUFFER(1:80).
           DISPLAY 'Uppercase: ' UPPERCASE-BUFFER(1:80).
           DISPLAY 'Lowercase: ' LOWERCASE-BUFFER(1:80).
           DISPLAY 'Title Case: ' TITLE-CASE-BUFFER(1:80).
           DISPLAY 'Reversed: ' REVERSED-BUFFER(1:80).
       
       STOP RUN.`
  }
];