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
  }
];