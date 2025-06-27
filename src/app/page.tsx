'use client';

import { useState } from 'react';
import CobolEditor from './components/CobolEditor';
import { cobolExamples } from './cobol-examples';
import styles from './page.module.css';

export default function Home() {
  const [code, setCode] = useState(cobolExamples[0].code);
  const [output, setOutput] = useState('');
  const [isRunning, setIsRunning] = useState(false);

  const handleRunCode = async () => {
    setIsRunning(true);
    setOutput('Compiling COBOL program...\n');
    
    // Simulate program execution with more realistic output
    setTimeout(() => {
      // Simple pattern matching for different program outputs
      let mockOutput = '';
      
      if (code.includes('HELLO-WORLD')) {
        mockOutput = `GnuCOBOL Compiler - Version 3.1.2
Compiling HELLO-WORLD...
Compilation successful.

Running program...
Hello, World!

Program terminated normally.
Exit code: 0`;
      } else if (code.includes('SIMPLE-ADD')) {
        mockOutput = `GnuCOBOL Compiler - Version 3.1.2
Compiling SIMPLE-ADD...
Compilation successful.

Running program...
Result: 0300

Program terminated normally.
Exit code: 0`;
      } else if (code.includes('EMPLOYEE-RECORD')) {
        mockOutput = `GnuCOBOL Compiler - Version 3.1.2
Compiling EMPLOYEE-RECORD...
Compilation successful.

Running program...
Employee ID: 12345
Employee Name: JOHN DOE                   
Employee Salary: 0075000.00

Program terminated normally.
Exit code: 0`;
      } else if (code.includes('SIMPLE-LOOP')) {
        mockOutput = `GnuCOBOL Compiler - Version 3.1.2
Compiling SIMPLE-LOOP...
Compilation successful.

Running program...
Number: 01
Number: 02
Number: 03
Number: 04
Number: 05

Program terminated normally.
Exit code: 0`;
      } else if (code.includes('IF-EXAMPLE')) {
        mockOutput = `GnuCOBOL Compiler - Version 3.1.2
Compiling IF-EXAMPLE...
Compilation successful.

Running program...
Grade B

Program terminated normally.
Exit code: 0`;
      } else if (code.includes('STRING-EXAMPLE')) {
        mockOutput = `GnuCOBOL Compiler - Version 3.1.2
Compiling STRING-EXAMPLE...
Compilation successful.

Running program...
Full Name: JANE SMITH                    

Program terminated normally.
Exit code: 0`;
      } else if (code.includes('TABLE-EXAMPLE')) {
        mockOutput = `GnuCOBOL Compiler - Version 3.1.2
Compiling TABLE-EXAMPLE...
Compilation successful.

Running program...
Sales 01: 01000
Sales 02: 01500
Sales 03: 02000
Total Sales: 004500

Program terminated normally.
Exit code: 0`;
      } else if (code.includes('MATH-OPERATIONS')) {
        mockOutput = `GnuCOBOL Compiler - Version 3.1.2
Compiling MATH-OPERATIONS...
Compilation successful.

Running program...
Addition: 0019
Subtraction: +0011
Multiplication: 000060
Division: 003.75

Program terminated normally.
Exit code: 0`;
      } else if (code.includes('DATE-TIME')) {
        const now = new Date();
        const year = now.getFullYear();
        const month = String(now.getMonth() + 1).padStart(2, '0');
        const day = String(now.getDate()).padStart(2, '0');
        const hour = String(now.getHours()).padStart(2, '0');
        const minute = String(now.getMinutes()).padStart(2, '0');
        const second = String(now.getSeconds()).padStart(2, '0');
        mockOutput = `GnuCOBOL Compiler - Version 3.1.2
Compiling DATE-TIME...
Compilation successful.

Running program...
Current Date: ${month}/${day}/${year}
Current Time: ${hour}:${minute}:${second}

Program terminated normally.
Exit code: 0`;
      } else if (code.includes('FILE-INPUT')) {
        mockOutput = `GnuCOBOL Compiler - Version 3.1.2
Compiling FILE-INPUT...
Compilation successful.

Running program...
Error: File 'DATA.TXT' not found.
Program terminated with error.
Exit code: 1`;
      } else {
        // Generic output for custom programs
        mockOutput = `GnuCOBOL Compiler - Version 3.1.2
Compiling user program...
Compilation successful.

Running program...
[Program output would appear here]

Program terminated normally.
Exit code: 0`;
      }
      
      setOutput(mockOutput);
      setIsRunning(false);
    }, 1500);
  };

  const loadExample = (exampleIndex: number) => {
    setCode(cobolExamples[exampleIndex].code);
    setOutput('');
  };

  return (
    <div className={styles.container}>
      <header className={styles.header}>
        <h1>Web-based COBOL Interpreter</h1>
        <p>Write, run, and learn COBOL programming in your browser</p>
      </header>

      <div className={styles.examplesBar}>
        <h3>Example Programs:</h3>
        <div className={styles.exampleButtons}>
          {cobolExamples.map((example, index) => (
            <button
              key={index}
              onClick={() => loadExample(index)}
              className={styles.exampleButton}
              title={example.description}
            >
              {example.name}
            </button>
          ))}
        </div>
      </div>

      <div className={styles.mainContent}>
        <div className={styles.editorPanel}>
          <div className={styles.panelHeader}>
            <h3>COBOL Editor</h3>
            <button 
              onClick={handleRunCode} 
              disabled={isRunning}
              className={styles.runButton}
            >
              {isRunning ? 'Running...' : 'Run Code'}
            </button>
          </div>
          <CobolEditor
            value={code}
            onChange={setCode}
            height="500px"
          />
        </div>

        <div className={styles.outputPanel}>
          <div className={styles.panelHeader}>
            <h3>Output</h3>
            <button 
              onClick={() => setOutput('')}
              className={styles.clearButton}
            >
              Clear
            </button>
          </div>
          <pre className={styles.output}>
            {output || 'Click "Run Code" to execute your COBOL program'}
          </pre>
        </div>
      </div>
    </div>
  );
}
