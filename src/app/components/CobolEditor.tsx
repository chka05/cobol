'use client';

import { Editor } from '@monaco-editor/react';
import { useRef } from 'react';

/* eslint-disable @typescript-eslint/no-explicit-any */

interface CobolEditorProps {
  value: string;
  onChange: (value: string) => void;
  height?: string;
}

export default function CobolEditor({ value, onChange, height = '600px' }: CobolEditorProps) {
  const editorRef = useRef<any>(null);

  const handleEditorDidMount = (editor: any, monaco: any) => {
    editorRef.current = editor;
    
    // Register COBOL language if not already registered
    if (!monaco.languages.getLanguages().find((lang: any) => lang.id === 'cobol')) {
      monaco.languages.register({ id: 'cobol' });
      
      // Define COBOL syntax highlighting
      monaco.languages.setMonarchTokensProvider('cobol', {
        tokenizer: {
          root: [
            // Comments
            [/^\s*\*.*$/, 'comment'],
            
            // Division headers
            [/\b(IDENTIFICATION|ENVIRONMENT|DATA|PROCEDURE)\s+DIVISION\b/i, 'keyword.division'],
            
            // Section headers
            [/\b(PROGRAM-ID|AUTHOR|DATE-WRITTEN|DATE-COMPILED|SECURITY|REMARKS|CONFIGURATION|INPUT-OUTPUT|FILE-CONTROL|I-O-CONTROL|DATA|FILE|WORKING-STORAGE|LOCAL-STORAGE|LINKAGE|REPORT|SCREEN)\s+SECTION\b/i, 'keyword.section'],
            
            // COBOL keywords
            [/\b(ACCEPT|ADD|CALL|CANCEL|CLOSE|COMPUTE|CONTINUE|DELETE|DISPLAY|DIVIDE|EVALUATE|EXIT|GO|IF|INITIALIZE|INSPECT|MOVE|MULTIPLY|OPEN|PERFORM|READ|RETURN|REWRITE|SEARCH|SET|SORT|START|STOP|STRING|SUBTRACT|UNSTRING|WRITE|ELSE|END-IF|END-PERFORM|END-READ|END-WRITE|END-EVALUATE|END-STRING|END-UNSTRING|WHEN|OTHER|VARYING|FROM|BY|UNTIL|TIMES|THRU|THROUGH|GIVING|TO|INTO|ON|SIZE|ERROR|OVERFLOW|AT|END|NOT|AND|OR|EQUAL|EQUALS|GREATER|LESS|THAN)\b/i, 'keyword'],
            
            // Data types and clauses
            [/\b(PIC|PICTURE|VALUE|OCCURS|REDEFINES|USAGE|SYNC|SYNCHRONIZED|JUST|JUSTIFIED|BLANK|ZERO|ZEROS|ZEROES|SPACE|SPACES|HIGH-VALUE|HIGH-VALUES|LOW-VALUE|LOW-VALUES|QUOTE|QUOTES|ALL)\b/i, 'type'],
            
            // Numeric literals
            [/\b\d+(\.\d+)?\b/, 'number'],
            
            // String literals
            [/'[^']*'/, 'string'],
            [/"[^"]*"/, 'string'],
            
            // Level numbers
            [/^\s*\d{2}\s/, 'constant'],
            
            // Identifiers
            [/[A-Za-z][A-Za-z0-9-]*/, 'identifier']
          ]
        }
      });
      
      // Define theme for COBOL
      monaco.editor.defineTheme('cobol-theme', {
        base: 'vs',
        inherit: true,
        rules: [
          { token: 'comment', foreground: '008000', fontStyle: 'italic' },
          { token: 'keyword.division', foreground: '0000FF', fontStyle: 'bold' },
          { token: 'keyword.section', foreground: '800080', fontStyle: 'bold' },
          { token: 'keyword', foreground: '0000FF' },
          { token: 'type', foreground: '2B91AF' },
          { token: 'string', foreground: 'A31515' },
          { token: 'number', foreground: '098658' },
          { token: 'constant', foreground: '811F3F' }
        ],
        colors: {
          'editor.background': '#FFFFFF'
        }
      });
      
      monaco.editor.setTheme('cobol-theme');
    }
  };

  const handleEditorChange = (value: string | undefined) => {
    if (value !== undefined) {
      onChange(value);
    }
  };

  return (
    <Editor
      height={height}
      language="cobol"
      value={value}
      onChange={handleEditorChange}
      onMount={handleEditorDidMount}
      options={{
        minimap: { enabled: false },
        fontSize: 14,
        lineNumbers: 'on',
        rulers: [7, 11, 72],
        wordWrap: 'off',
        scrollBeyondLastLine: false,
        automaticLayout: true
      }}
    />
  );
}