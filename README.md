# Web-based COBOL Interpreter

A Next.js application that provides an in-browser COBOL editor and runner with syntax highlighting and example programs.

## Features

- **COBOL Code Editor**: Monaco Editor with custom COBOL syntax highlighting
- **Example Programs**: 10 pre-loaded COBOL programs covering various concepts
- **Mock Execution**: Realistic program execution simulation
- **Responsive Design**: Works on desktop and mobile devices

## Local Development

```bash
npm install
npm run dev
```

Open [http://localhost:3000](http://localhost:3000) in your browser.

## Docker Deployment

### Build and run with Docker

```bash
docker build -t cobol-interpreter .
docker run -p 3000:3000 cobol-interpreter
```

### Using Docker Compose

```bash
docker-compose up --build
```

## Deployment on Coolify

1. Create a new project in Coolify
2. Connect your Git repository
3. Coolify will automatically detect the Dockerfile
4. Set the port to `3000`
5. Deploy the application

### Environment Variables

- `NODE_ENV=production` (automatically set)
- `NEXT_TELEMETRY_DISABLED=1` (optional, to disable Next.js telemetry)

## Project Structure

```
cobol-interpreter/
├── src/
│   └── app/
│       ├── components/
│       │   └── CobolEditor.tsx
│       ├── cobol-examples.ts
│       ├── page.tsx
│       └── page.module.css
├── Dockerfile
├── docker-compose.yml
└── package.json
```

## Technology Stack

- **Next.js 15** with App Router
- **React 18** with TypeScript
- **Monaco Editor** for code editing
- **Custom COBOL syntax highlighting**
- **Docker** for containerization