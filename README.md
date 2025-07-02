# jobforge

An OTP application for sorting and executing shell command tasks with dependencies.

## Features

- Accepts a list of tasks (with dependencies) via HTTP POST.
- Returns a sorted list of tasks in execution order (dependencies first).
- Detects and reports cycles in dependencies.
- Can return a bash script representation of the sorted commands.
- Efficient for large task graphs (tested up to 100,000 tasks).
- Comprehensive test suite.

---

## Build & Run

### Prerequisites

- Erlang/OTP v26
- [rebar3](https://www.rebar3.org/) build tool

### Build

```sh
cd jobforge
rebar3 compile
```

### Run the Application

```sh
rebar3 shell
```

This will start the Cowboy HTTP server on port 8080.

---

## API Usage

### 1. Sort Tasks (JSON Output)

**Endpoint:**  
`POST http://localhost:8080/v1/job`

**Request Body Example:**
```json
{
  "tasks": [
    { "name": "task-1", "command": "touch /tmp/file1" },
    { "name": "task-2", "command": "cat /tmp/file1", "requires": ["task-3"] },
    { "name": "task-3", "command": "echo 'Hello World!' > /tmp/file1", "requires": ["task-1"] },
    { "name": "task-4", "command": "rm /tmp/file1", "requires": ["task-2", "task-3"] }
  ]
}
```

**Curl Example:**
```sh
curl -X POST http://localhost:8080/v1/job \
  -H "Content-Type: application/json" \
  -d '{
    "tasks": [
      { "name": "task-1", "command": "touch /tmp/file1", "requires": [] },
      { "name": "task-2", "command": "cat /tmp/file1", "requires": ["task-3"] },
      { "name": "task-3", "command": "echo '\''Hello World!'\'' > /tmp/file1", "requires": ["task-1"] },
      { "name": "task-4", "command": "rm /tmp/file1", "requires": ["task-2", "task-3"] }
    ]
  }'
```

**Response Example:**
```json
{
  "tasks": [
    { "name": "task-1", "command": "touch /tmp/file1" },
    { "name": "task-3", "command": "echo 'Hello World!' > /tmp/file1" },
    { "name": "task-2", "command": "cat /tmp/file1" },
    { "name": "task-4", "command": "rm /tmp/file1" }
  ]
}
```

---

### 2. Bash Script Output

**Endpoint:**  
`POST http://localhost:8080/v1/job/bash`

**Curl Example:**
```sh
curl -X POST http://localhost:8080/v1/job/bash \
  -H "Content-Type: application/json" \
  -d '{
    "tasks": [
      { "name": "task-1", "command": "touch /tmp/file1" },
      { "name": "task-2", "command": "cat /tmp/file1", "requires": ["task-3"] },
      { "name": "task-3", "command": "echo '\''Hello World!'\'' > /tmp/file1", "requires": ["task-1"] },
      { "name": "task-4", "command": "rm /tmp/file1", "requires": ["task-2", "task-3"] }
    ]
  }'
```

**Response Example:**
```bash
#!/usr/bin/env bash
touch /tmp/file1
echo 'Hello World!' > /tmp/file1
cat /tmp/file1
rm /tmp

## Performance & Load Testing

This project includes a large-scale test to demonstrate performance and scalability.

### Run the Large Test

To run the full test suite, including a test with 100,000 tasks:

```sh
rebar3 ct --readable=true
```

The large test will use cached/generated data and report the time taken to sort 100,000 tasks.

### Example Test Report

```
Loaded tasks from cache: /Users/gurwinder/Documents/Sort-Commands/jobforge/tmp/tasks_N100000_D3_Cfalse.json
* User 2025-07-02 17:45:08.624 *
Generated Tasks List of length 100000
* User 2025-07-02 17:45:08.932 *
Time taken: 307ms
=== successfully completed test case
=== Returned value: ok
```


- The test suite will print the time taken for the large acyclic sort.
- All test logs and reports are available in `_build/test/logs/`.

### Latest Test Results

For convenience, the latest test log output is also available in [`test/reports/`](test/reports/) (if you copy or symlink the latest log there).

---

## Test Output Location

- All Common Test logs are written to `_build/test/logs/`.
- You can view detailed HTML and text reports there after running the tests.

---
# jobforge-erl
