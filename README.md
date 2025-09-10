
# InCollege — GNU COBOL Project

A simple COBOL program that supports **account creation** and **login** using file-driven input/output.
Runs in **Docker + VS Code Dev Containers** (cross-platform: Windows/macOS/Linux).

---

## Quick Start

### Prerequisites

* **Docker Desktop** ([download](https://www.docker.com/products/docker-desktop))
* **VS Code** ([download](https://code.visualstudio.com/))
* Install the VS Code **Dev Containers** extension (`ms-vscode-remote.remote-containers`)

### Build & Run

1. Clone this repo and open it in VS Code.
2. Reopen in container when prompted.
3. Build & run:

   ```bash
   cobc -x -free InCollege.cob -o InCollege
   ./InCollege
   ```

---

## Input & Output

* **Runtime input:** `InCollege-Input.txt`
  (plain text, one token per line: choice, username, password, etc.)
* **Runtime output:** `InCollege-Output.txt`
  (mirrors every message shown to the screen)
* **Persistent users:** `users.dat` (`username,password`)
* **Optional tests:** `InCollege-Test.txt`

---

## Input Format

### Create Account

```
2
username
Password1!
```

### Login

```
1
username
Password1!
```

---

## Example

**`InCollege-Input.txt`**

```
2
userA
Abcd123!
1
userA
Abcd123!
```

**`InCollege-Output.txt`**

```
You have successfully logged in.
Welcome, userA!
```

---

## Test Cases

* \#1–#5 → Create users A–E successfully
* \#6–#9 → Invalid passwords (too short, no uppercase, no digit, no special)
* \#10 → Login wrong once, then right
* \#11 → Login wrong, then EOF
* \#12 → Account limit (max 5 users)
* \#13 → Invalid initial choice
* \#14 → Case-insensitive command
* \#15 → Leading whitespace tolerated

---

## License

MIT

