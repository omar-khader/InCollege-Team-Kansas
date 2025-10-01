# InCollege — GNU COBOL Project

A simple COBOL program that supports **account creation**, **login**, and a post-login menu with **Create/Edit Profile**, **View Profile**, **Search for User**, and **Learn a New Skill**. The app is file-driven (reads from an input file and mirrors all output to a file). Post-login menu options and profile prompts are confirmed by the current output transcript.

Runs in **Docker + VS Code Dev Containers** (cross-platform: Windows/macOS/Linux).

---

## Quick Start

### Prerequisites

* **Docker Desktop** (install)
* **VS Code** (install)
* VS Code **Dev Containers** extension (`ms-vscode-remote.remote-containers`)

### Build & Run

1. Clone this repo and open it in VS Code.
2. **Reopen in container** when prompted.
3. Build & run:

   ```bash
   cobc -x -free InCollege.cob -o InCollege
   ./InCollege
   ```

> The program reads from `InCollege-Input.txt` and mirrors all console output to `InCollege-Output.txt`. A successful login looks like:
>
> ```
> You have successfully logged in.
> Welcome, TestUser!
> ```
>
>

---

## Files & I/O

* **Runtime input:** `InCollege-Input.txt`
  Plain text, **one token per line** (menu choice, username, password, profile fields, etc.). See the current example input below.
* **Runtime output:** `InCollege-Output.txt`
  Mirrors all on-screen messages and menus (including the post-login menu and profile summary).
* **Persistent users:** `users.dat` (format: `username,password`)
* **Persistent connections:** `connections.dat` (format: `requester_username|target_username|status`) -- used to store pending connection requests and accepted connections.
* **Optional batch tests:** `InCollege-Test.txt`

---

## Features ( currently implemented)

After a successful login, the main menu offers:

1. **Create/Edit My Profile**
2. **View My Profile**
3. **Search for User**
4. **Learn a New Skill**
5. **View My Pending Connection Requests**
   The menu and prompts are visible in the current output transcript.

Connection requests (new):

- From the search result view you can send a connection request to the displayed user. The program will validate and persist requests (no duplicate requests, and you cannot send a request to yourself).
- Use the "View My Pending Connection Requests" menu item to list requests you've received. Requests are saved in `connections.dat` so they persist between runs.
- Example output messages include: "Connection request sent to <Name>.", "Connection request already sent to this user.", and "You have no pending connection requests at this time."


---

## Input Format

> **Important:** The program consumes inputs line-by-line from `InCollege-Input.txt`. Keep exactly one value per line.

### Create Account

```
2
username
Password1!
```

### Log In

```
1
username
Password1!
```

### Create/Edit Profile (sample sequence)

Below is a minimal example of profile data (after logging in, choose `1` for Create/Edit Profile, then provide fields; use `DONE` to finish optional lists):

```
John
Doe
Example University
Computer Science
2025
Enthusiastic developer always learning new things.
Software Intern
Tech Corp
Summer 2024
Developed tools for internal testing.
DONE
Bachelor of Science
Example University
2021-2025
DONE
```

This ordering matches the current profile prompts shown by the program.

---

## End-to-End Example

**`InCollege-Input.txt` (current sample)**

```
2
TestUser
TestPass123!
1
TestUser
TestPass123!
1
John
Doe
Example University
Computer Science
2025
Enthusiastic developer always learning new things.
Software Intern
Tech Corp
Summer 2024
Developed tools for internal testing.
DONE
Bachelor of Science
Example University
2021-2025
DONE
2
```



**`InCollege-Output.txt` (key excerpts you should see)**

* Login success:

  ```
  You have successfully logged in.
  Welcome, TestUser!
  ```



* Post-login menu:

  ```
  1. Create/Edit My Profile
  2. View My Profile
  3. Search for User
  4. Learn a New Skill
  ```



* Profile summary (after creating a profile and choosing “View My Profile”):

  ```
  --- Your Profile ---
  Name: John Doe
  University: Example University
  Major: Computer Science
  Graduation Year: 2025
  About Me: Enthusiastic developer always learning new things.
  Experience:
  Title: Software Intern
  Company: Tech Corp
  Dates: Summer 2024
  Description: Developed tools for internal testing.
  Education:
  Degree: Bachelor of Science
  University: Example University
  Years: 2021-2025
  ```



---


## License

MIT

---
