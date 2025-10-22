
# InCollege — GNU COBOL Project

A simple COBOL program that supports **account creation**, **login**, and a post-login menu with **Create/Edit Profile**, **View Profile**, **Search for User**, **Learn a New Skill**, **View My Pending Connection Requests**, and **View My Network**. The app is file-driven (reads from an input file and mirrors all output to a file). Post-login menu options, and job posting prompts are confirmed by the current output transcript.

Runs in **Docker + VS Code Dev Containers** (cross-platform: Windows/macOS/Linux).



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


> The program reads from `InCollege-Input.txt` and mirrors all console output to `InCollege-Output.txt`. A successful login looks like:
>
> ```
> You have successfully logged in.
> Welcome, TestUser!
> ```



## Files & I/O

  * **Runtime input:** `InCollege-Input.txt`
    Plain text, **one token per line** (menu choice, username, password, profile fields, job details, etc.). See the current example input below.
  * **Runtime output:** `InCollege-Output.txt`
    Mirrors all on-screen messages and menus (including the post-login menu, connection management prompts, network display, and **job posting prompts**).
  * **Persistent users:** `users.dat` (format: `username,password`)
  * **Persistent connections:** `connections.dat` (format: `requester_username|target_username|status`) -- used to store pending connection requests and accepted connections. Status values: `pending` for requests awaiting action, `accepted` for established connections.
  * **Persistent jobs/internships:** `jobs.dat` (format: `poster_username|title|description|employer|location|salary`) -- used to store all job listings.
  * **Optional batch tests:** `InCollege-Test.txt`

-----

## Features (currently implemented)

After a successful login, the main menu offers:

1.  **Create/Edit My Profile**
2.  **Search for a job** (leads to Job Search/Internship Menu)
3.  **View My Profile**
4.  **Find someone you know**
5.  **View My Network**
6.  **Learn a New Skill**
7.  **View My Pending Connection Requests**

### Job Board Functionality (Epic \#6):

  * The **"Search for a job"** option now leads to a **"Job Search/Internship Menu"**.
  * From this sub-menu, the user can choose to **"Post a Job/Internship"**.
      * The system prompts for **Job Title, Description, Employer, and Location** (all required).
      * **Salary** is an **optional** field.
      * All valid job listings are saved persistently in `jobs.dat`.
  * The **"Browse Jobs/Internships"** option remains **"under construction"**.

-----

## Input Format

> **Important:** The program consumes inputs line-by-line from `InCollege-Input.txt`. Keep exactly one value per line.

### Post a Job/Internship (sample sequence)

After logging in, choose the menu options to navigate to the job posting feature (usually `2` then `1`), then provide the job details in order.

```
2
1
Software Intern
Exciting opportunity to work with cutting-edge technologies
Tech Solutions Inc
Tampa, FL
$20/hour
```

*The salary field is optional; you must enter `NONE` to skip it.*

### Create Account, Log In, Create/Edit Profile, Accept/Reject Connection Requests

(Input format for these previous features remains unchanged.)



## Current Test Flow: InCollege-Input.txt

The current sample input file executes the following sequence:

1.  **Create New Account** (`2`) for user **MyUser** with password **MyPass@1**.
2.  **Log In** (`1`) with the newly created credentials, resulting in a successful login.
3.  Navigate to the **Job Search/Internship Menu** (`2` from the post-login menu).
4.  Select **Post a Job/Internship** (`1`).
5.  Provide all required and optional fields for a new job posting:
      * **Job Title**: `Software Intern`
      * **Description**: `Exciting opportunity to work with cutting-edge technologies`
      * **Employer**: `Tech Solutions Inc`
      * **Location**: `Tampa, FL`
      * **Salary**: `$20/hour`
6.  Verify the **"Job posted successfully\!"** confirmation message.
7.  Select **Browse Jobs/Internships** (`2` from the job search menu) and verify the **"under construction"** message.
8.  Select **Back to Main Menu** (`3`).

This flow successfully tests the basic required functionality for Week 6's Job Posting feature.

## License

MIT

