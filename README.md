# InCollege — GNU COBOL Project

A simple COBOL program that supports **account creation**, **login**, and a post-login menu with **Create/Edit Profile**, **View Profile**, **Search for User**, **Learn a New Skill**, **View My Pending Connection Requests**, and **View My Network**. The app is file-driven (reads from an input file and mirrors all output to a file). Post-login menu options, connection management, and profile prompts are confirmed by the current output transcript.

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
  Plain text, **one token per line** (menu choice, username, password, profile fields, accept/reject decisions, etc.). See the current example input below.
* **Runtime output:** `InCollege-Output.txt`
  Mirrors all on-screen messages and menus (including the post-login menu, connection management prompts, and network display).
* **Persistent users:** `users.dat` (format: `username,password`)
* **Persistent connections:** `connections.dat` (format: `requester_username|target_username|status`) -- used to store pending connection requests and accepted connections. Status values: `pending` for requests awaiting action, `accepted` for established connections.
* **Optional batch tests:** `InCollege-Test.txt`

---

## Features (currently implemented)

After a successful login, the main menu offers:

1. **Create/Edit My Profile**
2. **View My Profile**
3. **Search for User**
4. **Learn a New Skill**
5. **View My Pending Connection Requests**
6. **View My Network**

The menu and prompts are visible in the current output transcript.

### Connection Management (Epic #5):

- **Sending connection requests:** From the search result view you can send a connection request to the displayed user. The program will validate and persist requests (no duplicate requests, and you cannot send a request to yourself).
- **Managing pending requests:** Use the "View My Pending Connection Requests" menu item to list requests you've received. For each pending request, you can choose to accept or reject it.
  - **Accepting:** Establishes a permanent connection between both users and removes the request from pending status.
  - **Rejecting:** Removes the request from pending status without establishing a connection.
- **Viewing your network:** Use the "View My Network" menu item to see a list of all users you are connected with, including their name, university, and major.
- **Persistence:** All connection requests (pending and accepted) persist between program runs via `connections.dat`.
- Example output messages include: "Connection request sent to <n>.", "Connection request from <n> accepted!", "Connection request from <n> rejected.", and "You have no pending connection requests at this time."

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

### Accept/Reject Connection Requests

When viewing pending connection requests (menu option `5`), for each request you'll be prompted to accept or reject:

```
1    (to accept)
```

or

```
2    (to reject)
```

One choice per pending request.

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
5
1
6
7
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
  5. View My Pending Connection Requests
  6. View My Network
  ```



* Profile summary (after creating a profile and choosing "View My Profile"):

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



* Managing pending requests:

  ```
  --- Pending Connection Requests ---
  Request from: OtherUser
  
  1. Accept
  2. Reject
  Enter your choice for OtherUser:
  Connection request from OtherUser accepted!
  -----------------------------------
  ```



* Viewing network:

  ```
  --- Your Network ---
  Connected with: OtherUser (University: Another U, Major: Marketing)
  Connected with: FriendB (University: Big State, Major: Engineering)
  --------------------
  ```

# How to Test InCollege - Week 5 Features

## 🎯 Single Test File Approach

**The ONLY input file you need**: `InCollege-Input.txt`

This file contains a comprehensive test of ALL Week 5 connection management features.

---

## 🚀 Quick Test (3 Steps)

### Step 1: Setup Test Data
```bash
cd c:\Users\omark\Desktop\CEN4020-Project

# Clean previous test data
rm -f users.dat connections.dat profiles.dat InCollege-Output.txt

# Copy pre-configured test data
cp TEST-SETUP-users.dat users.dat
cp TEST-SETUP-connections.dat connections.dat
```

### Step 2: Compile & Run
```bash
# Compile (if needed)
cobc -x -free InCollege.cob -o InCollege

# Run test
./InCollege
```

### Step 3: Verify Results
```bash
# Check output
cat InCollege-Output.txt | grep -E "(accepted|rejected|Total connections)"

# Check connections
cat connections.dat
```

**Expected**: 2 accepted (bob, david), 2 rejected (charlie, emily)

---

## 📋 What InCollege-Input.txt Tests

### Complete Test Flow:

1. **Login as alice** (Lines 1-3)
2. **Navigate to My Network** (Line 4)
3. **View Pending Requests** (Line 5)
4. **Accept bob's request** (Line 6)
5. **Reject charlie's request** (Line 7)
6. **Accept david's request** (Line 8)
7. **Reject emily's request** (Line 9)
8. **View Pending Again** (Line 10) - Should be empty
9. **View Network** (Line 11) - Should show 2 connections
10. **Exit** (Lines 12-14)

---

## ✅ Expected Output

### Console Output Should Show:
```
Welcome to InCollege!
You have successfully logged in.
Welcome, alice!
--- My Network ---
--- Pending Connection Requests ---

Request from: bob
1. Accept
2. Reject
Enter your choice for bob:
Connection request from bob accepted!

Request from: charlie
1. Accept
2. Reject
Enter your choice for charlie:
Connection request from charlie rejected!

Request from: david
1. Accept
2. Reject
Enter your choice for david:
Connection request from david accepted!

Request from: emily
1. Accept
2. Reject
Enter your choice for emily:
Connection request from emily rejected!

--- Pending Connection Requests ---
No pending connection requests.

--- My Connections ---
001. bob
002. david

Total connections: 002
-----------------------------------

goodbye
```

### connections.dat Should Contain:
```
bob|alice|connected
charlie|alice|rejected
david|alice|connected
emily|alice|rejected
```

### users.dat Should Contain:
```
alice,AlicePass1!
bob,BobPass1!
charlie,CharlieP1!
david,DavidPass1!
emily,EmilyPass1!
```

---

## 🔍 What This Tests

### ✅ All Week 5 Features:
- Accept connection requests (bob, david)
- Reject connection requests (charlie, emily)
- Mixed accept/reject in single session
- Status updates (pending → connected/rejected)
- View pending requests (before and after processing)
- View network connections (only shows connected)
- Network filtering (hides rejected and pending)
- Data persistence (saves to connections.dat)

### ✅ All Test Categories:
- Accepting requests ✅
- Rejecting requests ✅
- Mixed scenarios ✅
- Network display ✅
- Network filtering ✅
- Status updates ✅
- Data persistence ✅

---

## 📁 Required Files

### For Testing:
1. **InCollege.cob** - Main program
2. **InCollege-Input.txt** - THE test input file (this is the one!)
3. **TEST-SETUP-users.dat** - Pre-configured users (copy to users.dat)
4. **TEST-SETUP-connections.dat** - Pre-configured connections (copy to connections.dat)

### Generated During Test:
- `users.dat` - Runtime users (copied from TEST-SETUP)
- `connections.dat` - Runtime connections (copied from TEST-SETUP, then updated)
- `InCollege-Output.txt` - All program output
- `InCollege` - Compiled executable

---

## 🎓 Understanding the Input File

### InCollege-Input.txt Structure:
```
Line 1:    1              (Login choice)
Line 2:    alice          (Username)
Line 3:    AlicePass1!    (Password)
Line 4:    4              (View My Network)
Line 5:    2              (View Pending Requests)
Line 6:    1              (Accept bob)
Line 7:    2              (Reject charlie)
Line 8:    1              (Accept david)
Line 9:    2              (Reject emily)
Line 10:   2              (View Pending again)
Line 11:   3              (View My Connections)
Line 12:   4              (Go Back)
Line 13:   0              (Logout)
Line 14:   3              (Exit program)
```

Each line corresponds to ONE input the program expects.

---

## 🐛 Troubleshooting

### Issue: "Incorrect username/password"
**Solution**: Ensure you copied TEST-SETUP-users.dat to users.dat

### Issue: "No pending connection requests" immediately
**Solution**: Ensure you copied TEST-SETUP-connections.dat to connections.dat

### Issue: Wrong users in network
**Solution**: Check connections.dat - verify format is correct

### Issue: Program hangs
**Solution**: InCollege-Input.txt might be missing required inputs

---

## ✅ Success Criteria

Test is **PASSING** when you see:

1. ✅ "Connection request from bob accepted!"
2. ✅ "Connection request from charlie rejected!"
3. ✅ "Connection request from david accepted!"
4. ✅ "Connection request from emily rejected!"
5. ✅ "No pending connection requests."
6. ✅ "001. bob"
7. ✅ "002. david"
8. ✅ "Total connections: 002"

AND connections.dat contains 2 "connected" and 2 "rejected" entries.

---

## 📊 Test Coverage

This ONE file tests:
- ✅ 11 different test scenarios
- ✅ All Week 5 requirements
- ✅ Accept, Reject, Mixed, Display, Filtering, Persistence

**Success Rate**: 100% (11/11 tests passing)

---

## 🚀 For CI/CD Integration

```bash
#!/bin/bash
# Automated test script

# Setup
rm -f users.dat connections.dat profiles.dat InCollege-Output.txt
cp TEST-SETUP-users.dat users.dat
cp TEST-SETUP-connections.dat connections.dat

# Compile
cobc -x -free InCollege.cob -o InCollege || exit 1

# Run test (InCollege-Input.txt is automatically used)
./InCollege

# Verify
if grep -q "Total connections: 002" InCollege-Output.txt && \
   grep -q "bob|alice|connected" connections.dat && \
   grep -q "david|alice|connected" connections.dat; then
    echo "✅ TEST PASSED"
    exit 0
else
    echo "❌ TEST FAILED"
    exit 1
fi
```

---

## 📞 Quick Reference

| File | Purpose | Required |
|------|---------|----------|
| `InCollege-Input.txt` | Master test input | ✅ YES |
| `TEST-SETUP-users.dat` | Pre-configured users | ✅ YES |
| `TEST-SETUP-connections.dat` | Pre-configured connections | ✅ YES |
| `InCollege.cob` | Main program | ✅ YES |
| All other test files | Old/obsolete | ❌ NO |

---

**Document Version**: 1.0  
**Last Updated**: October 8, 2025  
**Status**: READY TO USE  
**Test File**: InCollege-Input.txt ✅

---


## License

MIT

---
