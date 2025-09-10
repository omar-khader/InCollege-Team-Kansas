```markdown
# InCollege — Week 3 (File-Driven Login & Account Creation)

Single COBOL program that **reads all inputs from a text file** and **mirrors every screen message to an output file**, with persistent users and password validation.  
> This build exits immediately after a successful login (no post-login user menu), per our project setting.

---

## ✅ Assignment Compliance

- **Program name:** `InCollege.cob`
- **Runtime input:** `InCollege-Input.txt` (plain text; one token per line)
- **Test input bundle (optional):** `InCollege-Test.txt`
- **Runtime output:** `InCollege-Output.txt` (exact mirror of printed messages)
- **User persistence:** `users.dat` (line-sequential; `username,password` per line)
- **Account limit:** Max **5** accounts → on 6th attempt prints  
  `All permitted accounts have been created, please come back later`
- **Password policy:** Length **8–12**, **≥1 uppercase**, **≥1 digit**, **≥1 special char**
- **Login attempts:** Unlimited until success (program reads more credentials from the input file)

---

## Project Layout

```

.
├── InCollege.cob                # main COBOL program (free format)
├── InCollege-Input.txt          # input used for a single run
├── InCollege-Output.txt         # output produced by the run
├── users.dat                    # persistent user store (created if absent)
└── InCollege-Test.txt           # optional: bundled scenarios for grading/tests

````

---

## Build & Run

> Requires **GnuCOBOL / cobc** (3.x+ recommended)

```bash
# build
cobc -x -free InCollege.cob -o InCollege

# run (reads InCollege-Input.txt, writes InCollege-Output.txt)
./InCollege
````

> **WSL tip:** Work under your Linux home (e.g., `~/InCollege`) instead of `/mnt/c/...` to avoid Windows file locking.

---

## Input Format (`InCollege-Input.txt`)

The program consumes **line-by-line tokens** (case-insensitive; leading/trailing spaces trimmed).

### Create account

```
create
<username>
<password>
```

* On success, appends to `users.dat`.
* On invalid password, prints the policy message and stops.

### Login

```
login
<username>
<password>
[<username>
<password>
...]
```

* Tries pairs in sequence until success.
* On success, prints success + welcome and **exits** (no post-login menu in this build).

---

## Example Inputs

### A) Create & Login in one shot

```
create
userA
Abcd123!
userA
Abcd123!
```

### B) Invalid password (no special)

```
create
userNoSpecial
Abcd1234
```

### C) Login wrong then right (assumes userA exists)

```
login
userA
Wrong123!
userA
Abcd123!
```

### D) Account limit (run after 5 users already exist)

```
create
userF
Abcd123&
```

---

## Suggested Test Plan (`InCollege-Test.txt`)

Place multiple **blank-line-separated** blocks in `InCollege-Test.txt`, then copy a single block into `InCollege-Input.txt` for each run.

**Recommended seeding order** (create + login users A–E first), then run the limit case:

```
create
userA
Abcd123!
userA
Abcd123!

<blank line>

create
userB
Abcd123@
userB
Abcd123@

<blank line>

create
userC
Abcd123#
userC
Abcd123#

<blank line>

create
userD
Abcd123$
userD
Abcd123$

<blank line>

create
userE
Abcd123%
userE
Abcd123%

<blank line>

# account limit test (6th create)
create
userF
Abcd123&
```

**Extract block N with awk (Linux/WSL):**

```bash
awk -v RS= -v ORS='\n' 'NR==N' InCollege-Test.txt > InCollege-Input.txt
./InCollege
```

---

## Output

Every displayed line is also written to `InCollege-Output.txt`.

Typical messages:

* Invalid choice at start: `Invalid choice.`
* Create over limit: `All permitted accounts have been created, please come back later`
* Invalid password: policy explanation
* Successful login:

  ```
  You have successfully logged in.
  Welcome, <username>!
  ```

---

## Troubleshooting

**“Text file busy / cannot open output file” on build**

* A prior run may still be active, or Windows has locked the file on `/mnt/c`.

  ```bash
  pgrep -fl InCollege | awk '{print $1}' | xargs -r kill -9
  # or build to a temp name, then move atomically
  tmp=$(mktemp -u InCollege.XXXXXX)
  cobc -x -free InCollege.cob -o "$tmp" && mv -f "$tmp" InCollege
  ```

  Prefer building/running from `~/` instead of `/mnt/c/...`.

**Program seems to “hang”**

* If the input file ends (EOF) where another token is expected (e.g., password), this build prints a short EOF message and exits.
  Ensure your `InCollege-Input.txt` contains complete tokens for the scenario.

---

## Notes

* This repository’s default build **exits after successful login** (no post-login UI), matching our “file-driven only” requirement.
* If your grader expects the Week-3 navigation (Job Search / Find Someone / Learn a New Skill), keep a separate branch/tag with that flow enabled.

---

## License

This project is for academic use as part of the “InCollege” assignment.

```
