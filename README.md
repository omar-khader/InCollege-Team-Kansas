# InCollege - Week 10: System Enhancements & Bug Fixing
## Project Overview
InCollege is a professional networking application built in COBOL that enables users to create profiles, connect with others, search for jobs, and communicate through a messaging system. This Week 10 deliverable focuses on comprehensive bug fixing, quality-of-life improvements, and ensuring overall system stability for the alpha release.
Week 10 Focus Areas

## Comprehensive Bug Fixing: Thorough testing and resolution of all identified bugs from Weeks 1-9
Quality-of-Life Enhancements: Minor improvements to user experience and interface
Code Refactoring: Improved readability, consistency, and efficiency
System Stability: Robust error handling and graceful failure management
Documentation: Complete user and testing documentation

## Prerequisites

GnuCOBOL compiler (or any COBOL compiler)
Text editor for viewing/editing input files
Terminal/Command prompt access

## File Structure
InCollege/
├── InCollege.cob              # Main COBOL program (fully tested & debugged)
├── InCollege-Input.txt        # Comprehensive test input file
├── InCollege-Output.txt       # Generated output file
├── users.dat                  # User credentials storage
├── profiles.dat               # User profile information
├── connections.dat            # User connections and requests
├── jobs.dat                   # Job postings
├── applications.dat           # Job applications
├── messages.dat               # User messages
├── Connections-Output.txt     # Connection-specific output
└── README.md                  # This file
## Compilation Instructions
To compile the program, use the following command:
bashcobc -x -free InCollege.cob -o InCollege
For GnuCOBOL specifically:
bashcobc -x -free -std=default InCollege.cob -o InCollege
Running the Program
The program reads input from InCollege-Input.txt and writes output to both the console and InCollege-Output.txt.
bash./InCollege
```

**Note**: Ensure `InCollege-Input.txt` exists in the same directory before running.

## Main Menu Structure

### Welcome Menu
```
========================================
     Welcome to InCollege!             
========================================

  [1] Log In
  [2] Create New Account
  [3] Exit

Enter your choice:
```

### Post-Login Main Menu
```
========================================
          MAIN MENU                     
========================================

  [1] Create/Edit My Profile
  [2] Search for a Job/Internship
  [3] View My Profile
  [4] Find Someone You Know
  [5] View My Network
  [6] Learn a New Skill
  [7] View Pending Connection Requests
  [8] Messages
  [9] Logout

Enter your choice:
