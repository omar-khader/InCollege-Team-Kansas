# InCollege - Week 10: System Enhancements & Bug Fixing

## Project Overview
InCollege is a professional networking application built in COBOL that enables users to create profiles, connect with others, search for jobs, and communicate through a messaging system. This Week 10 deliverable focuses on comprehensive bug fixing, quality-of-life improvements, and ensuring overall system stability for the alpha release.

## Week 10 Focus Areas
- **Comprehensive Bug Fixing**: Thorough testing and resolution of all identified bugs from Weeks 1-9
- **Quality-of-Life Enhancements**: Minor improvements to user experience and interface
- **Code Refactoring**: Improved readability, consistency, and efficiency
- **System Stability**: Robust error handling and graceful failure management
- **Documentation**: Complete user and testing documentation

## Compilation Instructions

To compile the program, use the following command:
```bashcobc -x -free InCollege.cob -o InCollege

For GnuCOBOL specifically:
```bashcobc -x -free -std=default InCollege.cob -o InCollege

## Running the Program

The program reads input from `InCollege-Input.txt` and writes output to both the console and `InCollege-Output.txt`.
```bash./InCollege

**Note**: Ensure `InCollege-Input.txt` exists in the same directory before running.

## Main Menu Structure

### Welcome Menu========================================
Welcome to InCollege![1] Log In
[2] Create New Account
[3] ExitEnter your choice:

### Post-Login Main Menu========================================
MAIN MENU[1] Create/Edit My Profile
[2] Search for a Job/Internship
[3] View My Profile
[4] Find Someone You Know
[5] View My Network
[6] Learn a New Skill
[7] View Pending Connection Requests
[8] Messages
[9] LogoutEnter your choice:

## Feature Overview

### 1. User Authentication

- **Create Account**: Register new users with validated credentials
- **Login**: Secure authentication with username/password
- **Account Limit**: Maximum 5 user accounts
- **Logout**: Secure session termination

**Validation Rules:**
- Username: 1-10 alphanumeric characters, no special characters
- Password: 8-12 characters with uppercase, lowercase, digit, and special character

### 2. Profile Management

- **Create/Edit Profile**: Comprehensive profile with personal and professional information
- **View Profile**: Display complete profile with formatted sections
- **Profile Fields**:
  - Personal: First name, last name, university, major, graduation year
  - About Me: Optional 200-character biography
  - Experience: Up to 3 professional experiences with details
  - Education: Up to 3 education entries

### 3. User Search & Networking

- **Find Someone You Know**: Search users by first and last name
- **Send Connection Request**: Request to connect with other users
- **View Pending Requests**: See and respond to incoming connection requests
- **View My Network**: Display all established connections
- **Accept/Reject Requests**: Manage incoming connection requests

**Connection Request Options:**
- Send request directly from search results
- Accept or reject pending requests
- View all connected users

### 4. Job Board

- **Post Job/Internship**: Create job postings with full details
- **Browse Jobs**: View all available job listings
- **View Job Details**: See complete job information
- **Apply for Jobs**: Submit applications for positions
- **View My Applications**: Track all submitted applications

**Job Posting Fields:**
- Title (required)
- Description (required, max 200 chars)
- Employer (required)
- Location (required)
- Salary (optional)

### 5. Messaging System

- **Send Messages**: Send messages to connected users only
- **View Messages**: See all received messages with timestamps
- **Connection Validation**: Automatic verification of connection status
- **Message Persistence**: All messages stored across sessions

**Message Display Format:**--- Your Messages ---
From: username
Date/Time: MM/DD/YYYY HH:MM:SS
Message: Message content here
Total messages: X

### 6. Skills Learning

- Learn COBOL
- Learn Jira
- Learn Git
- Learn GitHub
- Learn Software Engineering

## Input File Format

The input file (`InCollege-Input.txt`) contains a sequence of menu choices and user inputs, one per line.
