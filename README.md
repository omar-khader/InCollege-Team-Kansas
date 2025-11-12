# InCollege - Week 9: Messaging System (View Messages)

## Project Overview
InCollege is a professional networking application built in COBOL that enables users to create profiles, connect with others, search for jobs, and communicate through a messaging system. This Week 9 deliverable implements the message viewing functionality, completing the two-way communication feature.

## New Features (Week 9)
- **View Received Messages**: Users can now view all messages that have been sent to them
- **Message Display**: Messages show sender username, content, and timestamp
- **No Messages Handling**: Appropriate message displayed when user has no messages
- **Persistent Message Storage**: Messages sent in previous sessions are retained and displayed

## Prerequisites
- GnuCOBOL compiler (or any COBOL compiler)
- Text editor for viewing/editing input files
- Terminal/Command prompt access

## File Structure
```
InCollege/
├── InCollege.cob              # Main COBOL program
├── InCollege-Input.txt        # Sample input file for testing
├── InCollege-Output.txt       # Generated output file
├── users.dat                  # User credentials storage
├── profiles.dat               # User profile information
├── connections.dat            # User connections and requests
├── jobs.dat                   # Job postings
├── applications.dat           # Job applications
├── messages.dat               # User messages (Week 8-9)
├── Connections-Output.txt     # Connection-specific output
└── README.md                  # This file
```

## Compilation Instructions
To compile the program, use the following command:

```bash
cobc -x -free InCollege.cob -o InCollege
```

For GnuCOBOL specifically:
```bash
cobc -x -free -std=default InCollege.cob -o InCollege
```

## Running the Program
The program reads input from `InCollege-Input.txt` and writes output to both the console and `InCollege-Output.txt`.

```bash
./InCollege
```

**Note**: Ensure `InCollege-Input.txt` exists in the same directory before running.

## Input File Format
The input file (`InCollege-Input.txt`) contains a sequence of menu choices and user inputs, one per line. 

### Week 9 Specific Input Flow - Viewing Messages

To view messages after logging in:
```
1                    # Log In
username             # Enter username
password             # Enter password
8                    # Messages menu option
2                    # View My Messages
3                    # Back to Main Menu (from Messages menu)
0                    # Logout
```

### Complete Week 9 Sample Flow
```
2                    # Create account (user1)
user1
Pass1234!
2                    # Create account (user2)
user2
Pass1234!
1                    # Login as user2
user2
Pass1234!
1                    # Create profile
Jane
Smith
...
5                    # View My Network
1                    # Send connection request
user1                # To user1
4                    # Go back
0                    # Logout
1                    # Login as user1
user1
Pass1234!
7                    # View pending requests
1                    # Accept request from user2
8                    # Messages menu
1                    # Send new message
user2                # To user2
Hello user2!         # Message content
3                    # Back to main menu
0                    # Logout
1                    # Login as user2
user2
Pass1234!
8                    # Messages menu
2                    # View My Messages
3                    # Back to main menu
0                    # Logout
```

## Messages Menu Options

When selecting option **8** from the main menu, users access the Messages menu:

1. **Send a New Message** (Week 8)
   - Enter recipient username
   - Can only message connected users
   - Message limited to 200 characters
   - Validates user exists and connection status

2. **View My Messages** (Week 9 - NEW)
   - Displays all messages received by the logged-in user
   - Shows sender username, date, and message content
   - Messages displayed chronologically
   - Shows total message count
   - Displays "You have no messages" if no messages exist

3. **Back to Main Menu**
   - Returns to the main post-login menu

## Message Display Format

When viewing messages, the output appears as:
```
--- Your Messages ---
-----------------------------------
From: user2
Date: 20251105
Message: Hi user1! Thanks for your message. This is user2 replying.
-----------------------------------
Total messages: 1
-----------------------------------
```

If no messages exist:
```
--- Your Messages ---
You have no messages.
-----------------------------------
```

## Output Files

### InCollege-Output.txt
Contains all program output including:
- Login/logout prompts and confirmations
- Menu displays
- Profile information
- Connection requests
- Job postings and applications
- **Message viewing output (NEW)**
- All status messages and confirmations

### Connections-Output.txt
Contains output specifically related to connection operations:
- Connection request sending
- Pending request viewing
- Connection acceptance/rejection

## Data File Formats

### messages.dat Format
```
sender_username|recipient_username|message_content|timestamp
```

Example:
```
user1|user2|Hello user2! This is a test message.|20251105143022
user2|user1|Thanks for the message!|20251105150530
```

**Fields:**
- `sender_username`: Username of the message sender (max 32 chars)
- `recipient_username`: Username of the message recipient (max 32 chars)
- `message_content`: The message text (max 200 chars)
- `timestamp`: Date and time in YYYYMMDDHHMMSS format (20 chars)

## Key Features Implemented

### Week 9 Functionality
✅ View all received messages for logged-in user  
✅ Display sender username for each message  
✅ Display message content (up to 200 characters)  
✅ Display timestamp for each message  
✅ Handle "no messages" scenario gracefully  
✅ Persistent message storage across sessions  
✅ Chronological message display  
✅ Total message count display  

### Previous Weeks (Still Functional)
✅ User registration and login  
✅ Profile creation with experience and education  
✅ User search by name  
✅ Connection request system  
✅ Job posting and browsing  
✅ Job application tracking  
✅ Message sending to connected users (Week 8)  

## Testing Scenarios

### Positive Test Cases
1. **Single Message**: User receives one message
2. **Multiple Messages**: User receives multiple messages from different senders
3. **Message Persistence**: Messages sent in previous sessions are displayed
4. **Timestamp Display**: Verify timestamp is shown correctly

### Negative Test Cases
1. **No Messages**: User with no messages sees appropriate message
2. **New User**: Newly created user has no messages

### Edge Cases
1. **Maximum Length Messages**: 200-character messages display correctly
2. **Special Characters**: Messages with special characters display properly
3. **Multiple Sessions**: Messages persist across program restarts

## Validation Rules

### Message Sending (Week 8)
- Users must be connected to send messages
- Message content required (cannot be empty)
- Message limited to 200 characters
- Recipient must be a valid, existing user

### Message Viewing (Week 9)
- Only messages addressed to the logged-in user are displayed
- Messages displayed in order received
- All message data (sender, content, timestamp) must be shown
- Empty message list handled gracefully

## Limitations
- Maximum 100 profiles can be stored
- Maximum 100 job postings
- Maximum 100 connections
- No message deletion functionality (future enhancement)
- No "mark as read" functionality (future enhancement)
- Messages limited to 200 characters

## Troubleshooting

### Common Issues

**"File not found" error:**
- Ensure `InCollege-Input.txt` exists in the program directory
- Check file permissions

**"No messages" when messages should exist:**
- Verify `messages.dat` file exists and is readable
- Check that messages were sent to the correct username
- Confirm users are actually connected

**Output file not created:**
- Check write permissions in the program directory
- Ensure disk space is available

**Compilation errors:**
- Verify GnuCOBOL is properly installed
- Check COBOL syntax for your specific compiler
- Use `-free` flag for free-format COBOL

## Development Team Roles

### Scrum Master
- Facilitate sprint planning and daily standups
- Track progress in Jira
- Remove impediments
- Generate burndown charts

### Programmers
- Implement message viewing functionality
- Parse and display message data
- Handle no-messages scenario
- Integrate with existing file I/O system
- Ensure data persistence

### Testers
- Develop comprehensive test cases
- Execute test scenarios
- Verify output consistency
- Log bugs in Jira
- Validate message persistence across sessions

## Version History
- **Week 9**: Added message viewing functionality
- **Week 8**: Added message sending functionality
- **Week 7**: Added job application tracking
- **Week 6**: Implemented job posting and browsing
- **Week 5**: Connection request system
- **Week 4**: User search functionality
- **Week 3**: Profile viewing and editing
- **Week 2**: User authentication system
- **Week 1**: Initial project setup

## Future Enhancements
- Mark messages as read/unread
- Delete messages
- Message threading/replies
- Message search functionality
- Message notifications
- Attachment support
- Group messaging

## Contact & Support
For issues or questions about this project:
1. Check existing Jira tickets
2. Create a new bug report with detailed steps to reproduce
3. Consult with team members during daily standup

---

**Last Updated**: Week 9  
**Epic**: #9 - Basic Messaging System Part 2 (View Messages)  
**Status**: Complete and Tested
