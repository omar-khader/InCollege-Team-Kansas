# Week 5: Connection Management Testing Plan

## 📋 Overview
This document outlines comprehensive test cases for Week 5 connection management functionalities including accepting requests, rejecting requests, viewing network, and persistence testing.

## 🔍 Code Review Summary

### Branch Analysis Completed:
- **Nicholas Keenan (nick-code)**: StoryEight.cob implements accept/reject functionality
- **Adesh Kessani (Adesh-Dev)**: Multiple story files for connection features (Stories 79, 80, 81, 86, 93, 94, 95, 96, 98)

### Key Findings:
1. **Nicholas's Implementation**: Standalone program with accept/reject in one flow
2. **Adesh's Implementation**: Modular approach with separate programs for different features
3. **Current Status**: Features need to be integrated into main InCollege.cob for comprehensive testing

---

## 🧪 Test Case Categories

### 1. Accepting Connection Requests

#### Test Case 1.1: Accept Single Request
**Objective**: Verify that a user can successfully accept a single pending connection request

**Pre-conditions**:
- User `alice` exists and is logged in
- User `bob` has sent a connection request to `alice`
- Connection exists in `connections.dat`: `bob|alice|pending`

**Test Steps**:
1. Login as `alice`
2. Navigate to "View My Pending Connection Requests" (option 4)
3. System displays pending request from `bob`
4. Select option 1 (Accept) for `bob`'s request
5. Verify confirmation message: "Connection request from bob accepted!"

**Expected Results**:
- ✅ Request status changes from "pending" to "connected" in `connections.dat`
- ✅ Request no longer appears in pending requests list
- ✅ Both users can see each other in their networks
- ✅ Output file matches console output exactly

**Post-conditions**:
- `connections.dat` contains: `bob|alice|connected`
- Network view shows `bob` as a connection for `alice`

---

#### Test Case 1.2: Accept Multiple Requests Sequentially
**Objective**: Verify that a user can accept multiple connection requests one after another

**Pre-conditions**:
- User `alice` exists and is logged in
- Users `bob`, `charlie`, `david` have all sent connection requests to `alice`
- Connections in `connections.dat`:
  - `bob|alice|pending`
  - `charlie|alice|pending`
  - `david|alice|pending`

**Test Steps**:
1. Login as `alice`
2. Navigate to "View My Pending Connection Requests"
3. System displays all 3 pending requests
4. Accept `bob`'s request (option 1)
5. Accept `charlie`'s request (option 1)
6. Accept `david`'s request (option 1)
7. View pending requests again - should show none
8. View network - should show all 3 connections

**Expected Results**:
- ✅ All requests show individual acceptance confirmations
- ✅ All statuses change to "connected" in `connections.dat`
- ✅ Pending requests list becomes empty
- ✅ Network view shows all 3 users
- ✅ Output file matches console output exactly

**Post-conditions**:
- `connections.dat` contains 3 "connected" entries
- No "pending" requests remain for `alice`

---

#### Test Case 1.3: Accept Request and Verify Bidirectional Visibility
**Objective**: Ensure both users can see the connection after acceptance

**Pre-conditions**:
- Users `alice` and `bob` exist
- `bob|alice|pending` exists in `connections.dat`

**Test Steps**:
1. Login as `alice`
2. Accept `bob`'s request
3. Logout and login as `bob`
4. View network
5. Verify `alice` appears in `bob`'s network
6. Logout and login as `alice`
7. View network
8. Verify `bob` appears in `alice`'s network

**Expected Results**:
- ✅ Both users see each other in their respective networks
- ✅ Connection is truly bidirectional
- ✅ Output file matches console output exactly

---

### 2. Rejecting Connection Requests

#### Test Case 2.1: Reject Single Request
**Objective**: Verify that a user can successfully reject a single pending connection request

**Pre-conditions**:
- User `alice` exists and is logged in
- User `bob` has sent a connection request to `alice`
- Connection exists: `bob|alice|pending`

**Test Steps**:
1. Login as `alice`
2. Navigate to "View My Pending Connection Requests"
3. System displays pending request from `bob`
4. Select option 2 (Reject) for `bob`'s request
5. Verify confirmation message: "Connection request from bob rejected!"

**Expected Results**:
- ✅ Request is removed from `connections.dat` or marked as "rejected"
- ✅ Request no longer appears in pending requests list
- ✅ `bob` does NOT appear in `alice`'s network
- ✅ Output file matches console output exactly

**Post-conditions**:
- `bob|alice` entry is removed or marked as "rejected"
- Network view does NOT show `bob`

---

#### Test Case 2.2: Reject Multiple Requests
**Objective**: Verify that a user can reject multiple connection requests

**Pre-conditions**:
- User `alice` exists
- Users `bob`, `charlie`, `david` have sent requests
- 3 pending connections exist in `connections.dat`

**Test Steps**:
1. Login as `alice`
2. Navigate to pending requests
3. Reject `bob`'s request (option 2)
4. Reject `charlie`'s request (option 2)
5. Reject `david`'s request (option 2)
6. View pending requests - should show none
7. View network - should show no connections

**Expected Results**:
- ✅ All requests show individual rejection confirmations
- ✅ All rejected requests removed from pending
- ✅ Network remains empty
- ✅ Output file matches console output exactly

**Post-conditions**:
- No pending or connected entries for these users
- Network shows 0 connections

---

#### Test Case 2.3: Verify Rejected Request Cannot Be Resent Immediately
**Objective**: Ensure proper handling of rejected requests

**Pre-conditions**:
- `alice` rejected `bob`'s request

**Test Steps**:
1. Login as `bob`
2. Attempt to send another request to `alice`
3. Verify system response

**Expected Results**:
- ✅ System should either allow new request OR show appropriate message
- ✅ Behavior is consistent and documented
- ✅ Output file matches console output exactly

---

### 3. Mixed Scenarios (Accept and Reject)

#### Test Case 3.1: Accept Some, Reject Others
**Objective**: Verify handling of mixed accept/reject actions

**Pre-conditions**:
- User `alice` has 5 pending requests from: `bob`, `charlie`, `david`, `emily`, `frank`

**Test Steps**:
1. Login as `alice`
2. View pending requests (should show 5)
3. Accept `bob`'s request
4. Reject `charlie`'s request
5. Accept `david`'s request
6. Reject `emily`'s request
7. Accept `frank`'s request
8. View network

**Expected Results**:
- ✅ 3 connections established (bob, david, frank)
- ✅ 2 requests rejected (charlie, emily)
- ✅ Network shows exactly 3 connections
- ✅ Pending requests list is empty
- ✅ Output file matches console output exactly

**Post-conditions**:
- `connections.dat` has 3 "connected" entries
- Network displays 3 users

---

#### Test Case 3.2: Sequential Accept/Reject Over Multiple Sessions
**Objective**: Test handling across multiple login sessions

**Test Steps**:
1. **Session 1**: Login as `alice`, accept 2 requests, logout
2. **Session 2**: Login as `alice`, reject 1 request, logout
3. **Session 3**: Login as `alice`, accept 1 more request, verify network

**Expected Results**:
- ✅ All actions persist across sessions
- ✅ Network accurately reflects all decisions
- ✅ Output file matches console output exactly

---

### 4. Network Display Testing

#### Test Case 4.1: View Network with No Connections
**Objective**: Verify proper display when user has no connections

**Pre-conditions**:
- User `alice` exists
- No accepted connections in `connections.dat`

**Test Steps**:
1. Login as `alice`
2. Select "View My Network" (option 5)
3. Observe output

**Expected Results**:
- ✅ System displays: "You have no established connections yet." or similar
- ✅ No connection entries shown
- ✅ Output file matches console output exactly

---

#### Test Case 4.2: View Network with One Connection
**Objective**: Verify proper display with a single connection

**Pre-conditions**:
- User `alice` has one accepted connection with `bob`
- `connections.dat` contains: `bob|alice|connected`

**Test Steps**:
1. Login as `alice`
2. Select "View My Network"
3. Verify display format

**Expected Results**:
- ✅ Displays "1. bob" or similar format
- ✅ Shows "Total connections: 1"
- ✅ Profile information may be shown (university, major) if implemented
- ✅ Output file matches console output exactly

---

#### Test Case 4.3: View Network with Multiple Connections
**Objective**: Verify proper display with multiple connections

**Pre-conditions**:
- User `alice` has 5 accepted connections

**Test Steps**:
1. Login as `alice`
2. Select "View My Network"
3. Verify all connections are listed

**Expected Results**:
- ✅ All 5 connections displayed in numbered list
- ✅ Shows "Total connections: 5"
- ✅ Each connection shows username (and possibly profile info)
- ✅ Output file matches console output exactly

---

#### Test Case 4.4: Verify Network Does NOT Show Rejected Requests
**Objective**: Ensure rejected requests don't appear in network

**Pre-conditions**:
- User `alice` rejected requests from `bob` and `charlie`

**Test Steps**:
1. Login as `alice`
2. View network
3. Verify `bob` and `charlie` are NOT shown

**Expected Results**:
- ✅ Rejected users do NOT appear
- ✅ Only accepted connections shown
- ✅ Output file matches console output exactly

---

#### Test Case 4.5: Verify Network Does NOT Show Pending Requests
**Objective**: Ensure pending requests don't appear in network view

**Pre-conditions**:
- User `alice` has 3 pending requests (not yet accepted/rejected)

**Test Steps**:
1. Login as `alice`
2. View network (not pending requests view)
3. Verify pending requests are NOT shown

**Expected Results**:
- ✅ Only "connected" status users appear in network
- ✅ Pending requests do NOT appear
- ✅ Output file matches console output exactly

---

### 5. Persistence Testing

#### Test Case 5.1: Accepted Connections Persist After Restart
**Objective**: Verify connections survive program restart

**Test Steps**:
1. Login as `alice`
2. Accept 3 connection requests
3. View network (verify 3 connections)
4. Exit program
5. Restart program
6. Login as `alice`
7. View network

**Expected Results**:
- ✅ All 3 connections still visible after restart
- ✅ No data loss
- ✅ `connections.dat` file intact
- ✅ Output file matches console output exactly

---

#### Test Case 5.2: Pending Requests Removal Persists
**Objective**: Verify that accepted/rejected requests don't reappear

**Test Steps**:
1. Login as `alice`
2. Accept 2 requests, reject 2 requests
3. View pending requests (should be empty if all processed)
4. Exit program
5. Restart program
6. Login as `alice`
7. View pending requests

**Expected Results**:
- ✅ Processed requests do NOT reappear as pending
- ✅ Only genuinely pending requests shown
- ✅ Persistence is correct
- ✅ Output file matches console output exactly

---

#### Test Case 5.3: Multi-User Persistence
**Objective**: Verify multiple users' connections persist independently

**Test Steps**:
1. Login as `alice`, accept connections, logout
2. Login as `bob`, accept connections, logout
3. Login as `charlie`, accept connections, logout
4. Restart program
5. Login as each user and verify their individual networks

**Expected Results**:
- ✅ Each user's network is independent and correct
- ✅ No cross-contamination of data
- ✅ All data persists correctly
- ✅ Output file matches console output exactly

---

## 📊 Test Execution Matrix

| Test ID | Category | Priority | Status | Tester | Bugs Found |
|---------|----------|----------|--------|--------|------------|
| 1.1 | Accept Single | HIGH | Pending | - | - |
| 1.2 | Accept Multiple | HIGH | Pending | - | - |
| 1.3 | Accept Bidirectional | MEDIUM | Pending | - | - |
| 2.1 | Reject Single | HIGH | Pending | - | - |
| 2.2 | Reject Multiple | HIGH | Pending | - | - |
| 2.3 | Reject Resend | LOW | Pending | - | - |
| 3.1 | Mixed Accept/Reject | HIGH | Pending | - | - |
| 3.2 | Multi-Session | MEDIUM | Pending | - | - |
| 4.1 | Network No Connections | MEDIUM | Pending | - | - |
| 4.2 | Network One Connection | MEDIUM | Pending | - | - |
| 4.3 | Network Multiple | HIGH | Pending | - | - |
| 4.4 | Network No Rejected | HIGH | Pending | - | - |
| 4.5 | Network No Pending | HIGH | Pending | - | - |
| 5.1 | Persistence Connections | HIGH | Pending | - | - |
| 5.2 | Persistence Removal | HIGH | Pending | - | - |
| 5.3 | Multi-User Persistence | MEDIUM | Pending | - | - |

---

## 🐛 Bug Reporting Template

When bugs are found, create a Jira ticket with the following information:

### Bug Template
```
Title: [Brief description of the issue]

Test Case ID: [e.g., 1.1, 2.2]

Priority: [Critical/High/Medium/Low]

Steps to Reproduce:
1. [Step 1]
2. [Step 2]
3. [Step 3]

Expected Result:
[What should happen]

Actual Result:
[What actually happened]

Environment:
- Branch: [branch name]
- Date: [date]
- Input File: [filename]
- Output File: [filename]

Attachments:
- InCollege-Input.txt
- InCollege-Output.txt
- connections.dat (before and after)

Additional Notes:
[Any other relevant information]
```

---

## 📝 Output Verification Procedure

For EVERY test case:

1. **Before Test**:
   - Clear or backup existing `InCollege-Output.txt`
   - Prepare `InCollege-Input.txt` with test data

2. **During Test**:
   - Observe console output in real-time
   - Note any discrepancies immediately

3. **After Test**:
   - Compare console output to `InCollege-Output.txt` line-by-line
   - Use diff tool: `diff console-capture.txt InCollege-Output.txt`
   - Verify they are **ABSOLUTELY IDENTICAL**

4. **Verification Checklist**:
   - [ ] Line breaks match
   - [ ] Whitespace matches
   - [ ] All prompts present in output file
   - [ ] All user inputs echoed (if applicable)
   - [ ] All confirmations present
   - [ ] All error messages captured
   - [ ] Menu displays match
   - [ ] Connection management messages captured

---

## 🛠️ Test Data Files Location

All test input files should be stored in:
- `tests/week5/accepting/` - Accept request test cases
- `tests/week5/rejecting/` - Reject request test cases
- `tests/week5/mixed/` - Mixed scenario test cases
- `tests/week5/network/` - Network display test cases
- `tests/week5/persistence/` - Persistence test cases

---

## ✅ Definition of Done

A test case is considered complete when:
1. ✅ Test has been executed at least once
2. ✅ Results documented in test matrix
3. ✅ Console and file output verified as identical
4. ✅ Any bugs found are reported in Jira
5. ✅ Test data files are saved
6. ✅ Actual results match expected results OR bug is filed

---

## 📅 Testing Schedule

- **Day 1**: Test Cases 1.1, 1.2, 2.1, 2.2 (Basic accept/reject)
- **Day 2**: Test Cases 3.1, 4.1-4.5 (Mixed scenarios & network display)
- **Day 3**: Test Cases 5.1-5.3 (Persistence testing)
- **Day 4**: Retest failed cases, verify bug fixes
- **Day 5**: Final regression testing

---

## 🎯 Success Criteria

Testing is successful when:
- ✅ All HIGH priority tests pass
- ✅ All bugs are documented in Jira
- ✅ Output verification shows 100% match between console and file
- ✅ Persistence tests confirm data integrity
- ✅ Network display accurately reflects connection states
- ✅ No data loss or corruption observed

---

**Document Version**: 1.0  
**Last Updated**: October 8, 2025  
**Prepared By**: Testing Team  
**Status**: Ready for Test Execution

