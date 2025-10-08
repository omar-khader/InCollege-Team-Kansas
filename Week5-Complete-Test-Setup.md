# Week 5 Complete Test - Single File Execution

## 📋 Overview
This is a comprehensive, single-execution test that validates ALL Week 5 connection management features including:
- Creating test users with profiles
- Sending connection requests
- Accepting some requests
- Rejecting some requests  
- Viewing pending requests
- Viewing network connections
- Persistence verification

---

## 🎯 Test Flow

### Phase 1: User Creation (Lines 1-75)
Creates 5 users with complete profiles:
1. **alice** (AlicePass1!) - Test coordinator, will receive and process requests
2. **bob** (BobPass1!) - Will send request to alice
3. **charlie** (CharlieP1!) - Will send request to alice
4. **david** (DavidPass1!) - Will send request to alice
5. **emily** (EmilyPass1!) - Will send request to alice

Each user gets a complete profile with:
- First/Last name
- University
- Major
- Graduation year
- About me section

### Phase 2: Connection Requests (Lines 76-102)
Each user (bob, charlie, david, emily) logs in and sends a connection request to alice:
- Bob → Alice: Connection request
- Charlie → Alice: Connection request
- David → Alice: Connection request
- Emily → Alice: Connection request

**Expected Result**: 4 pending requests in connections.dat

### Phase 3: Accept/Reject Testing (Lines 103-113)
Alice logs in and processes the pending requests:
1. Navigate to "View My Pending Connection Requests" (option 4)
2. **Accept** bob's request (option 1)
3. **Reject** charlie's request (option 2)
4. **Accept** david's request (option 1)
5. **Reject** emily's request (option 2)
6. View pending requests again (should be empty - option 4)
7. View network (should show 2 connections - option 5)

**Expected Results**:
- 2 accepted connections: bob, david
- 2 rejected requests: charlie, emily
- Network displays: bob and david
- Pending requests: empty

### Phase 4: Exit (Line 114)
Clean exit from program

---

## 🚀 How to Execute

### Step 1: Clean Environment
```bash
cd c:\Users\omark\Desktop\CEN4020-Project
rm -f users.dat profiles.dat connections.dat InCollege-Output.txt
```

### Step 2: Copy Input File
```bash
cp Week5-Complete-Test-Input.txt InCollege-Input.txt
```

### Step 3: Compile (if needed)
```bash
# If using Nicholas's implementation:
cobc -x -free StoryEight.cob -o InCollege

# If using integrated InCollege.cob:
cobc -x -free InCollege.cob -o InCollege
```

### Step 4: Execute
```bash
./InCollege
```

### Step 5: Verify Results
```bash
# Check output file
cat InCollege-Output.txt

# Verify users created
cat users.dat
# Expected: 5 users (alice, bob, charlie, david, emily)

# Verify profiles created
cat profiles.dat | wc -l
# Expected: 5 lines

# Verify connections
cat connections.dat
# Expected: 2 "connected" entries (bob|alice, david|alice)
#           0 "pending" entries
#           Rejected entries may be removed or marked "rejected"
```

---

## ✅ Expected Output Highlights

### User Creation
```
created: alice
created: bob
created: charlie
created: david
created: emily
```

### Profile Creation
```
Profile saved successfully!
(5 times)
```

### Connection Requests Sent
```
Connection request sent successfully!
(4 times - from bob, charlie, david, emily)
```

### Pending Requests View
```
--- Pending Connection Requests ---
Request from: bob
Request from: charlie
Request from: david
Request from: emily
Total pending requests: 4
```

### Accept/Reject Confirmations
```
Connection request from bob accepted!
Connection request from charlie rejected!
Connection request from david accepted!
Connection request from emily rejected!
```

### Final Pending Requests (Should be Empty)
```
--- Pending Connection Requests ---
No pending connection requests.

Total pending requests: 0
```

### Network View
```
--- My Connections ---
1. bob
2. david

Total connections: 2
-----------------------------------
```

---

## 📊 Validation Checklist

After execution, verify:

- [ ] **Users Created**: 5 users in users.dat
- [ ] **Profiles Created**: 5 profiles in profiles.dat
- [ ] **Requests Sent**: 4 connection requests sent successfully
- [ ] **Accepts Processed**: 2 requests accepted (bob, david)
- [ ] **Rejects Processed**: 2 requests rejected (charlie, emily)
- [ ] **Pending Empty**: No pending requests remain
- [ ] **Network Correct**: Network shows exactly 2 connections
- [ ] **No Rejected in Network**: charlie and emily NOT in network
- [ ] **Console = File**: InCollege-Output.txt matches console exactly
- [ ] **No Errors**: No error messages in output

---

## 🐛 Common Issues & Solutions

### Issue: "User not found" when sending requests
**Solution**: Ensure users are created before sending requests. Check users.dat

### Issue: Pending requests don't show up
**Solution**: Verify connections.dat has "pending" entries with correct format: `from|to|pending`

### Issue: Network shows wrong users
**Solution**: Check connections.dat - only "connected" status should appear in network

### Issue: Accepted requests still showing as pending
**Solution**: Verify accept logic changes status from "pending" to "connected"

### Issue: Output file doesn't match console
**Solution**: Ensure all display statements also write to OutFile via `say` procedure

---

## 🔄 Testing Persistence

After the main test completes:

### Test Restart Persistence
```bash
# Create new input to just view network
cat > InCollege-Input.txt << EOF
1
alice
AlicePass1!
5
0
EOF

# Run again
./InCollege

# Verify network still shows 2 connections
cat InCollege-Output.txt | grep "Total connections: 2"
```

---

## 📝 Line-by-Line Input Breakdown

```
Line 1-2:   Create alice account
Line 3-15:  Log in alice, create profile
Line 16:    Logout (option 0)

Line 17-18: Create bob account
Line 19-31: Log in bob, create profile
Line 32:    Logout

Line 33-34: Create charlie account
Line 35-47: Log in charlie, create profile
Line 48:    Logout

Line 49-50: Create david account
Line 51-63: Log in david, create profile
Line 64:    Logout

Line 65-66: Create emily account
Line 67-79: Log in emily, create profile
Line 80:    Logout

Line 81-82: Bob logs in
Line 83:    Select option 4 (View My Network)
Line 84:    Select option 1 (Send Connection Request)
Line 85:    Enter username: alice
Line 86:    Logout

Line 87-88: Charlie logs in
Line 89-91: Send request to alice, logout

Line 92-93: David logs in
Line 94-96: Send request to alice, logout

Line 97-98: Emily logs in
Line 99-101: Send request to alice, logout

Line 102-103: Alice logs in
Line 104:     View Pending Connection Requests (option 4)
Line 105:     Accept bob (option 1)
Line 106:     Reject charlie (option 2)
Line 107:     Accept david (option 1)
Line 108:     Reject emily (option 2)
Line 109:     View pending again (option 4)
Line 110:     View network (option 5)
Line 111:     Exit to main menu (option 3 or 0)
Line 112:     Exit program (option 3)
```

---

## 🎯 Success Criteria

Test is **PASSING** when:
✅ All 5 users created successfully  
✅ All 5 profiles saved  
✅ All 4 connection requests sent  
✅ 2 requests accepted (bob, david)  
✅ 2 requests rejected (charlie, emily)  
✅ Pending requests list becomes empty  
✅ Network shows exactly 2 connections  
✅ Network does NOT show rejected users  
✅ Output file matches console 100%  
✅ connections.dat has 2 "connected" entries  
✅ Data persists after program restart  

---

## 📞 Support

If test fails:
1. Check InCollege-Output.txt for error messages
2. Verify connections.dat format
3. Ensure accept/reject logic updates status correctly
4. Create bug report using template in WEEK5-TESTING-PLAN.md
5. Attach all artifacts (input, output, data files)

---

**Document Version**: 1.0  
**Last Updated**: October 8, 2025  
**Test Type**: Comprehensive Integration Test  
**Status**: Ready for Execution

