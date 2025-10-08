# Test Files Documentation - Week 5

## 📋 Three Separate Test Files

As requested, we now have **three separate test files** covering positive, negative, and edge cases.

---

## 1️⃣ POSITIVE TEST

### InCollege-Input-Positive.txt
**Purpose**: Test successful acceptance of ALL connection requests

**Test Flow**:
1. Login as alice
2. Navigate to My Network
3. View Pending Requests
4. **Accept bob** (line 6: 1)
5. **Accept charlie** (line 7: 1)
6. **Accept david** (line 8: 1)
7. **Accept emily** (line 9: 1)
8. View Pending again (should be empty)
9. View My Connections (should show 4 connections)
10. Exit

**Expected Result**:
- ✅ All 4 requests accepted
- ✅ connections.dat shows 4 "connected" entries
- ✅ Network displays 4 users
- ✅ Total connections: 004

**Output File**: `InCollege-Output-Positive.txt`

---

## 2️⃣ NEGATIVE TEST

### InCollege-Input-Negative.txt
**Purpose**: Test rejection of ALL connection requests

**Test Flow**:
1. Login as alice
2. Navigate to My Network
3. View Pending Requests
4. **Reject bob** (line 6: 2)
5. **Reject charlie** (line 7: 2)
6. **Reject david** (line 8: 2)
7. **Reject emily** (line 9: 2)
8. View Pending again (should be empty)
9. View My Connections (should be empty)
10. Exit

**Expected Result**:
- ✅ All 4 requests rejected
- ✅ connections.dat shows 4 "rejected" entries
- ✅ Network displays no users
- ✅ Message: "You have no established connections yet."

**Output File**: `InCollege-Output-Negative.txt`

---

## 3️⃣ EDGE CASE TEST

### InCollege-Input-Edge.txt
**Purpose**: Test edge cases and error handling

**Test Flow**:
1. Login as alice
2. Navigate to My Network
3. **View My Connections first** (empty network - edge case)
4. Go back to network menu
5. View Pending Requests
6. Go back
7. **Send request to nonexistent user** (edge case - should show error)
8. Go back
9. **Send request to self** (edge case - should show error)
10. Go back
11. View Pending again
12. Exit

**Expected Result**:
- ✅ Empty network displays correctly
- ✅ "User not found" error for nonexistent user
- ✅ "You cannot send a connection request to yourself" error
- ✅ Program handles errors gracefully
- ✅ No crashes

**Output File**: `InCollege-Output-Edge.txt`

---

## 📊 Test Coverage

### Positive Cases:
- ✅ Accept single request
- ✅ Accept multiple requests
- ✅ View connections with multiple users
- ✅ Status updates (pending → connected)

### Negative Cases:
- ✅ Reject single request
- ✅ Reject multiple requests
- ✅ Empty network after rejections
- ✅ Status updates (pending → rejected)

### Edge Cases:
- ✅ Empty network display
- ✅ Send request to nonexistent user
- ✅ Send request to self
- ✅ View pending with no requests
- ✅ Network filtering (rejected not shown)

---

## 🚀 How to Run Tests

### Test 1: Positive (All Accept)
```bash
# Setup
rm -f users.dat connections.dat profiles.dat InCollege-Output.txt
cp TEST-SETUP-users.dat users.dat
cp TEST-SETUP-connections.dat connections.dat

# Run
cp InCollege-Input-Positive.txt InCollege-Input.txt
cobc -x -free InCollege.cob -o InCollege
./InCollege

# Verify
cat connections.dat | grep "connected"
# Expected: 4 connected entries
```

### Test 2: Negative (All Reject)
```bash
# Setup
rm -f connections.dat InCollege-Output.txt
cp TEST-SETUP-connections.dat connections.dat

# Run
cp InCollege-Input-Negative.txt InCollege-Input.txt
./InCollege

# Verify
cat connections.dat | grep "rejected"
# Expected: 4 rejected entries
```

### Test 3: Edge Cases
```bash
# Setup
rm -f connections.dat InCollege-Output.txt
echo "bob|alice|pending" > connections.dat

# Run
cp InCollege-Input-Edge.txt InCollege-Input.txt
./InCollege

# Verify
cat InCollege-Output.txt | grep -i "user not found\|cannot send"
# Expected: Error messages displayed
```

---

## ✅ Expected Outputs

### InCollege-Output-Positive.txt
```
Connection request from bob accepted!
Connection request from charlie accepted!
Connection request from david accepted!
Connection request from emily accepted!
No pending connection requests.
001. bob
002. charlie
003. david
004. emily
Total connections: 004
```

### InCollege-Output-Negative.txt
```
Connection request from bob rejected!
Connection request from charlie rejected!
Connection request from david rejected!
Connection request from emily rejected!
No pending connection requests.
You have no established connections yet.
```

### InCollege-Output-Edge.txt
```
You have no established connections yet.
No pending connection requests.
User not found.
You cannot send a connection request to yourself.
```

---

## 📦 Files for Andrew

**Input Files**:
- InCollege-Input-Positive.txt
- InCollege-Input-Negative.txt
- InCollege-Input-Edge.txt

**Output Files**:
- InCollege-Output-Positive.txt
- InCollege-Output-Negative.txt
- InCollege-Output-Edge.txt

**Setup Files**:
- TEST-SETUP-users.dat
- TEST-SETUP-connections.dat

**Total**: 8 files to combine with Andrew's tests

---

**Document Version**: 1.0  
**Created**: October 8, 2025  
**Test Type**: Positive, Negative, Edge Cases  
**Status**: Ready for Coordination ✅
