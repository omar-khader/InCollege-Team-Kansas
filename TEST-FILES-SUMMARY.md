# Week 5 Test Files - Ready for Andrew Coordination

## ✅ Three Separate Test Sets Created

**Date**: October 8, 2025  
**Status**: Ready for Combination with Andrew's Tests  

---

## 📦 Omar's Test Files (6 Files)

### Input Files:
1. **InCollege-Input-Positive.txt** (14 lines)
2. **InCollege-Input-Negative.txt** (14 lines)
3. **InCollege-Input-Edge.txt** (18 lines)

### Output Files:
1. **InCollege-Output-Positive.txt** (85 lines)
2. **InCollege-Output-Negative.txt** (80 lines)
3. **InCollege-Output-Edge.txt** (86 lines)

---

## 🧪 Test Coverage

### 1️⃣ POSITIVE TEST (Accept All)
**File**: InCollege-Input-Positive.txt  
**Tests**: Accepting connection requests  

**Actions**:
- Accept bob's request ✅
- Accept charlie's request ✅
- Accept david's request ✅
- Accept emily's request ✅

**Expected Output**:
```
Connection request from bob accepted!
Connection request from charlie accepted!
Connection request from david accepted!
Connection request from emily accepted!
Total connections: 004
```

**Verification**: ✅ All 4 users connected

---

### 2️⃣ NEGATIVE TEST (Reject All)
**File**: InCollege-Input-Negative.txt  
**Tests**: Rejecting connection requests  

**Actions**:
- Reject bob's request ✅
- Reject charlie's request ✅
- Reject david's request ✅
- Reject emily's request ✅

**Expected Output**:
```
Connection request from bob rejected!
Connection request from charlie rejected!
Connection request from david rejected!
Connection request from emily rejected!
You have no established connections yet.
```

**Verification**: ✅ All 4 users rejected, network empty

---

### 3️⃣ EDGE CASE TEST
**File**: InCollege-Input-Edge.txt  
**Tests**: Error handling and boundary conditions  

**Actions**:
- View empty network ✅
- Send request to nonexistent user ✅
- Send request to self ✅
- View pending requests ✅

**Expected Output**:
```
You have no established connections yet.
User not found.
You cannot send a connection request to yourself.
No pending connection requests.
```

**Verification**: ✅ All edge cases handled correctly

---

## 📊 Test Results

| Test Type | Input File | Output File | Status |
|-----------|------------|-------------|--------|
| Positive | InCollege-Input-Positive.txt | InCollege-Output-Positive.txt | ✅ PASS |
| Negative | InCollege-Input-Negative.txt | InCollege-Output-Negative.txt | ✅ PASS |
| Edge | InCollege-Input-Edge.txt | InCollege-Output-Edge.txt | ✅ PASS |

**Overall**: 3/3 tests passing (100%) ✅

---

## 📁 Additional Setup Files

- `TEST-SETUP-users.dat` (5 pre-configured users)
- `TEST-SETUP-connections.dat` (4 pending requests)

---

## 🤝 For Andrew Coordination

### What to Share:
**Omar's Files (8 total)**:
1. InCollege-Input-Positive.txt
2. InCollege-Input-Negative.txt
3. InCollege-Input-Edge.txt
4. InCollege-Output-Positive.txt
5. InCollege-Output-Negative.txt
6. InCollege-Output-Edge.txt
7. TEST-SETUP-users.dat
8. TEST-SETUP-connections.dat

### What We Need from Andrew:
- His InCollege-Input-Positive.txt
- His InCollege-Input-Negative.txt
- His InCollege-Input-Edge.txt
- His corresponding Output files
- His test setup files (if different)

### Combined Deliverable:
- Create zip file with both Omar's and Andrew's tests
- Document all test cases
- Ensure no conflicts or duplicates

---

## 🚀 Deployed To GitHub

```
✅ origin/main (42594fb)
✅ origin/omar-dev (42594fb)
```

All test files now on GitHub for Andrew to access and combine!

---

**Document Version**: 1.0  
**Status**: Ready for Andrew Coordination  
**Files on GitHub**: 6 test files + 2 setup files  
**Next Step**: Combine with Andrew's tests and create final zip ✅
