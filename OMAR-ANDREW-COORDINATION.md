# Omar & Andrew Test Coordination

## 📋 Overview

**Date**: October 8, 2025  
**Task**: Combine positive, negative, and edge case tests  
**Deliverable**: Combined input/output zip file  

---

## 📦 Omar's Contribution

### File: `Omar-Tests-InputOutput.zip`

**Contents**:
1. `InCollege-Input.txt` - Comprehensive test input
2. `InCollege-Output.txt` - Expected output

---

## 🧪 Test Coverage in Omar's Files

### InCollege-Input.txt (14 lines)

**What it tests**:

#### ✅ Positive Cases:
- Accept connection requests (bob, david)
- View network with connections
- Display connected users correctly
- Status updates work (pending → connected)

#### ✅ Negative Cases:
- Reject connection requests (charlie, emily)
- Rejected users NOT shown in network
- Status updates work (pending → rejected)

#### ✅ Edge Cases (Mixed):
- Mix of accept and reject in same session
- Viewing pending after all processed (empty list)
- Viewing network with mixed statuses
- Data persistence across operations

---

## 📊 Test Flow

### Phase 1: Login
```
Line 1-3: Login as alice
```

### Phase 2: View Pending & Process
```
Line 4: Navigate to My Network (4)
Line 5: View Pending Requests (2)
Line 6: Accept bob (1) ← POSITIVE
Line 7: Reject charlie (2) ← NEGATIVE
Line 8: Accept david (1) ← POSITIVE
Line 9: Reject emily (2) ← NEGATIVE
Line 10: View Pending again (2) ← EDGE: Should be empty
```

### Phase 3: View Network
```
Line 11: View My Connections (3) ← EDGE: Filter test
```

### Phase 4: Exit
```
Lines 12-14: Exit
```

---

## ✅ Expected Results

### InCollege-Output.txt Contains:

**Positive Test Evidence**:
```
Connection request from bob accepted! ✅
Connection request from david accepted! ✅
001. bob ✅
002. david ✅
Total connections: 002 ✅
```

**Negative Test Evidence**:
```
Connection request from charlie rejected! ✅
Connection request from emily rejected! ✅
(charlie NOT in network list) ✅
(emily NOT in network list) ✅
```

**Edge Case Evidence**:
```
No pending connection requests. ✅
(Shows empty pending list after processing) ✅
(Network filters correctly) ✅
(Mixed statuses handled correctly) ✅
```

---

## 📁 Files for Andrew

### Omar-Tests-InputOutput.zip
- `InCollege-Input.txt` (14 lines)
- `InCollege-Output.txt` (output from test run)

### Supporting Files (if needed):
- `TEST-SETUP-users.dat` (5 pre-configured users)
- `TEST-SETUP-connections.dat` (4 pending requests)

---

## 🔄 Coordination Steps

### 1. Share with Andrew:
- Send `Omar-Tests-InputOutput.zip`
- Share setup files (TEST-SETUP-*.dat)

### 2. Andrew to provide:
- His input/output test files
- His positive/negative/edge cases

### 3. Combine:
- Merge all test cases
- Create unified test documentation
- Verify no conflicts

### 4. Create final zip:
- Combined input files
- Combined output files
- Test documentation
- Setup instructions

---

## 📝 What's Tested

### Connection Management Features:
- ✅ Accept single request
- ✅ Accept multiple requests
- ✅ Reject single request
- ✅ Reject multiple requests
- ✅ Mixed accept/reject
- ✅ View pending requests
- ✅ View network connections
- ✅ Status updates
- ✅ Network filtering
- ✅ Data persistence

### Test Categories:
- ✅ Positive: Accept requests, view connections
- ✅ Negative: Reject requests, verify filtering
- ✅ Edge: Empty lists, mixed statuses, filtered display

---

## 🎯 Test Data Setup

### TEST-SETUP-users.dat
```
alice,AlicePass1!
bob,BobPass1!
charlie,CharlieP1!
david,DavidPass1!
emily,EmilyPass1!
```

### TEST-SETUP-connections.dat (Initial)
```
bob|alice|pending
charlie|alice|pending
david|alice|pending
emily|alice|pending
```

### connections.dat (After Test)
```
bob|alice|connected      ← Positive test
charlie|alice|rejected   ← Negative test
david|alice|connected    ← Positive test
emily|alice|rejected     ← Negative test
```

---

## ✅ Verification

### Test Results:
- ✅ 2 accepted (bob, david) - Positive cases
- ✅ 2 rejected (charlie, emily) - Negative cases
- ✅ Network shows 2 connections - Edge case filtering
- ✅ No pending requests remain - Edge case empty list
- ✅ Output file matches console - Verification

**Status**: All positive, negative, and edge cases covered ✅

---

## 📧 Ready to Coordinate

Files ready to share with Andrew:
- ✅ `Omar-Tests-InputOutput.zip`
- ✅ `TEST-SETUP-users.dat`
- ✅ `TEST-SETUP-connections.dat`
- ✅ This coordination document

Next step: Get Andrew's tests and combine them.

---

**Document Version**: 1.0  
**Date**: October 8, 2025  
**Status**: Ready for Andrew Coordination  
**Deliverable**: Omar-Tests-InputOutput.zip ✅
