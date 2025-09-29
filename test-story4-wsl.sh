#!/bin/bash
# Story 4 Connection Request Testing Script for WSL

echo "====== Story 4: Connection Request Testing ======"
echo "Setting up test environment..."

# Copy test input
cp Story4-Test-Input.txt InCollege-Input.txt
echo "✓ Test input copied"

# Clean up any existing data files
rm -f users.dat profiles.dat connections.dat
echo "✓ Cleaned up existing data files"

# Compile the program
echo ""
echo "Compiling InCollege.cob..."
cobc -x -free InCollege.cob -o InCollege
if [ $? -eq 0 ]; then
    echo "✓ Compilation successful"
else
    echo "✗ Compilation failed"
    exit 1
fi

# Run the test
echo ""
echo "Running Story 4 test..."
./InCollege
echo "✓ Test execution complete"

# Display the output
echo ""
echo "====== Test Output ======"
cat InCollege-Output.txt

# Compare with expected output
echo ""
echo "====== Comparing with Expected Output ======"
diff Story4-Test-Output.txt InCollege-Output.txt
if [ $? -eq 0 ]; then
    echo ""
    echo "✅ TEST PASSED: Output matches expected results!"
else
    echo ""
    echo "❌ TEST FAILED: Output differs from expected results"
    echo ""
    echo "To see the differences in detail, run:"
    echo "diff -u Story4-Test-Output.txt InCollege-Output.txt"
fi

# Show created data files
echo ""
echo "====== Created Data Files ======"
echo "Users created:"
if [ -f users.dat ]; then
    cat users.dat
else
    echo "(none)"
fi

echo ""
echo "Profiles created:"
if [ -f profiles.dat ]; then
    cat profiles.dat | cut -d'|' -f1-5
else
    echo "(none)"
fi

echo ""
echo "Connection requests:"
if [ -f connections.dat ]; then
    cat connections.dat
else
    echo "(none)"
fi
