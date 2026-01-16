#!/bin/bash

# Navigate to the project root (one folder up from where this script is)
cd "$(dirname "$0")/.."

echo "--- STARTING AUTO-SYNC ---"

# 1. Check status
echo "Checking Git status..."
git status

# 2. Clean up duplicate folder if it exists
if [ -d "final project/final project" ]; then
    echo "Removing nested duplicate folder..."
    rm -rf "final project/final project"
fi

# 3. Add all files
echo "Staging files..."
git add .

# 4. Commit using the system date
echo "Committing..."
git commit -m "Auto-Update: Final Project Report and Artifacts $(date)"

# 5. Push
echo "Pushing to GitHub..."
git push

echo "--- DONE! ---"
echo "Your repository should be up to date."
