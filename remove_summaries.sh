#!/bin/bash
# remove_summaries.sh

echo "Running summary removal script..."
cd "$1"
echo "Working in directory: $(pwd)"
echo "Looking for summary sections in ./doc"

FILES=$(find ./doc -type f -exec grep -l '<section.*id="summary"' {} \; 2>/dev/null)

# Process each file
for file in $FILES; do
  echo "Processing $file"
  # Remove sections with id="summary"
  sed -i '' '/<section[^>]*id="summary"[^>]*>/,/\/section>/d' "$file"
done

echo "Summary removal complete."
