#!/bin/bash
# remove_summaries.sh

echo "Running summary removal script..."
cd "$1"
echo "Working in directory: $(pwd)"
echo "Looking for summary sections in ./doc"

# JavaScript code to remove <li> elements with <a> containing #summary in href
JS_CODE=$(cat <<'EOF'
<script>
  document.addEventListener('DOMContentLoaded', () => {
    document.querySelectorAll('a[href$="#summary"]').forEach(link => {
      const li = link.closest('li');
      if (li) {
        li.remove();
      }
    });
  });
</script>
EOF
)

# Process each file
for file in $FILES; do
  echo "Processing $file"
  # Remove sections with id="summary"
  sed -i '' '/<section[^>]*id="summary"[^>]*>/,/\/section>/d' "$file"
  # Append JavaScript code to the end of the file
  echo "$JS_CODE" >> "$file"
done

echo "Summary removal complete."
