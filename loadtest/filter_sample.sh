#!/bin/bash
## Utility to filter transaction ID that only have 200 OK HTTP responses from log, and remove duplicates
awk '$10 == 200' fresh_sample.log | grep -oE '/[A-Za-z0-9_-]{43}' | sed 's|/||' | sort | uniq -d >>only_unique_200_tx.log
