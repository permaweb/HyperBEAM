# Load Test 

## Information
This folder contains some utilities to run load test against HyperBEAM.

Make sure you have K6 installed. In MacOS use `brew install k6`.

Modify `loadtest.js` to match what you need. By default uses sequential, non-repetitive, valid (200 OK) transaction ID.
Some transaction ID might return 404, and a further clean up might be needed.
By default, the load test is done using 20 users and duration of 3 minutes.

## Run
To run the load test:

```bash
k6 run loadtest.js
```

