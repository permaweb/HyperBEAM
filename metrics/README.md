# Metrics

This is a dashboard for Grafana, that connects to the prometheus database.
Metrics are fetched every 10 seconds.

## How to use

Start the services using:

```bash 
docker-compose up -d
```

And access the webpage `http://localhost:3000` with `admin` for both user and password.

The hyperbuddy dashboard is in `http://localhost:3000/d/hyperbuddy/hyperbuddy-dashboard?orgId=1&from=now-1h&to=now&timezone=browser&refresh=10s`

