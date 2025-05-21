# Tests

## Table of Contents
- [Description](#description)
- [Jobs](#jobs)
- [Credentials](#credentials)

### Description 
This workflow is triggered when a pull request is opened on the HyperBEAM main branch. Or manually via the Github Actions UI. It is responsible for installing and running the HyperBEAM rebar3 eunit tests.

### Jobs

The workflow consists of 1 main job:

1. **test**: 
    - Utilizes actions/checkout@v4 and erlef/setup-beam@v1 to create the Erlang ready environment in the github test runner.
    - Iterates over each erlang module in Hyperbeam, running `rebar3 eunit --module modulename` on each, aggregating the results and successes.
    - Generates and sends a Slack message if any tests failed, containing number of successes, and each module that failed.

### Credentials

There are 2 credentials used by this action, both stored in Github secrets. Settings -> Secrets and variables -> Actions. 

`SLACK_BOT_TOKEN` and `SLACK_CHANNEL_ID`

Generate the token in Slack, and grab the channel ID you want to send to, add those to the action secrets in Github, and the action will work.