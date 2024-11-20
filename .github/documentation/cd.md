# CD

## Table of Contents
- [Description](#description)
- [Variables](#variables)
- [Jobs](#jobs)
- [Credentials](#credentials)

### Description 
This workflow is triggered when a push is made to the `main` branch and is responsible for building and deploying AO/HyperBEAM to a confidential VM in GCP.

### Variables

The following variables are defined by the workflow:
- `GCP_PROJECT`: The GCP project to deploy the application to (hyperbeam-cd)
- `GCP_IMAGE_NAME`: The name of the Packer image that is built (hyperbeam-image)
- `GCP_INSTANCE_NAME`: The name of the GCP instance (hyperbeam)
- `GCP_ZONE`: The GCP zone to deploy to (us-central1-a)

### Jobs

The workflow consists of four main jobs:

1. **build**: 
   - Sets up the build environment with Erlang, Packer, and Rebar3
   - Builds and releases AO/HyperBEAM
   - Creates a Packer image with a unique name using timestamp and commit SHA
   - Tags the image with workflow run ID and commit SHA

2. **deploy**:
   - Creates a confidential AMD SEV-SNP VM using the built image
   - Configures the VM with secure boot, vTPM, and integrity monitoring

3. **test**:
   - Waits for deployment to complete
   - Runs tests (placeholder for actual test implementation)

4. **cleanup**:
   - Deletes the created VM instance
   - Cleans up old images, keeping only the last 5

### Credentials

The credentials are stored as a GitHub secret named `CD_SERVICE_ACCOUNT` containing GCP service account credentials.
They were created as follows:
```sh
$ gcloud iam service-accounts create hyperbeam-cd-gha \
    --description="Service account for the hyperbeam-cd project" \
    --display-name="hyperbeam-cd-gha"
```

and the `.json` credentials file was created as follows:
```sh
$ gcloud iam service-accounts keys create "hyperbeam-cd-gha.json" \
 --iam-account "hyperbeam-cd-gha@hyperbeam-cd.iam.gserviceaccount.com"
```

The workflow uses these credentials to authenticate with Google Cloud using the `google-github-actions/auth` action.