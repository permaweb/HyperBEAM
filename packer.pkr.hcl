packer {
  required_plugins {
    googlecompute = {
      version = ">= 1.1.6"
      source  = "github.com/hashicorp/googlecompute"
    }
  }
}

# Define required variables
variable "project_id" {
  type    = string
  default = "hyperbeam-cd"
}

variable "region" {
  type    = string
  default = "us-east1"
}

variable "zone" {
  type    = string
  default = "us-east1-c"
}

variable "image_name" {
  type    = string
  default = "hyperbeam-image"
}

# Source block to define GCP builder
source "googlecompute" "ubuntu" {
  project_id          = var.project_id
  source_image_family = "ubuntu-2204-lts"
  image_name          = var.image_name
  zone                = var.zone
  machine_type        = "n1-standard-1"
  ssh_username        = "packer"
}

# Define the build stage
build {
  sources = ["source.googlecompute.ubuntu"]

  # Add a provisioner to download and install go-tpm-tools
  provisioner "shell" {
    inline = [
      "sudo apt-get update -y",
      "sudo apt-get install -y wget tar",

      # Download the go-tpm-tools binary archive
      "wget https://github.com/google/go-tpm-tools/releases/download/v0.4.4/go-tpm-tools_Linux_x86_64.tar.gz -O /tmp/go-tpm-tools.tar.gz",

      # Extract the binary
      "tar -xzf /tmp/go-tpm-tools.tar.gz -C /tmp",

      # Move the gotpm binary to /usr/local/bin
      "sudo mv /tmp/gotpm /usr/local/bin/",

      # Clean up
      "rm -f /tmp/go-tpm-tools.tar.gz /tmp/LICENSE /tmp/README.md"
    ]
  }


  # Upload the pre-built release (with ERTS included) to the instance
  provisioner "file" {
    source      = "./_build/default/rel/ao"
    destination = "/tmp/hyperbeam"
  }

  provisioner "shell" {
    inline = [
      # Move the release to /opt with sudo
      "sudo mv /tmp/hyperbeam /opt/hyperbeam",
      "sudo chmod -R 755 /opt/hyperbeam",

      # Create a symlink to make it easier to run the app
      "sudo ln -s /opt/hyperbeam/bin/hyperbeam /usr/local/bin/hyperbeam",

      # (Optional) If you want to create a systemd service to manage the app
      "echo '[Unit]' | sudo tee /etc/systemd/system/hyperbeam.service",
      "echo 'Description=Permaweb Node' | sudo tee -a /etc/systemd/system/hyperbeam.service",
      "echo '[Service]' | sudo tee -a /etc/systemd/system/hyperbeam.service",
      "echo 'Type=simple' | sudo tee -a /etc/systemd/system/hyperbeam.service",
      "echo 'ExecStart=/opt/hyperbeam/bin/hyperbeam foreground' | sudo tee -a /etc/systemd/system/hyperbeam.service",
      "echo 'Restart=on-failure' | sudo tee -a /etc/systemd/system/hyperbeam.service",
      "echo '[Install]' | sudo tee -a /etc/systemd/system/hyperbeam.service",
      "echo 'WantedBy=multi-user.target' | sudo tee -a /etc/systemd/system/hyperbeam.service",

      # Enable and start the service
      "sudo systemctl enable hyperbeam",
      "sudo systemctl start hyperbeam"
    ]
  }

  # Disable ssh
  # provisioner "shell" {
  #  inline = [
  #    "sudo systemctl stop ssh",
  #    "sudo systemctl disable ssh"
  #  ]
  # }
}

