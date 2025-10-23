#!/usr/bin/env python3

import os
import sys
import json
import time
import signal
import subprocess
import threading
from typing import Dict, Any, Optional
from http.server import HTTPServer, BaseHTTPRequestHandler
from urllib.parse import urlparse, parse_qs
import urllib.request
import urllib.error


class SGLangManager:
    """Manages the SGLang inference server lifecycle."""

    def __init__(self, model_path: str = None, host: str = "127.0.0.1", port: int = 30000):
        self.model_path = model_path
        self.host = host
        self.port = port
        self.sglang_process = None
        self.base_url = f"http://{self.host}:{self.port}"

    def start_server(self) -> bool:
        """Start the SGLang server."""
        if not self.model_path:
            print("Error: No model path specified.")
            return False

        try:
            cmd = [
                "python3", "-m", "sglang.launch_server",
                "--model-path", self.model_path,
                "--host", self.host,
                "--port", str(self.port)
            ]

            print(f"Starting SGLang server with model: {self.model_path}")
            print(f"Command: {' '.join(cmd)}")

            # Start SGLang server in background
            self.sglang_process = subprocess.Popen(
                cmd,
                stdout=subprocess.PIPE,
                stderr=subprocess.PIPE,
                env=os.environ.copy()
            )

            # Wait for server to be ready
            return self._wait_for_ready()

        except Exception as e:
            print(f"Error starting SGLang server: {e}")
            return False

    def _wait_for_ready(self, timeout: int = 300) -> bool:
        """Wait for SGLang server to be ready."""
        start_time = time.time()

        while time.time() - start_time < timeout:
            try:
                # Check if process is still running
                if self.sglang_process.poll() is not None:
                    stdout, stderr = self.sglang_process.communicate()
                    print(f"SGLang process exited unexpectedly:")
                    print(f"STDOUT: {stdout.decode()}")
                    print(f"STDERR: {stderr.decode()}")
                    return False

                # Check if server is responding
                try:
                    response = urllib.request.urlopen(f"{self.base_url}/health", timeout=5)
                    if response.getcode() == 200:
                        print(f"SGLang server is ready at {self.base_url}")
                        return True
                except urllib.error.URLError:
                    pass

                time.sleep(2)

            except Exception as e:
                print(f"Error while waiting for SGLang server: {e}")
                time.sleep(2)

        print("Timeout waiting for SGLang server to be ready")
        return False

    def stop_server(self):
        """Stop the SGLang server."""
        if self.sglang_process:
            print("Stopping SGLang server...")
            self.sglang_process.terminate()
            try:
                self.sglang_process.wait(timeout=10)
            except subprocess.TimeoutExpired:
                self.sglang_process.kill()
                self.sglang_process.wait()
            print("SGLang server stopped")


class OpenAIProxyHandler(BaseHTTPRequestHandler):
    """HTTP request handler that proxies OpenAI API calls to SGLang."""

    sglang_manager: Optional[SGLangManager] = None

    def do_POST(self):
        """Handle POST requests - proxy to SGLang OpenAI-compatible endpoint."""
        print(f"Received POST request to: {self.path}")
        if (self.path.startswith("/v1/completions") or 
            self.path.startswith("/v1/chat/completions") or
            self.path == "/v1/responses"):
            self._proxy_to_sglang()
        else:
            print(f"Path {self.path} not matched, returning 404")
            self._send_error(404, "Not Found")

    def do_GET(self):
        """Handle GET requests."""
        if self.path == "/health":
            self._send_health_response()
        else:
            self._send_error(404, "Not Found")

    def _proxy_to_sglang(self):
        """Proxy request to SGLang server."""
        if not self.sglang_manager or not self.sglang_manager.sglang_process:
            self._send_error(503, "SGLang server not available")
            return

        try:
            # Read request body
            content_length = int(self.headers.get('Content-Length', 0))
            post_data = self.rfile.read(content_length)
            
            print(f"Proxying to SGLang: {self.path}")
            print(f"Request body: {post_data.decode('utf-8', errors='replace')}")

            # Prepare SGLang URL
            sglang_url = f"{self.sglang_manager.base_url}{self.path}"

            # Create request to SGLang
            req = urllib.request.Request(
                sglang_url,
                data=post_data,
                headers=dict(self.headers)
            )

            # Forward request to SGLang
            with urllib.request.urlopen(req) as response:
                # Send response back to client
                self.send_response(response.getcode())
                for header, value in response.headers.items():
                    if header.lower() not in ['connection', 'transfer-encoding']:
                        self.send_header(header, value)
                self.end_headers()

                # Stream response body
                while True:
                    chunk = response.read(8192)
                    if not chunk:
                        break
                    self.wfile.write(chunk)

        except urllib.error.HTTPError as e:
            # Forward SGLang's HTTP response (including error responses) transparently
            print(f"SGLang returned HTTP error: {e.code} - {e.reason}")
            self.send_response(e.code)

            # Copy headers from SGLang response
            for header, value in e.headers.items():
                if header.lower() not in ['connection', 'transfer-encoding']:
                    self.send_header(header, value)
            self.end_headers()

            # Copy response body
            response_body = e.read()
            self.wfile.write(response_body)

        except urllib.error.URLError as e:
            print(f"Network error connecting to SGLang: {e}")
            self._send_error(502, f"Bad Gateway - Cannot connect to SGLang server: {str(e)}")
        except Exception as e:
            print(f"Unexpected error: {e}")
            self._send_error(500, "Internal Server Error")

    def _send_health_response(self):
        """Send health check response."""
        sglang_status = "ready" if (self.sglang_manager and
                                   self.sglang_manager.sglang_process and
                                   self.sglang_manager.sglang_process.poll() is None) else "not_ready"

        response = {
            "status": "healthy",
            "sglang_server": sglang_status,
            "timestamp": time.time()
        }

        self.send_response(200)
        self.send_header('Content-Type', 'application/json')
        self.end_headers()
        self.wfile.write(json.dumps(response).encode())

    def _send_error(self, code: int, message: str):
        """Send error response."""
        self.send_response(code)
        self.send_header('Content-Type', 'application/json')
        self.end_headers()

        error_response = {
            "error": {
                "code": code,
                "message": message
            }
        }
        self.wfile.write(json.dumps(error_response).encode())

    def log_message(self, format, *args):
        """Override log_message to reduce verbosity."""
        pass  # Suppress default HTTP logging


class HyperBeamInferenceServer:
    """Main inference server that combines SGLang management with OpenAI API proxy."""

    def __init__(self,
                 model_path: str = None,
                 sglang_host: str = "127.0.0.1",
                 sglang_port: int = 30000,
                 proxy_host: str = "127.0.0.1",
                 proxy_port: int = 8080):
        self.sglang_manager = SGLangManager(model_path=model_path, host=sglang_host, port=sglang_port)
        self.proxy_host = proxy_host
        self.proxy_port = proxy_port
        self.http_server = None
        self.server_thread = None

        # Set the sglang_manager for the HTTP handler
        OpenAIProxyHandler.sglang_manager = self.sglang_manager

    def start(self) -> bool:
        """Start the inference server."""
        print("Starting HyperBEAM Inference Server...")

        # Start SGLang server
        if not self.sglang_manager.start_server():
            print("Failed to start SGLang server")
            return False

        # Start HTTP proxy server
        try:
            self.http_server = HTTPServer(
                (self.proxy_host, self.proxy_port),
                OpenAIProxyHandler
            )

            # Start HTTP server in background thread
            self.server_thread = threading.Thread(
                target=self.http_server.serve_forever,
                daemon=True
            )
            self.server_thread.start()

            print(f"HyperBEAM Inference Server started successfully")
            print(f"OpenAI-compatible API available at: http://{self.proxy_host}:{self.proxy_port}")
            print(f"SGLang server running at: {self.sglang_manager.base_url}")
            return True

        except Exception as e:
            print(f"Error starting HTTP server: {e}")
            self.sglang_manager.stop_server()
            return False

    def stop(self):
        """Stop the inference server."""
        print("Stopping HyperBEAM Inference Server...")

        if self.http_server:
            self.http_server.shutdown()
            self.http_server.server_close()
            print("HTTP proxy server stopped")

        self.sglang_manager.stop_server()
        print("HyperBEAM Inference Server stopped")

    def wait_forever(self):
        """Keep the server running."""
        try:
            while True:
                time.sleep(1)
        except KeyboardInterrupt:
            print("\nReceived interrupt signal")
            self.stop()


def signal_handler(signum, frame):
    """Handle shutdown signals."""
    print(f"\nReceived signal {signum}")
    if hasattr(signal_handler, 'server'):
        signal_handler.server.stop()
    sys.exit(0)


def main():
    """Main entry point."""
    # Parse command line arguments
    import argparse
    parser = argparse.ArgumentParser(description='HyperBEAM Inference Server')
    parser.add_argument('--model-path', type=str, help='Path to the model directory')
    parser.add_argument('--sglang-host', type=str, default='127.0.0.1', help='SGLang server host')
    parser.add_argument('--sglang-port', type=int, default=30000, help='SGLang server port')
    parser.add_argument('--proxy-host', type=str, default='127.0.0.1', help='Proxy server host')
    parser.add_argument('--proxy-port', type=int, default=8080, help='Proxy server port')
    args = parser.parse_args()

    # Set model path from argument or environment
    if args.model_path:
        os.environ["SGLANG_MODEL_PATH"] = args.model_path

    # Create and start server
    server = HyperBeamInferenceServer(
        model_path=args.model_path,
        sglang_host=args.sglang_host,
        sglang_port=args.sglang_port,
        proxy_host=args.proxy_host,
        proxy_port=args.proxy_port
    )

    # Set up signal handlers
    signal_handler.server = server
    signal.signal(signal.SIGINT, signal_handler)
    signal.signal(signal.SIGTERM, signal_handler)

    # Start server and wait
    if server.start():
        server.wait_forever()
    else:
        print("Failed to start server")
        sys.exit(1)


if __name__ == "__main__":
    main()