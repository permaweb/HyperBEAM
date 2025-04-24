.PHONY: all clean rebuild install test

# Build directory (can be overridden)
BUILD_DIR ?= ./_build

# Additional CMake arguments
CMAKE_ARGS ?=

# Default target
all: $(BUILD_DIR)
	@echo "Configuring CMake project..."
	@cd $(BUILD_DIR) && cmake .. $(CMAKE_ARGS)
	@echo "Building project..."
	@cd $(BUILD_DIR) && $(MAKE)

# Create build directory if it doesn't exist
$(BUILD_DIR):
	@echo "Creating build directory..."
	@mkdir -p $(BUILD_DIR)

# Clean build artifacts
clean:
	@echo "Cleaning build directory..."
	@rm -rf $(BUILD_DIR)

# Rebuild from scratch
rebuild: clean all

# Install
install: all
	@echo "Install complete."

# Run tests
test:
	@echo "Running tests..."
	@cd $(BUILD_DIR) && $(MAKE) run_tests 