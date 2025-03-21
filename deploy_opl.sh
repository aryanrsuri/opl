#!/bin/bash

# Configuration
OPL_DIR="$(pwd)"
DEST_DIR=~
BUILD_INTERVAL=60  # 1 minute in seconds

# Function to build and copy the binary
build_and_deploy() {
  echo "$(date): Building OPL..."
  cd "$OPL_DIR" && cargo build -r
  
  if [ $? -eq 0 ]; then
    echo "$(date): Build successful, copying to $DEST_DIR"
    cp "$OPL_DIR/target/release/opl" "$DEST_DIR"
    echo "$(date): Deployed OPL to $DEST_DIR"
  else
    echo "$(date): Build failed"
  fi
}

# Parse command line arguments
MODE="interval"  # Default mode
while getopts "wh" opt; do
  case $opt in
    w) MODE="watch" ;;
    h) 
      echo "Usage: $0 [-w] [-h]"
      echo "  -w  Watch for file changes instead of timed interval"
      echo "  -h  Show this help message"
      exit 0
      ;;
    \?) 
      echo "Invalid option: -$OPTARG" >&2
      exit 1
      ;;
  esac
done

# Check for inotify-tools if watch mode is selected
if [ "$MODE" = "watch" ]; then
  if ! command -v inotifywait &> /dev/null; then
    echo "Error: inotify-tools not found. Please install it with:"
    echo "  Debian/Ubuntu: sudo apt-get install inotify-tools"
    echo "  macOS with Homebrew: brew install fswatch"
    exit 1
  fi
fi

# Initial build
build_and_deploy

# Run in the selected mode
if [ "$MODE" = "watch" ]; then
  echo "Watching for file changes in $OPL_DIR..."
  
  if command -v inotifywait &> /dev/null; then
    # Linux with inotify-tools
    while true; do
      inotifywait -r -e modify,create,delete,move "$OPL_DIR/src"
      build_and_deploy
    done
  elif command -v fswatch &> /dev/null; then
    # macOS with fswatch
    fswatch -o "$OPL_DIR/src" | while read; do
      build_and_deploy
    done
  else
    echo "No file watching tool available"
    exit 1
  fi
else
  echo "Running in timed mode, building every $BUILD_INTERVAL seconds..."
  while true; do
    sleep $BUILD_INTERVAL
    build_and_deploy
  done
fi 