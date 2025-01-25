# Stage 1: Build the application
FROM haskell:9.10.1-slim-bullseye AS builder

# Install necessary tools
RUN apt-get update && apt-get install -y pkg-config

# Set the working directory
WORKDIR /app

# Copy files required for dependency resolution first
COPY stack.yaml stack.yaml
COPY stack.yaml.lock stack.yaml.lock
COPY package.yaml package.yaml

# Install stack dependencies
RUN stack setup
RUN stack build --only-dependencies

# Copy the rest of the application files
COPY . .

# Build the application
RUN stack build

# Extract the built binary
# RUN stack path --local-install-root && ls -R /app/.stack-work
RUN export INSTALL_DIR=$(stack path --local-install-root) && \
    cp "$INSTALL_DIR/bin/morphology-exe" /app/morphology-exe

# Stage 2: Create a minimal runtime image
FROM debian:bullseye
WORKDIR /app

# Copy the compiled binary from the builder stage
COPY --from=builder /app/morphology-exe ./morphology-exe

# Expose the application port
EXPOSE 3000

# Run the application
CMD ["./morphology-exe"]
