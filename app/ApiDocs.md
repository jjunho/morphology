# API Documentation

## API Endpoints

### GET /api/v1/health

- **Description:** Health check endpoint.
- **Response:**
  - **Status:** 200 OK
  - **Body:**

    ```json
    {"status": "ok"}
    ```

  - **Content-Type:** application/json
  - **Example:**

    ```bash
    curl -X GET http://localhost:3000/api/v1/health
    ```

### POST /api/v1/paradigm

- **Description:** Create a new paradigm.
- **Request:**
  - **Body:**

    ```json
    {"citation": "verb citation form"}
    ```

    - `citation`: (string, required) The base form of the verb.
  - **Content-Type:** application/json
  - **Example:**

    ```bash
    curl -X POST -H "Content-Type: application/json" -d '{"citation": "falar"}' http://localhost:3000/api/v1/paradigm
    ```

- **Response:**
  - **Success:**
    - **Status:** 200 OK
    - **Content-Type:** application/json
    - **Body:**

      ```json
      {"data": { "paradigm": { /* paradigm data */ } }, "message": "Paradigm generated", "status": "ok"}
      ```

  - **Error:**
    - **Status:** 422 Unprocessable Entity
    - **Content-Type:** application/json
    - **Body:**

      ```json
      {"message": "Invalid input: citation is required", "status": "error"}
      ```
