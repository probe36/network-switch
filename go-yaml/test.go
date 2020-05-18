package main

import (
    "fmt"
    "net/http"
)

func main() {
    // Listen to the root path of the web app
    http.HandleFunc("/", handler)
    
    // Start a web server.
    http.ListenAndServe(":8080", nil)
}

// The handler for the root path.
func handler(writer http.ResponseWriter, request *http.Request) {
    fmt.Fprintf(writer, "Hello, World")
}
