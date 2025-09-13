package main

import (
    "fmt"
    "os"
    "strings"
    "strconv"
)

func pygo(n int) {
    for i := 1; i < n; i++ {
        fmt.Print(strings.Repeat(" ", n-i))
        if i == 1 {
            fmt.Println("*")
        } else {
            fmt.Println("*" + strings.Repeat(" ", 2*i-3) + "*")
        }
    }
    fmt.Println(strings.Repeat("*", 2*n-1))
}

func main() {
    data, _ := os.ReadFile("input.txt")
    n, _ := strconv.Atoi(strings.TrimSpace(string(data)))
    pygo(n)
}
