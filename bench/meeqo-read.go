package main

import (
    "net"
    "time"
    "fmt"
)

const JOBS int = 100000

func check(err error) {
    if err != nil {
        panic(err)
    }
    return
}

func main() {
    conn, err := net.Dial("tcp", "127.0.0.1:6611")
    defer conn.Close()
    check(err)

    var buf [80]byte
    for i := 0; i < JOBS; {
        _, err = conn.Write([]byte("read"))
        check(err)
        n, err := conn.Read(buf[:])
        check(err)
        if n == 0 {
            time.Sleep(time.Millisecond)
        } else {
            //fmt.Println(string(buf[:n]))
            i++
        }
    }
    fmt.Printf("Read %d messages.\n", JOBS)
}
