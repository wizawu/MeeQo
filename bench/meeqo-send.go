package main

import (
    "net"
    "time"
    "fmt"
    "strconv"
    "flag"
)

var msg [64]byte
var symb [4]byte = [4]byte{0, '\t', '\t'}
const JOBS int = 100000
var MSGL int

func fill(x int) {
    t := time.Now().UnixNano()
    for i := 0; i < x; i++ {
        msg[i] = byte(t & 15) + 'A'
        t >>= 1
    }
}

func check(err error) {
    if err != nil {
        panic(err)
    }
    return
}

func swrite(conn net.Conn, bytes []byte) {
    _, err := conn.Write(bytes)
    check(err)
}

func main() {
    flag.Parse()
    MSGL, err := strconv.Atoi(flag.Arg(0))
    check(err)
    fmt.Printf("Msg length: %d bytes\n", MSGL)

    conn, err := net.Dial("tcp", "127.0.0.1:6611")
    defer conn.Close()
    check(err)

    for i := 0; i < JOBS; i++ {
        fill(MSGL)
        swrite(conn, []byte("send 192.168.3.179:6611 [["))
        swrite(conn, []byte(strconv.Itoa(i+1)))
        swrite(conn, symb[1:3])
        swrite(conn, msg[:MSGL])
        swrite(conn, []byte("]]"))

        var ok [2]byte
        _, err := conn.Read(ok[:])
        check(err)
        if string(ok[:]) != "ok" {
            panic("Not ok")
        }
    }
    fmt.Printf("Sent %d messages.\n", JOBS)
}
