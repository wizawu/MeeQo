package main

import (
    "net"
    "time"
    "fmt"
    "strconv"
)

var msg [64]byte
var symb [4]byte = [4]byte{0, '\t', '\t'}
const (
    JOBS int = 100000
    MSGL int = 8
    HOST string = "192.168.3.171"
)

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
/* Send-Read protocol */
func main() {
    conn, err := net.Dial("tcp", HOST + ":6611")
    defer conn.Close()
    check(err)

    chn := make(chan int)
    go fetch(chn)

    for i := 0; i < JOBS; i++ {
        fill(MSGL)
        swrite(conn, []byte("send " + HOST + ":6613 [["))
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
    <-chn
}

/* Tweet protocol
func main() {
    conn, err := net.Dial("tcp", HOST + ":6611")
    defer conn.Close()
    check(err)

    chn := make(chan int)
    go fetch(chn)

    for i := 0; i < JOBS; i++ {
        fill(MSGL)
        swrite(conn, []byte("tweet " + HOST + ":6613 "))
        swrite(conn, []byte(strconv.Itoa(i+1)))
        swrite(conn, symb[1:3])
        swrite(conn, msg[:MSGL])
        swrite(conn, symb[0:1])
    }
    fmt.Printf("Tweeted %d messages.\n", JOBS)
    <-chn
}
*/
func fetch(chn chan int) {
    time.Sleep(time.Millisecond * 200)
    conn, err := net.Dial("tcp", HOST + ":6613")
    defer conn.Close()
    check(err)

    var buf [80]byte
    for i := 0; i < JOBS; {
        _, err = conn.Write([]byte("read " + HOST + ":6611"))
        check(err)
        n, err := conn.Read(buf[:])
        check(err)
        if n == 0 {
            time.Sleep(time.Millisecond)
        } else {
            fmt.Println(string(buf[:n]))
            i++
        }
    }
    fmt.Printf("Read %d messages.\n", JOBS)
    chn <- 88
}
