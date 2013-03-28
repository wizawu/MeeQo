package main

import (
    "./beanstalk"    /* git://github.com/kr/beanstalk.git */
    "time"
    "fmt"
)

var msg [64]byte
const (
    JOBS int = 100000
    MSGL int = 8
)

func fill(x int) {
    t := time.Now().UnixNano()
    for i := 0; i < x; i++ {
        msg[i] = byte(t & 15) + 'A'
        t >>= 1
    }
}

func main() {
    c, err := beanstalk.Dial("tcp", "127.0.0.1:5511")
    if err != nil {
        panic(err)
    }

    chn := make(chan int)
    go fetch(chn)

    for i := 0; i < JOBS; i++ {
        fill(MSGL)
        c.Put(msg[:MSGL], 1, 0, time.Hour)
    }
    fmt.Printf("Put %d messages.\n", JOBS)
    <-chn
}

func fetch(chn chan int) {
    c, err := beanstalk.Dial("tcp", "127.0.0.1:5511")
    if err != nil {
        panic(err)
    }
    for i := 0; i < JOBS; {
        id, body, err := c.Reserve(0)
        if err != nil {
            time.Sleep(time.Millisecond)
            continue
        }
        if len(body) != MSGL || true {
            fmt.Println(id, "\t\t", string(body))
        }
        i++
    }
    fmt.Printf("Reserved %d messages.\n", JOBS)
    chn <- 88
}
