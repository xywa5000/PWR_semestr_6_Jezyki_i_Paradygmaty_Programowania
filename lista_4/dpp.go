package main

import (
	"fmt"
	"math/rand"
	"sync"
	"time"
)

type Philosopher struct {
	number    int
	chopstick chan bool
	neighbor  *Philosopher
}

func makePhilosopher(number int, neighbor *Philosopher) *Philosopher {
	phil := &Philosopher{number, make(chan bool, 1), neighbor}
	phil.chopstick <- true
	return phil
}

func (phil *Philosopher) think() {
	fmt.Printf("%d is thinking\n", phil.number)
	time.Sleep(time.Duration(rand.Int63n(1e9)))
}

func (phil *Philosopher) eat(mealNum int, wg *sync.WaitGroup) {
	fmt.Printf("\t\t\t\t\t\t%d is eating his %d. meal\n", phil.number, mealNum)
	time.Sleep(time.Duration(rand.Int63n(1e9)))
	wg.Done()
}

func (phil *Philosopher) getChopsticks() {
	timeout := make(chan bool, 1)
	go func() { time.Sleep(1e9); timeout <- true }()
	<-phil.chopstick
	fmt.Printf("\t\t\t%d is HUNGRY\n", phil.number)
	select {
	case <-phil.neighbor.chopstick:
		//fmt.Printf("\t\t\t\t\t\t%d is eating\n", phil.number)
		//fmt.Printf("\t\t\t\t\t\t%d is eating his 1. meal\n", phil.number)
		return
	case <-timeout:
		phil.chopstick <- true
		phil.think()
		phil.getChopsticks()
	}
}

func (phil *Philosopher) returnChopsticks() {
	phil.chopstick <- true
	phil.neighbor.chopstick <- true
}

func (phil *Philosopher) dine(announce chan *Philosopher, k int) {
	var wg sync.WaitGroup
	for j := 0; j < k; j++ {
		phil.think()
		phil.getChopsticks()
		wg.Add(1)
		phil.eat(j+1, &wg)
		phil.returnChopsticks()
	}
	wg.Wait()
	announce <- phil
}

func main() {
	n := 5 // liczba filozofów
	k := 3 // liczba posiłków dla każdego filozofa
	philosophers := make([]*Philosopher, n)
	var phil *Philosopher
	for i := 0; i < n; i++ {
		phil = makePhilosopher(i, phil)
		philosophers[i] = phil
	}
	philosophers[0].neighbor = phil
	fmt.Printf("There are %d philosophers sitting at a table.\n", n)
	fmt.Printf("Each philosopher will eat %d meals.\n", k)
	announce := make(chan *Philosopher)
	for _, phil := range philosophers {
		go phil.dine(announce, k)
	}
	for i := 0; i < n; i++ {
		phil := <-announce
		fmt.Printf("\t\t\t\t\t\t\t\t\t\t\tPhilosopher %d has finished eating %d meals.\n", phil.number, k)
	}
	fmt.Printf("All philosophers have eaten their meals.\n")
}
