import threading
import time
import random

N = 5  # number of philosophers (and forks)
k = 3  # number of meals each philosopher needs to eat

class State:
    THINKING = 0
    HUNGRY = 1
    EATING = 2

def left(i):
    return (i - 1 + N) % N

def right(i):
    return (i + 1) % N

state = [State.THINKING] * N
meal_count = [0] * N  # array to track the number of meals each philosopher has eaten
critical_region_mtx = threading.Lock()
output_mtx = threading.Lock()

both_forks_available = [threading.Semaphore(0) for _ in range(N)]

def my_rand(min_val, max_val):
    return random.randint(min_val, max_val)

def test(i):
    if state[i] == State.HUNGRY and state[left(i)] != State.EATING and state[right(i)] != State.EATING:
        state[i] = State.EATING
        both_forks_available[i].release()

def think(i):
    duration = my_rand(400, 800) / 1000.0
    with output_mtx:
        print(f"{i} is thinking {duration * 1000:.0f}ms")
    time.sleep(duration)

def take_forks(i):
    with critical_region_mtx:
        state[i] = State.HUNGRY
        with output_mtx:
            print(f"\t\t\t{i} is HUNGRY")
        test(i)
    both_forks_available[i].acquire()

def eat(i):
    duration = my_rand(400, 800) / 1000.0
    with output_mtx:
        print(f"\t\t\t\t\t\t{i} is eating {duration * 1000:.0f}ms his {meal_count[i]+1}. meal")
    time.sleep(duration)
    meal_count[i] += 1

def put_forks(i):
    with critical_region_mtx:
        state[i] = State.THINKING
        test(left(i))
        test(right(i))

def philosopher(i):
    while meal_count[i] < k:
        think(i)
        take_forks(i)
        eat(i)
        put_forks(i)
    with output_mtx:
        print(f"\t\t\t\t\t\t\t\t\t\t\tPhilosopher {i} has finished eating {k} meals.")

if __name__ == "__main__":
    print("dp_14")

    threads = [threading.Thread(target=philosopher, args=(i,)) for i in range(N)]
    for t in threads:
        t.start()
    for t in threads:
        t.join()
    
    print("All philosophers have eaten their meals.")
