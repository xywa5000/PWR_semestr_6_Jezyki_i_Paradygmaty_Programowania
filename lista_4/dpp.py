import threading
import time
import random

class Philosopher(threading.Thread):
    def __init__(self, id, left_fork, right_fork, max_meals):
        threading.Thread.__init__(self)
        self.id = id
        self.left_fork = left_fork
        self.right_fork = right_fork
        self.max_meals = max_meals
        self.meals_eaten = 0

    def run(self):
        while self.meals_eaten < self.max_meals:
            print(f'Philosopher {self.id} is thinking.')
            time.sleep(random.uniform(1, 3))  # Philosophers think for a while

            # Pick up the forks by hierarchy order
            first_fork, second_fork = (self.left_fork, self.right_fork) if self.left_fork.id < self.right_fork.id else (self.right_fork, self.left_fork)
            
            print(f'Philosopher {self.id} is hungry and tries to pick up fork {first_fork.id}.')
            with first_fork.lock:
                print(f'Philosopher {self.id} picked up fork {first_fork.id}.')
                print(f'Philosopher {self.id} tries to pick up fork {second_fork.id}.')
                with second_fork.lock:
                    print(f'Philosopher {self.id} picked up fork {second_fork.id}.')
                    print(f'Philosopher {self.id} is eating.')
                    time.sleep(random.uniform(1, 2))  # Philosophers eat for a while
                    self.meals_eaten += 1
                    print(f'Philosopher {self.id} has finished eating. Meals eaten: {self.meals_eaten}/{self.max_meals}.')

            # Forks are released automatically when exiting the `with` block
            print(f'Philosopher {self.id} put down forks {first_fork.id} and {second_fork.id}.')

        print(f'Philosopher {self.id} has left the table after eating {self.meals_eaten} meals.')

class Fork:
    def __init__(self, id):
        self.id = id
        self.lock = threading.Lock()

def main():
    num_philosophers = 30
    max_meals = 30  # Set the maximum number of meals each philosopher should eat
    forks = [Fork(i) for i in range(num_philosophers)]
    philosophers = [Philosopher(i, forks[i], forks[(i + 1) % num_philosophers], max_meals) for i in range(num_philosophers)]

    for philosopher in philosophers:
        philosopher.start()

    for philosopher in philosophers:
        philosopher.join()

if __name__ == "__main__":
    main()
