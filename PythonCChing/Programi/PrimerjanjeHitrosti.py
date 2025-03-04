from time import perf_counter
import msvcrt

def zvezdice(st):
    print("Hello World!")

msvcrt.getch()

startTime = perf_counter()
zvezdice(50)
print(f"Proces koncan --- {perf_counter() - startTime} sekund ---")
