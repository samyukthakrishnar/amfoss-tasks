STAR

PYTHON
------

def pyramid(n=5):
    for i in range(1, n):                 
        print(" " * (n - i), end="")      
        if i == 1:                        
            print("*")                    
        else:                             
            print("*" + " " * (2*i - 3) + "*")  
    print("*" * (2*n - 1))                

C
--

#include <stdio.h>

void pyramid(int n) {
    for (int i = 1; i < n; i++) {
        for (int j = 0; j < n - i; j++) printf(" ");
        if (i == 1) {
            printf("*\n");
        } else {
            printf("*");
            for (int j = 0; j < 2*i - 3; j++) printf(" ");
            printf("*\n");
        }
    }
    for (int j = 0; j < 2*n - 1; j++) printf("*");
    printf("\n");
}

int main() {
    pyramid(5);
    return 0;
}


C++
----

#include <iostream>
using namespace std;

void pyramid(int n) {
    for (int i = 1; i < n; i++) {
        cout << string(n - i, ' ');
        if (i == 1) {
            cout << "*" << endl;
        } else {
            cout << "*" << string(2*i - 3, ' ') << "*" << endl;
        }
    }
    cout << string(2*n - 1, '*') << endl;
}

int main() {
    pyramid(5);
    return 0;
}

JAVA
----

public class Pyramid {
    public static void pyramid(int n) {
        for (int i = 1; i < n; i++) {
            System.out.print(" ".repeat(n - i));
            if (i == 1) {
                System.out.println("*");
            } else {
                System.out.println("*" + " ".repeat(2*i - 3) + "*");
            }
        }
        System.out.println("*".repeat(2*n - 1));
    }

    public static void main(String[] args) {
        pyramid(5);
    }
}


JAVASCRIPT
----------

function pyramid(n = 5) {
    for (let i = 1; i < n; i++) {
        process.stdout.write(" ".repeat(n - i));
        if (i === 1) {
            console.log("*");
        } else {
            console.log("*" + " ".repeat(2*i - 3) + "*");
        }
    }
    console.log("*".repeat(2*n - 1));
}

pyramid(5);



GO
---

package main

import (
    "fmt"
    "strings"
)

func pyramid(n int) {
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
    pyramid(5)
}



RUST
----

fn pyramid(n: usize) {
    for i in 1..n {
        print!("{}", " ".repeat(n - i));
        if i == 1 {
            println!("*");
        } else {
            println!("*{}*", " ".repeat(2*i - 3));
        }
    }
    println!("{}", "*".repeat(2*n - 1));
}

fn main() {
    pyramid(5);
}

HASKEL
------

pyramid :: Int -> IO ()
pyramid n = do
    mapM_ putStrLn [row i | i <- [1..n-1]]
    putStrLn (replicate (2*n - 1) '*')
  where
    row 1 = replicate (n-1) ' ' ++ "*"
    row i = replicate (n-i) ' ' ++ "*" ++ replicate (2*i - 3) ' ' ++ "*"

main :: IO ()
main = pyramid 5


RUBY
-----


def pyramid(n = 5)
  (1...n).each do |i|
    print " " * (n - i)
    if i == 1
      puts "*"
    else
      puts "*" + " " * (2*i - 3) + "*"
    end
  end
  puts "*" * (2*n - 1)
end

pyramid(5)



ELIXIR
------


defmodule Pyramid do
  def draw(n) do
    for i <- 1..(n-1) do
      if i == 1 do
        IO.puts String.duplicate(" ", n - i) <> "*"
      else
        IO.puts String.duplicate(" ", n - i) <> "*" <> String.duplicate(" ", 2*i - 3) <> "*"
      end
    end
    IO.puts String.duplicate("*", 2*n - 1)
  end
end

Pyramid.draw(5)
