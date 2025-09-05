*Code used for first subtask*,

_Codeforces Contest_

T=int(input())
for i in range(T):
    x=int(input())
    if x<=10:
        print("YES")

    else:
        print("NO")

- code is made such that the output displays 'yes' if chef has a rank less than 10 
and 'no' if he has a rank greater than 10
- T=number of test cases and x is the rank of thre chef
- Loop used, for , if, else.

 *code used for second sub task*

_Insurance_

t=int(input())
for i in range(t):
    x,y=map(int,input().split())
    if y<=x:                #since company only provides x amount
        print (y)
    else:
        print(x)
- max amt rebatable= Xlacks if amt needed is y 
- if y is less than x, y amount will be rebated also if y is equal to x then x amt will be rebated
- but if y is greater than x only x will be rebated 

*code for third subtask*

_MineGold_

T= int(input())
for i in range (T):
    N,X,Y=map(int,input().split())
    s=N+1 #no of friends
    if (N+1)*Y>=X: #no of friends multiplied with individual capacity
        print ("YES")
    else:
        print ("NO")
- Each person has a capacity of Y amt of gold, maximum gold in the mine is x so the total amount they can carry is equal to n*y but since rohan is also included
 another variable s is assigned as the total number of people
- So the total capacity is (n+1)*y

*code for fourth subtask*

_Big Hotel_

T= int(input())
for i in range(T):
    X,Y=map(int,input().split())  #reads as strings and converts into integers
      #to find no of floors, dividing room number by 10 will give floor no
    f=(X+9)//10   #'x+9'used so that rooms which has 10 and its multiples as nos can be accurately calculated 
    #f calculates rm nos from x
    g=(Y+9)//10    #calculates rm no from floor y
    print(abs(f-g))   #clclats abs diff btw 2flrs and abs used like modulus func


_Remove card_

from collections import Counter

t= int(input()) #test cases
for i in range(t): 
    n=int(input())  #no of cards
    c=list(map(int,input().split()))  #cards
    b=Counter(c)   #count repeating cards
    m=max(b.values())   #takes most repeated
    z=n-m   #throws least repeated & z= no of cards thrown
    print(z)  


- Task is to remove the least repeated card
 
_escape false alarm_

t = int(input())   # test cases

for i in range(t):
    n, x = map(int, input().split())         # n = number of doors, x = step length
    d = list(map(int, input().split()))  
    
    p = False  


    for s in range(n):
        b = s + x  # b = end position
        t = 0
        c = True

        
        for i in range(n):
            if d[i] == 0:   # door is free
                t += 1
            else:  # door is blocked
                if s <= t < b:
                    t += 1
                else:
                    c = False
                    break

        if c:  
            p = True
            break

    if p:
        print("YES")
    else:
        print("NO")

-
