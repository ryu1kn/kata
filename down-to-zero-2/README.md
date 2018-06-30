
# Down to Zero II

https://www.hackerrank.com/challenges/down-to-zero-ii/problem

> You are given `Q` queries. Each query consists of a single number `N`. You can perform any of the 2 operations on `N` in each move:
>
> 1. If we take 2 integers `a` and `b` where `N = a * b (a != 1, b != 1)`, then we can change `N = max(a, b)`
> 1. Decrease the value of `N` by `1`.
>
> Determine the minimum number of moves required to reduce the value of `N` to `0`.
>
> ## Input Format
>
> The first line contains the integer `Q`.
> The next `Q` lines each contain an integer, `N`.
>
> ## Constraints
>
> * `1 <= Q <= 10^3`
> * `0 <= N <= 10^6`
>
> ## Output Format
>
> Output `Q` lines. Each line containing the minimum number of moves required to reduce the value of `N` to `0`.
>
> ## Sample Input
>
> ```
> 2
> 3
> 4
> ```
>
> ## Sample Output
>
> ```
> 3
> 3
> ```
>
> ## Explanation
>
> For test case 1, We only have one option that gives the minimum number of moves.
> Follow `3 -> 2 -> 1 -> 0`. Hence, 3 moves.
>
> For the case 2, we can either go `4 -> 3 -> 2 -> 1 -> 0` or `4 -> 2 -> 1 -> 0`. The 2nd option is more optimal. Hence, 3 moves.
