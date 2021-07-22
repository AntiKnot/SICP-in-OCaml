open Base
open Stdio

let tolerance = 0.0001

let average a b =
  (a +. b) /. 2.0

let rec log b n = 
  let m = n/b in
  if m > 1 then 
    1 + (log b m)
  else 
    if m < 1 then 
      0 
    else 1
let log2 n = 
  log 2 n

let rec power b n =
  if n = 1 then b 
  else b *. (power b (n-1))

let compose f g =  
  fun x -> f (g x)

let rec repeated f n =
  if n = 1 then f
  else compose f (repeated f (n-1))

let average_damp f = 
  fun x -> average  x  (f x)

let is_close_enough p1 p2 = 
  compare_float (Float.abs (p1-.p2)) tolerance = (-1)

let fixed_point g guess =
  let rec tryme guess = 
    let next = g guess in
    if is_close_enough guess next then
      next
    else tryme next in
    tryme guess

let target x n= 
  fun y -> x /.(power y (n-1))

let fixed_point_nth_damp damp g guess = 
  let f = (repeated average_damp damp) g in
  fixed_point f guess
let target_fixed_point x n guess=
  fixed_point_nth_damp (log2 n) (target x n) guess

let () = 
printf "%F\n" (target_fixed_point 2.0 256 1.0)


(* https://sicp.readthedocs.io/en/latest/chp1/45.html 
因为需要对输入的公式进行不定数量的 average-damp 以确保不动点收敛，为了保持代码的可读性，我们可以写一个辅助函数来做这件事：
函数 damped-nth-root 接受两个参数 n 和 damp-times ： n 表示要计算的方根次数， damp-times 指定要对公式进行多少次平均阻尼变换。
damped-nth-root 的返回值是一个过程，它接受参数 x ，并计算 x 的 n 次方根。
可以通过定义平方根、立方根和四次方根来测试 damped-nth-root （因为暂时只知道这三个方根需要多少次平均阻尼）：
*)