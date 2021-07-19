utop trace 

I didn't find a way for tracing function in file.ml Conveniently. So I pasted the code into utop to complete this task.  

```
utop # open Base;;
─( 11:00:30 )─< command 2 >──────────────────────────────────────────────────────────────────────────────{ counter: 0 }─
utop # open Stdio;;
─( 11:01:52 )─< command 3 >──────────────────────────────────────────────────────────────────────────────{ counter: 0 }─
utop # let cube x = x*.x*.x;;
val cube : float -> float = <fun>
─( 11:01:56 )─< command 4 >──────────────────────────────────────────────────────────────────────────────{ counter: 0 }─
utop # let p x  = 3.*.x-.4.*.(cube x);;
val p : float -> float = <fun>
─( 11:02:07 )─< command 5 >──────────────────────────────────────────────────────────────────────────────{ counter: 0 }─
utop # let rec sine angle = if compare_float (Float.abs angle) 0.1 = (-1) then angle else p (sine (angle /. 3.0));;
val sine : float -> float = <fun>
─( 11:02:16 )─< command 6 >──────────────────────────────────────────────────────────────────────────────{ counter: 0 }─
utop # #trace sine;;
sine is now traced.
─( 11:02:31 )─< command 7 >──────────────────────────────────────────────────────────────────────────────{ counter: 0 }─
utop # let () = printf "sine 1.0: %F\n" (sine 12.15)
;;
sine <-- 12.15
sine <-- 4.05
sine <-- 1.3499999999999999
sine <-- 0.44999999999999996
sine <-- 0.15
sine <-- 0.049999999999999996
sine --> 0.049999999999999996
sine --> 0.1495
sine --> 0.4351345505
sine --> 0.97584653316787717
sine --> -0.78956311447082284
sine --> -0.39980345741334
sine 1.0: -0.399803457413
─( 11:02:46 )─< command 8 >─────────────────────────────────────────────────────────────────( 11:02:53 )─< command 8─( 1─( 11:02:53 )─< command 8 >──────────────────────────────────────────────────────────────────────────────{ counter: 0 }─
utop #
```
