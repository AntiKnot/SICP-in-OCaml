type account  = {mutable balance: int};;
let myaccount = ref {balance= 1000000};; 

let withdraw account amount = 
  begin
  account.balance <- account.balance - amount;
  account.balance;
  end;;

let deposit account amount = 
  begin
  account.balance <- account.balance + amount;
  account.balance;
  end;;

let make_withdraw balance amount = 
  let _bal = ref balance in
  begin 
    _bal := !_bal-amount;
    !_bal;
  end;;

let w1 = make_withdraw 100;;

let () = print_int (w1 10);;
