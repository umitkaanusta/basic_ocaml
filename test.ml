open OUnit2
open Sum (* Module import *)

(* Building a test suite *)
let tests = "test suite for sum_list" >::: [
  "empty" >:: (fun _ -> assert_equal 0 (sum []));
  "singleton" >:: (fun _ -> assert_equal 1 (sum [1]));
  "two_elements" >:: (fun _ -> assert_equal 3 (sum [1; 2]));
]
let _ = run_test_tt_main tests


(* Abstracting the test suite a bit *)
let make_sum_testcase case_name input desired_output = 
    case_name >:: (fun _ -> assert_equal (sum input) desired_output ~printer:string_of_int)
let tests = "test suite for sum" >::: [
  make_sum_testcase "empty" [] 0;
  make_sum_testcase "singleton" [1] 1;
  make_sum_testcase "two_elements" [1; 2] 3;
]
let _ = run_test_tt_main tests
