let%expect_test "hello world" =
  print_endline "test";
  [%expect {| test |}]
