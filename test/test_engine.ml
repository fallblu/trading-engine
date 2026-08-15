let () =
  Alcotest.run "trading-engine"
    [
      ("domain", Test_domain.tests);
      ("accounting", Test_accounting.tests);
      ("execution", Test_execution.tests);
      ("reducer", Test_reducer.tests);
      ("scenario", Test_scenario.tests);
    ]
