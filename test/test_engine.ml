let () =
  Alcotest.run "trading-engine"
    [
      ("diagnostic", Test_diagnostic.tests);
      ("domain", Test_domain.tests);
      ("accounting", Test_accounting.tests);
      ("execution", Test_execution.tests);
      ("reducer", Test_reducer.tests);
      ("checkpoint4", Test_checkpoint4.tests);
      ("strategy-protocol", Test_strategy_protocol.tests);
      ("scenario", Test_scenario.tests);
    ]
