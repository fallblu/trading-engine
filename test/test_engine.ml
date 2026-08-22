let () =
  Alcotest.run "trading-engine"
    [
      ("diagnostic", Test_diagnostic.tests);
      ("domain", Test_domain.tests);
      ("accounting", Test_accounting.tests);
      ("execution", Test_execution.tests);
      ("reducer", Test_reducer.tests);
      ("reducer-properties", Test_reducer_properties.tests);
      ("checkpoint4", Test_checkpoint4.tests);
      ("strategy-protocol", Test_strategy_protocol.tests);
      ("contract-conformance", Test_contract_conformance.tests);
      ("boundary-failures", Test_boundary_failures.tests);
      ("venue-calendar", Test_venue_calendar.tests);
      ("risk-groups", Test_risk_groups.tests);
      ("scenario", Test_scenario.tests);
    ]
