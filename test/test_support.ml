module T = Trading_engine

let ok = function Ok value -> value | Error message -> Alcotest.fail message

let error = function
  | Error message -> message
  | Ok _ -> Alcotest.fail "expected an error"

let price value = T.Scalar.Price.of_decimal_string value |> ok
let quantity value = T.Scalar.Quantity.of_string value |> ok
let money value = T.Scalar.Money.of_decimal_string value |> ok
let instrument_id value = T.Id.Instrument.of_string_exn value
let order_id value = T.Id.Order.of_string_exn value
let fill_id value = T.Id.Fill.of_string_exn value
let run_id value = T.Id.Run.of_string_exn value
let timestamp value = T.Codec.ptime_of_string value |> ok

let quantity_testable =
  Alcotest.testable T.Scalar.Quantity.pp T.Scalar.Quantity.equal

let price_testable = Alcotest.testable T.Scalar.Price.pp T.Scalar.Price.equal
let money_testable = Alcotest.testable T.Scalar.Money.pp T.Scalar.Money.equal
let order_id_testable = Alcotest.testable T.Id.Order.pp T.Id.Order.equal

let instrument ?(id = "test-equity") ?(symbol = "TEST") ?(currency = "USD")
    ?(tick_size = "0.01") ?(lot_size = "1") () =
  T.Instrument.create ~id:(instrument_id id) ~symbol ~quote_currency:currency
    ~tick_size:(price tick_size) ~lot_size:(quantity lot_size)
  |> ok

let day sequence = Int64.to_int sequence + 1

let bar ?(instrument = instrument_id "test-equity") ?(open_price = "100")
    ?(high_price = "120") ?(low_price = "80") ?(close_price = "105")
    ?(volume = Some "100") ?start_at ?end_at ?available_at ?received_at sequence
    =
  let day = day sequence in
  let start_at =
    Option.value start_at
      ~default:(timestamp (Printf.sprintf "2026-01-%02dT14:30:00Z" day))
  in
  let end_at =
    Option.value end_at
      ~default:(timestamp (Printf.sprintf "2026-01-%02dT21:00:00Z" day))
  in
  let available_at =
    Option.value available_at
      ~default:(timestamp (Printf.sprintf "2026-01-%02dT21:00:01Z" day))
  in
  let received_at =
    Option.value received_at
      ~default:(timestamp (Printf.sprintf "2026-01-%02dT21:00:02Z" day))
  in
  let volume = Option.map quantity volume in
  T.Bar.create ~source_sequence:sequence ~instrument_id:instrument ~start_at
    ~end_at ~available_at ~received_at ~open_price:(price open_price)
    ~high_price:(price high_price) ~low_price:(price low_price)
    ~close_price:(price close_price) ~volume
  |> ok

let request ?(instrument = instrument_id "test-equity") ?(side = T.Order.Buy)
    ?(quantity_value = "10") ?(kind = T.Order.Market) ?(origin = T.Order.Direct)
    () =
  T.Order.request ~instrument_id:instrument ~side
    ~quantity:(quantity quantity_value) ~kind ~origin
  |> ok

let accepted_order ?(id = "order-1") ?(accepted_sequence = 1L)
    ?(created_at = timestamp "2026-01-02T21:00:02Z")
    ?(eligible_after_bar_sequence = 1L) request =
  T.Order.accept ~id:(order_id id) ~accepted_sequence ~created_at
    ~eligible_after_bar_sequence request
  |> ok

let oms_with_order ?(id = "order-1") ?(accepted_sequence = 1L)
    ?(created_at = timestamp "2026-01-02T21:00:02Z")
    ?(eligible_after_bar_sequence = 1L) request =
  T.Oms.accept T.Oms.empty ~id:(order_id id) ~accepted_sequence ~created_at
    ~eligible_after_bar_sequence request
  |> ok

let fill ?(id = "fill-1") ?(price_value = "100") ?(quantity_value = "1")
    ?(fee_value = "0") ?(executed_at = timestamp "2026-01-03T14:30:00Z")
    ?(bar_sequence = 2L) order =
  T.Fill.create ~id:(fill_id id) ~order_id:order.T.Order.id
    ~instrument_id:order.request.instrument_id ~side:order.request.side
    ~quantity:(quantity quantity_value) ~price:(price price_value)
    ~fee:(money fee_value) ~executed_at ~bar_sequence
  |> ok

let execution ?(participation_bps = 10_000) ?(fixed_fee = "0") ?(fee_bps = 0) ()
    =
  T.Execution.create ~participation_bps ~fixed_fee:(money fixed_fee) ~fee_bps
  |> ok

let risk ?(base_currency = "USD") ?(instruments = [ instrument () ])
    ?(max_order = "1000") ?(max_position = "1000") () =
  T.Risk.create ~base_currency ~instruments
    ~max_order_quantity:(quantity max_order)
    ~max_position:(quantity max_position)
  |> ok

let engine_config ?(risk = risk ()) ?(execution = execution ())
    ?(max_internal_events = 1000) () =
  T.Engine.config ~risk ~execution ~max_internal_events |> ok
