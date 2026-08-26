type t = {
  base_currency : string;
  instruments : Instrument.t Id.Instrument.Map.t;
  instrument_policies : instrument_policy Id.Instrument.Map.t;
  groups : group list;
  max_order_quantity : Scalar.Quantity.t;
  max_long_position : Scalar.Quantity.t;
  max_short_position : Scalar.Quantity.t;
  max_gross_exposure : Scalar.Money.t;
  max_leverage : Scalar.Ratio.t;
  initial_margin_bps : int;
  maintenance_margin_bps : int;
}

and instrument_policy = {
  instrument_id : Id.Instrument.t;
  max_order_quantity : Scalar.Quantity.t;
  max_long_position : Scalar.Quantity.t;
  max_short_position : Scalar.Quantity.t;
  max_notional_exposure : Scalar.Money.t option;
  initial_margin_bps : int;
  maintenance_margin_bps : int;
  shorting_allowed : bool;
}

and group_kind = Issuer | Sector | Currency | Country | Asset_class | Custom

and group_limits = {
  max_gross_exposure : Scalar.Money.t option;
  max_long_exposure : Scalar.Money.t option;
  max_short_exposure : Scalar.Money.t option;
  max_absolute_net_exposure : Scalar.Money.t option;
  max_concentration : Scalar.Ratio.t option;
}

and group = {
  group_id : Id.Risk_group.t;
  group_kind : group_kind;
  instrument_ids : Id.Instrument.t list;
  limits : group_limits;
}

type group_exposure = {
  group_id : Id.Risk_group.t;
  gross_exposure : Scalar.Money.t;
  net_exposure : Scalar.Money.t;
  long_exposure : Scalar.Money.t;
  short_exposure : Scalar.Money.t;
  concentration : Scalar.Weight.t option;
}

type margin_snapshot = {
  initial_requirement : Scalar.Money.t;
  maintenance_requirement : Scalar.Money.t;
  initial_excess : Scalar.Money.t;
  maintenance_excess : Scalar.Money.t;
  margin_call : bool;
  group_exposures : group_exposure list;
}

type fill_limit =
  | Maximum_order_quantity of Scalar.Quantity.t
  | Maximum_long_position of Scalar.Quantity.t
  | Maximum_short_position of Scalar.Quantity.t
  | Maximum_gross_exposure of Scalar.Money.t
  | Maximum_leverage of Scalar.Ratio.t
  | Initial_margin of int
  | Instrument_maximum_long_position of Id.Instrument.t * Scalar.Quantity.t
  | Instrument_maximum_short_position of Id.Instrument.t * Scalar.Quantity.t
  | Instrument_maximum_notional of Id.Instrument.t * Scalar.Money.t
  | Instrument_shorting_disabled of Id.Instrument.t
  | Instrument_borrow_availability of Id.Instrument.t * Scalar.Quantity.t
  | Settlement_cash_buying_power of string * Scalar.Money.t
  | Settlement_position_availability of Id.Instrument.t * Scalar.Quantity.t
  | Instrument_initial_margin of Id.Instrument.t * int
  | Group_maximum_gross of Id.Risk_group.t * Scalar.Money.t
  | Group_maximum_long of Id.Risk_group.t * Scalar.Money.t
  | Group_maximum_short of Id.Risk_group.t * Scalar.Money.t
  | Group_maximum_absolute_net of Id.Risk_group.t * Scalar.Money.t
  | Group_maximum_concentration of Id.Risk_group.t * Scalar.Ratio.t

type fill_check_error = Limit of fill_limit | Invalid of string

let ( let* ) result function_ =
  match result with Ok value -> function_ value | Error _ as error -> error

let valid_label value =
  String.length value > 0
  && String.for_all
       (fun character ->
         let code = Char.code character in
         code >= 0x21 && code <> 0x7f)
       value

let valid_margin ~initial_margin_bps ~maintenance_margin_bps =
  if initial_margin_bps <= 0 || initial_margin_bps > 10_000 then
    Error "initial margin basis points must be between 1 and 10000"
  else if maintenance_margin_bps <= 0 || maintenance_margin_bps > 10_000 then
    Error "maintenance margin basis points must be between 1 and 10000"
  else if initial_margin_bps < maintenance_margin_bps then
    Error "initial margin must not be below maintenance margin"
  else Ok ()

let create_instrument_policy ~instrument ~max_order_quantity ~max_long_position
    ~max_short_position ~max_notional_exposure ~initial_margin_bps
    ~maintenance_margin_bps ~shorting_allowed =
  let lot = instrument.Instrument.lot_size in
  if not (Scalar.Quantity.is_positive max_order_quantity) then
    Error "maximum order quantity must be positive"
  else if not (Scalar.Quantity.is_positive max_long_position) then
    Error "maximum long position must be positive"
  else if not (Scalar.Quantity.is_positive max_short_position) then
    Error "maximum short position must be positive"
  else if Scalar.Quantity.compare max_order_quantity lot < 0 then
    Error "maximum order quantity must cover the instrument lot size"
  else if Scalar.Quantity.compare max_long_position lot < 0 then
    Error "maximum long position must cover the instrument lot size"
  else if Scalar.Quantity.compare max_short_position lot < 0 then
    Error "maximum short position must cover the instrument lot size"
  else if
    Option.exists
      (fun value -> Scalar.Money.compare value Scalar.Money.zero <= 0)
      max_notional_exposure
  then Error "maximum instrument notional exposure must be positive"
  else
    let* () = valid_margin ~initial_margin_bps ~maintenance_margin_bps in
    Ok
      {
        instrument_id = instrument.id;
        max_order_quantity;
        max_long_position;
        max_short_position;
        max_notional_exposure;
        initial_margin_bps;
        maintenance_margin_bps;
        shorting_allowed;
      }

let create_group_limits ~max_gross_exposure ~max_long_exposure
    ~max_short_exposure ~max_absolute_net_exposure ~max_concentration =
  let positive_money = function
    | None -> true
    | Some value -> Scalar.Money.compare value Scalar.Money.zero > 0
  in
  if
    not
      (List.for_all positive_money
         [
           max_gross_exposure;
           max_long_exposure;
           max_short_exposure;
           max_absolute_net_exposure;
         ])
  then Error "group money limits must be positive"
  else if
    Option.exists
      (fun value -> Scalar.Ratio.compare value Scalar.Ratio.one > 0)
      max_concentration
  then Error "group concentration must be greater than zero and at most one"
  else if
    List.for_all Option.is_none
      [
        max_gross_exposure;
        max_long_exposure;
        max_short_exposure;
        max_absolute_net_exposure;
      ]
    && Option.is_none max_concentration
  then Error "group must configure at least one limit"
  else
    Ok
      {
        max_gross_exposure;
        max_long_exposure;
        max_short_exposure;
        max_absolute_net_exposure;
        max_concentration;
      }

let create_group ~group_id ~group_kind ~instrument_ids ~limits =
  if instrument_ids = [] then Error "risk group must contain an instrument"
  else if
    List.length instrument_ids
    <> List.length (List.sort_uniq Id.Instrument.compare instrument_ids)
  then Error "risk group instrument IDs must be unique"
  else
    Ok
      {
        group_id;
        group_kind;
        instrument_ids = List.sort Id.Instrument.compare instrument_ids;
        limits;
      }

let create ~base_currency ~instruments
    ~(instrument_policies : instrument_policy list) ~(groups : group list)
    ~max_gross_exposure ~max_leverage =
  if not (valid_label base_currency) then
    Error "base currency must not be empty or contain whitespace"
  else if instruments = [] then Error "risk must define at least one instrument"
  else if Scalar.Money.compare max_gross_exposure Scalar.Money.zero <= 0 then
    Error "maximum gross exposure must be positive"
  else
    let add_instrument result instrument =
      let* map = result in
      if Id.Instrument.Map.mem instrument.Instrument.id map then
        Error "instrument IDs must be unique"
      else Ok (Id.Instrument.Map.add instrument.id instrument map)
    in
    let* instrument_map =
      List.fold_left add_instrument (Ok Id.Instrument.Map.empty) instruments
    in
    let add_policy result policy =
      let* map = result in
      if not (Id.Instrument.Map.mem policy.instrument_id instrument_map) then
        Error "instrument policy refers to an unknown instrument"
      else if Id.Instrument.Map.mem policy.instrument_id map then
        Error "instrument policy IDs must be unique"
      else Ok (Id.Instrument.Map.add policy.instrument_id policy map)
    in
    let* policy_map =
      List.fold_left add_policy (Ok Id.Instrument.Map.empty) instrument_policies
    in
    if
      Id.Instrument.Map.cardinal policy_map
      <> Id.Instrument.Map.cardinal instrument_map
    then Error "risk must define exactly one policy for every instrument"
    else
      let group_ids = List.map (fun (group : group) -> group.group_id) groups in
      if
        List.length group_ids
        <> List.length (List.sort_uniq Id.Risk_group.compare group_ids)
      then Error "risk group IDs must be unique"
      else if
        List.exists
          (fun (group : group) ->
            List.exists
              (fun instrument_id ->
                not (Id.Instrument.Map.mem instrument_id instrument_map))
              group.instrument_ids)
          groups
      then Error "risk group refers to an unknown instrument"
      else
        let representative = List.hd instrument_policies in
        Ok
          {
            base_currency;
            instruments = instrument_map;
            instrument_policies = policy_map;
            groups =
              List.sort
                (fun (left : group) right ->
                  Id.Risk_group.compare left.group_id right.group_id)
                groups;
            max_order_quantity = representative.max_order_quantity;
            max_long_position = representative.max_long_position;
            max_short_position = representative.max_short_position;
            max_gross_exposure;
            max_leverage;
            initial_margin_bps = representative.initial_margin_bps;
            maintenance_margin_bps = representative.maintenance_margin_bps;
          }

let base_currency state = state.base_currency

let instruments state =
  Id.Instrument.Map.bindings state.instruments |> List.map snd

let instrument state instrument_id =
  Id.Instrument.Map.find_opt instrument_id state.instruments

let instrument_policies state =
  Id.Instrument.Map.bindings state.instrument_policies |> List.map snd

let instrument_policy state instrument_id =
  Id.Instrument.Map.find_opt instrument_id state.instrument_policies

let groups state = state.groups
let max_order_quantity state = state.max_order_quantity
let max_long_position state = state.max_long_position
let max_short_position state = state.max_short_position
let max_gross_exposure state = state.max_gross_exposure
let max_leverage state = state.max_leverage
let initial_margin_bps state = state.initial_margin_bps
let maintenance_margin_bps state = state.maintenance_margin_bps

let max_order_quantity_for state instrument_id =
  Option.map
    (fun (policy : instrument_policy) -> policy.max_order_quantity)
    (instrument_policy state instrument_id)

let group_exposure_from_positions group ~equity positions =
  let member instrument_id =
    List.exists (Id.Instrument.equal instrument_id) group.instrument_ids
  in
  let* net, long, short =
    List.fold_left
      (fun result (position : Account.position_attribution) ->
        let* net, long, short = result in
        if not (member position.instrument_id) then Ok (net, long, short)
        else
          let* net = Scalar.Money.add net position.base_market_value in
          if
            Scalar.Money.compare position.base_market_value Scalar.Money.zero
            >= 0
          then
            let* long = Scalar.Money.add long position.base_market_value in
            Ok (net, long, short)
          else
            let* magnitude = Scalar.Money.negate position.base_market_value in
            let* short = Scalar.Money.add short magnitude in
            Ok (net, long, short))
      (Ok (Scalar.Money.zero, Scalar.Money.zero, Scalar.Money.zero))
      positions
  in
  let* gross = Scalar.Money.add long short in
  let* concentration =
    if Scalar.Money.compare equity Scalar.Money.zero <= 0 then Ok None
    else Scalar.Money.weight_toward_zero gross ~equity |> Result.map Option.some
  in
  Ok
    {
      group_id = group.group_id;
      gross_exposure = gross;
      net_exposure = net;
      long_exposure = long;
      short_exposure = short;
      concentration;
    }

let group_exposures state (valuation : Account.valuation) =
  List.fold_left
    (fun result group ->
      let* exposures = result in
      let* exposure =
        group_exposure_from_positions group ~equity:valuation.Account.equity
          valuation.positions
      in
      Ok (exposure :: exposures))
    (Ok []) state.groups
  |> Result.map List.rev

let margin_snapshot state (valuation : Account.valuation) =
  let requirements =
    List.fold_left
      (fun result (position : Account.position_attribution) ->
        let* initial, maintenance = result in
        let* notional = Scalar.Money.absolute position.base_market_value in
        let* policy =
          match instrument_policy state position.instrument_id with
          | Some policy -> Ok policy
          | None -> Error "valuation position has no instrument risk policy"
        in
        let* item_initial =
          Scalar.Money.bps_ceil notional ~bps:policy.initial_margin_bps
        in
        let* item_maintenance =
          Scalar.Money.bps_ceil notional ~bps:policy.maintenance_margin_bps
        in
        let* initial = Scalar.Money.add initial item_initial in
        let* maintenance = Scalar.Money.add maintenance item_maintenance in
        Ok (initial, maintenance))
      (Ok (Scalar.Money.zero, Scalar.Money.zero))
      valuation.positions
  in
  let* initial_requirement, maintenance_requirement = requirements in
  let* initial_excess =
    Scalar.Money.subtract valuation.equity initial_requirement
  in
  let* maintenance_excess =
    Scalar.Money.subtract valuation.equity maintenance_requirement
  in
  let* group_exposures = group_exposures state valuation in
  Ok
    {
      initial_requirement;
      maintenance_requirement;
      initial_excess;
      maintenance_excess;
      margin_call =
        Scalar.Money.compare maintenance_excess Scalar.Money.zero < 0;
      group_exposures;
    }

let check_initial state valuation =
  let* () =
    if
      Scalar.Money.compare valuation.Account.gross_exposure
        state.max_gross_exposure
      > 0
    then Error "portfolio would exceed maximum gross exposure"
    else
      let* leveraged_equity =
        Scalar.Money.multiply_ratio valuation.equity state.max_leverage
      in
      if Scalar.Money.compare valuation.gross_exposure leveraged_equity > 0 then
        Error "portfolio would exceed maximum leverage"
      else Ok ()
  in
  let* () =
    List.fold_left
      (fun result (position : Account.position_attribution) ->
        let* () = result in
        let* policy =
          match instrument_policy state position.instrument_id with
          | Some policy -> Ok policy
          | None -> Error "initial position has no instrument risk policy"
        in
        let* minimum_short = Scalar.Quantity.negate policy.max_short_position in
        let* () =
          if
            Scalar.Quantity.compare position.quantity policy.max_long_position
            > 0
          then Error "initial position exceeds its maximum long position"
          else if Scalar.Quantity.compare position.quantity minimum_short < 0
          then Error "initial position exceeds its maximum short position"
          else if
            (not policy.shorting_allowed)
            && Scalar.Quantity.is_negative position.quantity
          then Error "initial position violates its shorting policy"
          else Ok ()
        in
        let* notional = Scalar.Money.absolute position.base_market_value in
        match policy.max_notional_exposure with
        | Some limit when Scalar.Money.compare notional limit > 0 ->
            Error
              "initial position exceeds the instrument maximum notional \
               exposure"
        | _ -> Ok ())
      (Ok ()) valuation.positions
  in
  let* margin = margin_snapshot state valuation in
  let* () =
    if Scalar.Money.compare margin.initial_excess Scalar.Money.zero < 0 then
      Error "portfolio would violate instrument initial margin requirements"
    else Ok ()
  in
  List.fold_left
    (fun result (group : group) ->
      let* () = result in
      let* exposure =
        match
          List.find_opt
            (fun item -> Id.Risk_group.equal item.group_id group.group_id)
            margin.group_exposures
        with
        | Some exposure -> Ok exposure
        | None -> Error "initial valuation omitted a configured risk group"
      in
      let* absolute_net = Scalar.Money.absolute exposure.net_exposure in
      let exceeds option observed =
        Option.exists
          (fun limit -> Scalar.Money.compare observed limit > 0)
          option
      in
      let group_name = Id.Risk_group.to_string group.group_id in
      if exceeds group.limits.max_gross_exposure exposure.gross_exposure then
        Error
          (Printf.sprintf
             "initial portfolio exceeds group %s maximum gross exposure"
             group_name)
      else if exceeds group.limits.max_long_exposure exposure.long_exposure then
        Error
          (Printf.sprintf
             "initial portfolio exceeds group %s maximum long exposure"
             group_name)
      else if exceeds group.limits.max_short_exposure exposure.short_exposure
      then
        Error
          (Printf.sprintf
             "initial portfolio exceeds group %s maximum short exposure"
             group_name)
      else if exceeds group.limits.max_absolute_net_exposure absolute_net then
        Error
          (Printf.sprintf
             "initial portfolio exceeds group %s maximum absolute net exposure"
             group_name)
      else
        match group.limits.max_concentration with
        | None -> Ok ()
        | Some limit ->
            let* threshold =
              Scalar.Money.multiply_ratio valuation.equity limit
            in
            if Scalar.Money.compare exposure.gross_exposure threshold > 0 then
              Error
                (Printf.sprintf
                   "initial portfolio exceeds group %s maximum concentration"
                   group_name)
            else Ok ())
    (Ok ()) state.groups

let invalid result = Result.map_error (fun message -> Invalid message) result

let check_fill_initial state ~equity ~gross_exposure =
  if Scalar.Money.compare gross_exposure state.max_gross_exposure > 0 then
    Error (Limit (Maximum_gross_exposure state.max_gross_exposure))
  else
    let* leveraged_equity =
      Scalar.Money.multiply_ratio equity state.max_leverage |> invalid
    in
    if Scalar.Money.compare gross_exposure leveraged_equity > 0 then
      Error (Limit (Maximum_leverage state.max_leverage))
    else
      let* initial_requirement =
        Scalar.Money.bps_ceil gross_exposure ~bps:state.initial_margin_bps
        |> invalid
      in
      let* initial_excess =
        Scalar.Money.subtract equity initial_requirement |> invalid
      in
      if Scalar.Money.compare initial_excess Scalar.Money.zero < 0 then
        Error (Limit (Initial_margin state.initial_margin_bps))
      else Ok ()

let check_post_fill state ~before_position ~after_position ~before ~after =
  let* before_absolute = Scalar.Quantity.absolute before_position |> invalid in
  let* after_absolute = Scalar.Quantity.absolute after_position |> invalid in
  if Scalar.Quantity.compare after_absolute before_absolute <= 0 then Ok ()
  else if Scalar.Quantity.compare after_position state.max_long_position > 0
  then Error (Limit (Maximum_long_position state.max_long_position))
  else
    let* minimum_short =
      Scalar.Quantity.negate state.max_short_position |> invalid
    in
    if Scalar.Quantity.compare after_position minimum_short < 0 then
      Error (Limit (Maximum_short_position state.max_short_position))
    else if
      Scalar.Money.compare after.Account.gross_exposure
        before.Account.gross_exposure
      <= 0
    then Ok ()
    else
      check_fill_initial state ~equity:after.equity
        ~gross_exposure:after.gross_exposure

type projected_value = {
  instrument_id : Id.Instrument.t;
  quantity : Scalar.Quantity.t;
  signed_value : Scalar.Money.t;
  absolute_value : Scalar.Money.t;
}

let sum_values values =
  List.fold_left
    (fun result value ->
      let* gross, long, short, net = result in
      let* gross = Scalar.Money.add gross value.absolute_value in
      let* net = Scalar.Money.add net value.signed_value in
      if Scalar.Money.compare value.signed_value Scalar.Money.zero >= 0 then
        let* long = Scalar.Money.add long value.signed_value in
        Ok (gross, long, short, net)
      else
        let* magnitude = Scalar.Money.negate value.signed_value in
        let* short = Scalar.Money.add short magnitude in
        Ok (gross, long, short, net))
    (Ok
       ( Scalar.Money.zero,
         Scalar.Money.zero,
         Scalar.Money.zero,
         Scalar.Money.zero ))
    values

let group_values (group : group) values =
  List.filter
    (fun value ->
      List.exists (Id.Instrument.equal value.instrument_id) group.instrument_ids)
    values
  |> sum_values

let check_group_fill_limit state ~equity values =
  let rec check = function
    | [] -> Ok ()
    | (group : group) :: rest ->
        let* gross, long, short, net = group_values group values |> invalid in
        let* absolute_net = Scalar.Money.absolute net |> invalid in
        let fail option observed make =
          match option with
          | Some limit when Scalar.Money.compare observed limit > 0 ->
              Error (Limit (make group.group_id limit))
          | _ -> Ok ()
        in
        let* () =
          fail group.limits.max_gross_exposure gross (fun id limit ->
              Group_maximum_gross (id, limit))
        in
        let* () =
          fail group.limits.max_long_exposure long (fun id limit ->
              Group_maximum_long (id, limit))
        in
        let* () =
          fail group.limits.max_short_exposure short (fun id limit ->
              Group_maximum_short (id, limit))
        in
        let* () =
          fail group.limits.max_absolute_net_exposure absolute_net
            (fun id limit -> Group_maximum_absolute_net (id, limit))
        in
        let* () =
          match group.limits.max_concentration with
          | None -> Ok ()
          | Some limit ->
              let* threshold =
                Scalar.Money.multiply_ratio equity limit |> invalid
              in
              if Scalar.Money.compare gross threshold > 0 then
                Error
                  (Limit (Group_maximum_concentration (group.group_id, limit)))
              else Ok ()
        in
        check rest
  in
  check state.groups

let check_post_fill_for _state ~instrument_id:_ ~before_position:_
    ~after_position:_ ~before:_ ~after:_ =
  Ok ()

let check_position state quantity =
  if Scalar.Quantity.compare quantity state.max_long_position > 0 then
    Error "position would exceed the maximum long position"
  else
    let* minimum_short = Scalar.Quantity.negate state.max_short_position in
    if Scalar.Quantity.compare quantity minimum_short < 0 then
      Error "position would exceed the maximum short position"
    else Ok ()

let check_position_for state instrument_id quantity =
  match instrument_policy state instrument_id with
  | None -> Error "position refers to an unknown instrument risk policy"
  | Some policy ->
      if Scalar.Quantity.compare quantity policy.max_long_position > 0 then
        Error "position would exceed the instrument maximum long position"
      else
        let* minimum_short = Scalar.Quantity.negate policy.max_short_position in
        if Scalar.Quantity.compare quantity minimum_short < 0 then
          Error "position would exceed the instrument maximum short position"
        else if
          (not policy.shorting_allowed) && Scalar.Quantity.is_negative quantity
        then Error "instrument policy does not allow short positions"
        else Ok ()

let check_alignment instrument request =
  if
    not
      (Scalar.Quantity.is_multiple request.Order.quantity
         ~lot:instrument.Instrument.lot_size)
  then Error "order quantity is not aligned to the instrument lot size"
  else
    match request.kind with
    | Order.Market -> Ok ()
    | Order.Limit price | Order.Stop price ->
        if Scalar.Price.is_multiple price ~tick:instrument.tick_size then Ok ()
        else Error "order price is not aligned to the instrument tick size"
    | Order.Stop_limit { trigger_price; limit_price } ->
        if
          Scalar.Price.is_multiple trigger_price ~tick:instrument.tick_size
          && Scalar.Price.is_multiple limit_price ~tick:instrument.tick_size
        then Ok ()
        else Error "order price is not aligned to the instrument tick size"

type reservations = { buys : Scalar.Quantity.t; sells : Scalar.Quantity.t }

let empty_reservations =
  { buys = Scalar.Quantity.zero; sells = Scalar.Quantity.zero }

let reservations_for_instrument ~oms instrument_id =
  Oms.active_for_instrument oms instrument_id
  |> List.fold_left
       (fun result order ->
         let* reservations = result in
         let remaining = Order.remaining_quantity order in
         match order.Order.request.side with
         | Order.Buy ->
             let* buys = Scalar.Quantity.add reservations.buys remaining in
             Ok { reservations with buys }
         | Order.Sell ->
             let* sells = Scalar.Quantity.add reservations.sells remaining in
             Ok { reservations with sells })
       (Ok empty_reservations)

let add_request reservations request =
  match request.Order.side with
  | Order.Buy ->
      let* buys = Scalar.Quantity.add reservations.buys request.quantity in
      Ok { reservations with buys }
  | Order.Sell ->
      let* sells = Scalar.Quantity.add reservations.sells request.quantity in
      Ok { reservations with sells }

let directional_positions ~account instrument_id reservations =
  let current = Account.position_quantity account instrument_id in
  let* buy_position = Scalar.Quantity.add current reservations.buys in
  let* sell_position = Scalar.Quantity.subtract current reservations.sells in
  Ok (buy_position, sell_position)

let position_for_side side (buy_position, sell_position) =
  match side with Order.Buy -> buy_position | Order.Sell -> sell_position

let worst_directional_position positions =
  let buy_position, sell_position = positions in
  let* buy_absolute = Scalar.Quantity.absolute buy_position in
  let* sell_absolute = Scalar.Quantity.absolute sell_position in
  if Scalar.Quantity.compare buy_absolute sell_absolute >= 0 then
    Ok buy_position
  else Ok sell_position

let check_self_cross ~oms request =
  let active = Oms.active_for_instrument oms request.Order.instrument_id in
  if
    List.exists
      (fun order -> order.Order.request.side <> request.Order.side)
      active
  then Error "order would self-cross an active opposite-side order"
  else Ok ()

let projected_position ~account ~oms request =
  let* reservations =
    reservations_for_instrument ~oms request.Order.instrument_id
  in
  let* pending_positions =
    directional_positions ~account request.instrument_id reservations
  in
  let pending = position_for_side request.side pending_positions in
  let* projected_reservations = add_request reservations request in
  let* projected_positions =
    directional_positions ~account request.instrument_id projected_reservations
  in
  let projected = position_for_side request.side projected_positions in
  if
    Scalar.Quantity.is_positive pending
    && Scalar.Quantity.is_negative projected
    || Scalar.Quantity.is_negative pending
       && Scalar.Quantity.is_positive projected
  then Error "one order must not cross a position through zero"
  else Ok (pending, projected)

let projected_valuation_quantities state ~account ~oms request =
  Id.Instrument.Map.bindings state.instruments
  |> List.fold_left
       (fun result (instrument_id, _) ->
         let* values = result in
         let* reservations = reservations_for_instrument ~oms instrument_id in
         let* reservations =
           if Id.Instrument.equal instrument_id request.Order.instrument_id then
             add_request reservations request
           else Ok reservations
         in
         let* positions =
           directional_positions ~account instrument_id reservations
         in
         let* quantity = worst_directional_position positions in
         Ok ((instrument_id, quantity) :: values))
       (Ok [])
  |> Result.map List.rev

let fill_projected_quantities state ~account ~oms ~(order : Order.t)
    ~filled_quantity =
  Id.Instrument.Map.bindings state.instruments
  |> List.fold_left
       (fun result (instrument_id, _) ->
         let* values = result in
         let* reservations = reservations_for_instrument ~oms instrument_id in
         let* reservations =
           if Id.Instrument.equal instrument_id order.request.instrument_id then
             match order.request.side with
             | Order.Buy ->
                 let* buys =
                   Scalar.Quantity.subtract reservations.buys filled_quantity
                 in
                 Ok { reservations with buys }
             | Order.Sell ->
                 let* sells =
                   Scalar.Quantity.subtract reservations.sells filled_quantity
                 in
                 Ok { reservations with sells }
           else Ok reservations
         in
         let* positions =
           directional_positions ~account instrument_id reservations
         in
         let* quantity = worst_directional_position positions in
         Ok ((instrument_id, quantity) :: values))
       (Ok [])
  |> Result.map List.rev

let projected_values state ~marks ~fx_rates quantities =
  let mark_map =
    List.fold_left
      (fun map (instrument_id, mark) ->
        Id.Instrument.Map.add instrument_id mark map)
      Id.Instrument.Map.empty marks
  in
  let module Currency_map = Map.Make (String) in
  let fx_map =
    List.fold_left
      (fun map (currency, rate) -> Currency_map.add currency rate map)
      Currency_map.empty fx_rates
  in
  List.fold_left
    (fun result (instrument_id, quantity) ->
      let* values = result in
      let* instrument =
        match instrument state instrument_id with
        | Some value -> Ok value
        | None -> Error "projected position has no configured instrument"
      in
      let* mark =
        match Id.Instrument.Map.find_opt instrument_id mark_map with
        | Some value -> Ok value
        | None -> Error "projected position has no current market price"
      in
      let* rate =
        match Currency_map.find_opt instrument.quote_currency fx_map with
        | Some value -> Ok value
        | None -> Error "projected position has no current FX rate"
      in
      let* signed_value = Scalar.Money.notional mark quantity in
      let* signed_value = Scalar.Money.convert signed_value ~rate in
      let* absolute_value = Scalar.Money.absolute signed_value in
      Ok ({ instrument_id; quantity; signed_value; absolute_value } :: values))
    (Ok []) quantities
  |> Result.map List.rev

let first_some checks =
  let rec loop = function
    | [] -> Ok ()
    | check :: rest -> (
        match check () with Ok () -> loop rest | Error _ as e -> e)
  in
  loop checks

let check_projected_values state ~equity values =
  let* gross, _, _, _ = sum_values values in
  let* () =
    if Scalar.Money.compare gross state.max_gross_exposure > 0 then
      Error "portfolio would exceed maximum gross exposure"
    else
      let* leveraged_equity =
        Scalar.Money.multiply_ratio equity state.max_leverage
      in
      if Scalar.Money.compare gross leveraged_equity > 0 then
        Error "portfolio would exceed maximum leverage"
      else
        let* initial_requirement =
          List.fold_left
            (fun result value ->
              let* total = result in
              let* policy =
                match instrument_policy state value.instrument_id with
                | Some policy -> Ok policy
                | None ->
                    Error "projected position has no instrument risk policy"
              in
              let* requirement =
                Scalar.Money.bps_ceil value.absolute_value
                  ~bps:policy.initial_margin_bps
              in
              Scalar.Money.add total requirement)
            (Ok Scalar.Money.zero) values
        in
        let* excess = Scalar.Money.subtract equity initial_requirement in
        if Scalar.Money.compare excess Scalar.Money.zero < 0 then
          Error "portfolio would violate instrument initial margin requirements"
        else Ok ()
  in
  let* () =
    List.fold_left
      (fun result value ->
        let* () = result in
        let* policy =
          match instrument_policy state value.instrument_id with
          | Some policy -> Ok policy
          | None -> Error "projected position has no instrument risk policy"
        in
        let* () = check_position_for state value.instrument_id value.quantity in
        match policy.max_notional_exposure with
        | Some limit when Scalar.Money.compare value.absolute_value limit > 0 ->
            Error
              "position would exceed the instrument maximum notional exposure"
        | _ -> Ok ())
      (Ok ()) values
  in
  List.fold_left
    (fun result (group : group) ->
      let* () = result in
      let* gross, long, short, net = group_values group values in
      let* absolute_net = Scalar.Money.absolute net in
      let concentration_exceeded limit =
        let* threshold = Scalar.Money.multiply_ratio equity limit in
        Ok (Scalar.Money.compare gross threshold > 0)
      in
      first_some
        [
          (fun () ->
            match group.limits.max_gross_exposure with
            | Some limit when Scalar.Money.compare gross limit > 0 ->
                Error
                  (Printf.sprintf
                     "position would exceed group %s maximum gross exposure"
                     (Id.Risk_group.to_string group.group_id))
            | _ -> Ok ());
          (fun () ->
            match group.limits.max_long_exposure with
            | Some limit when Scalar.Money.compare long limit > 0 ->
                Error
                  (Printf.sprintf
                     "position would exceed group %s maximum long exposure"
                     (Id.Risk_group.to_string group.group_id))
            | _ -> Ok ());
          (fun () ->
            match group.limits.max_short_exposure with
            | Some limit when Scalar.Money.compare short limit > 0 ->
                Error
                  (Printf.sprintf
                     "position would exceed group %s maximum short exposure"
                     (Id.Risk_group.to_string group.group_id))
            | _ -> Ok ());
          (fun () ->
            match group.limits.max_absolute_net_exposure with
            | Some limit when Scalar.Money.compare absolute_net limit > 0 ->
                Error
                  (Printf.sprintf
                     "position would exceed group %s maximum absolute net \
                      exposure"
                     (Id.Risk_group.to_string group.group_id))
            | _ -> Ok ());
          (fun () ->
            match group.limits.max_concentration with
            | None -> Ok ()
            | Some limit ->
                let* exceeded = concentration_exceeded limit in
                if exceeded then
                  Error
                    (Printf.sprintf
                       "position would exceed group %s maximum concentration"
                       (Id.Risk_group.to_string group.group_id))
                else Ok ());
        ])
    (Ok ()) state.groups

let check_reserved_fill state ~account ~oms ~marks ~fx_rates ~(order : Order.t)
    ~filled_quantity ~after =
  let instrument_id = order.request.instrument_id in
  let* quantities =
    fill_projected_quantities state ~account ~oms ~order ~filled_quantity
    |> invalid
  in
  let* values = projected_values state ~marks ~fx_rates quantities |> invalid in
  let* policy =
    match instrument_policy state instrument_id with
    | Some policy -> Ok policy
    | None -> Error (Invalid "fill has no instrument risk policy")
  in
  let* () =
    List.fold_left
      (fun result value ->
        let* () = result in
        let* item_policy =
          match instrument_policy state value.instrument_id with
          | Some policy -> Ok policy
          | None -> Error (Invalid "fill projection has no risk policy")
        in
        if
          Scalar.Quantity.compare value.quantity item_policy.max_long_position
          > 0
        then
          Error
            (Limit
               (Instrument_maximum_long_position
                  (value.instrument_id, item_policy.max_long_position)))
        else
          let* minimum_short =
            Scalar.Quantity.negate item_policy.max_short_position |> invalid
          in
          if Scalar.Quantity.compare value.quantity minimum_short < 0 then
            Error
              (Limit
                 (Instrument_maximum_short_position
                    (value.instrument_id, item_policy.max_short_position)))
          else if
            (not item_policy.shorting_allowed)
            && Scalar.Quantity.is_negative value.quantity
          then Error (Limit (Instrument_shorting_disabled value.instrument_id))
          else
            match item_policy.max_notional_exposure with
            | Some limit
              when Scalar.Money.compare value.absolute_value limit > 0 ->
                Error
                  (Limit
                     (Instrument_maximum_notional (value.instrument_id, limit)))
            | _ -> Ok ())
      (Ok ()) values
  in
  let* gross, _, _, _ = sum_values values |> invalid in
  let* () =
    if Scalar.Money.compare gross state.max_gross_exposure > 0 then
      Error (Limit (Maximum_gross_exposure state.max_gross_exposure))
    else
      let* leveraged_equity =
        Scalar.Money.multiply_ratio after.Account.equity state.max_leverage
        |> invalid
      in
      if Scalar.Money.compare gross leveraged_equity > 0 then
        Error (Limit (Maximum_leverage state.max_leverage))
      else Ok ()
  in
  let* initial_requirement =
    List.fold_left
      (fun result value ->
        let* total = result in
        let* item_policy =
          match instrument_policy state value.instrument_id with
          | Some policy -> Ok policy
          | None -> Error (Invalid "fill projection has no risk policy")
        in
        let* requirement =
          Scalar.Money.bps_ceil value.absolute_value
            ~bps:item_policy.initial_margin_bps
          |> invalid
        in
        Scalar.Money.add total requirement |> invalid)
      (Ok Scalar.Money.zero) values
  in
  let* excess =
    Scalar.Money.subtract after.equity initial_requirement |> invalid
  in
  if Scalar.Money.compare excess Scalar.Money.zero < 0 then
    Error
      (Limit
         (Instrument_initial_margin (instrument_id, policy.initial_margin_bps)))
  else check_group_fill_limit state ~equity:after.equity values

let check state ~account ~oms ~marks ~fx_rates (request : Order.request) =
  match instrument state request.instrument_id with
  | None -> Error "order refers to an unknown instrument"
  | Some instrument ->
      let* policy =
        match instrument_policy state request.instrument_id with
        | Some value -> Ok value
        | None -> Error "order has no instrument risk policy"
      in
      if
        Scalar.Quantity.compare request.Order.quantity policy.max_order_quantity
        > 0
      then Error "order exceeds the instrument maximum order quantity"
      else
        let* () = check_alignment instrument request in
        let* () = check_self_cross ~oms request in
        let* pending, projected = projected_position ~account ~oms request in
        let* pending_absolute = Scalar.Quantity.absolute pending in
        let* projected_absolute = Scalar.Quantity.absolute projected in
        if Scalar.Quantity.compare projected_absolute pending_absolute <= 0 then
          Ok ()
        else
          let* () = check_position_for state request.instrument_id projected in
          let* before =
            Account.value account ~instruments:(instruments state) ~marks
              ~fx_rates
          in
          let* quantities =
            projected_valuation_quantities state ~account ~oms request
          in
          let* values = projected_values state ~marks ~fx_rates quantities in
          check_projected_values state ~equity:before.equity values
