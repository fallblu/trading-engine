let initial =
  [|
    0x6a09e667l;
    0xbb67ae85l;
    0x3c6ef372l;
    0xa54ff53al;
    0x510e527fl;
    0x9b05688cl;
    0x1f83d9abl;
    0x5be0cd19l;
  |]

let constants =
  [|
    0x428a2f98l;
    0x71374491l;
    0xb5c0fbcfl;
    0xe9b5dba5l;
    0x3956c25bl;
    0x59f111f1l;
    0x923f82a4l;
    0xab1c5ed5l;
    0xd807aa98l;
    0x12835b01l;
    0x243185bel;
    0x550c7dc3l;
    0x72be5d74l;
    0x80deb1fel;
    0x9bdc06a7l;
    0xc19bf174l;
    0xe49b69c1l;
    0xefbe4786l;
    0x0fc19dc6l;
    0x240ca1ccl;
    0x2de92c6fl;
    0x4a7484aal;
    0x5cb0a9dcl;
    0x76f988dal;
    0x983e5152l;
    0xa831c66dl;
    0xb00327c8l;
    0xbf597fc7l;
    0xc6e00bf3l;
    0xd5a79147l;
    0x06ca6351l;
    0x14292967l;
    0x27b70a85l;
    0x2e1b2138l;
    0x4d2c6dfcl;
    0x53380d13l;
    0x650a7354l;
    0x766a0abbl;
    0x81c2c92el;
    0x92722c85l;
    0xa2bfe8a1l;
    0xa81a664bl;
    0xc24b8b70l;
    0xc76c51a3l;
    0xd192e819l;
    0xd6990624l;
    0xf40e3585l;
    0x106aa070l;
    0x19a4c116l;
    0x1e376c08l;
    0x2748774cl;
    0x34b0bcb5l;
    0x391c0cb3l;
    0x4ed8aa4al;
    0x5b9cca4fl;
    0x682e6ff3l;
    0x748f82eel;
    0x78a5636fl;
    0x84c87814l;
    0x8cc70208l;
    0x90befffal;
    0xa4506cebl;
    0xbef9a3f7l;
    0xc67178f2l;
  |]

let rotate_right value bits =
  Int32.logor
    (Int32.shift_right_logical value bits)
    (Int32.shift_left value (32 - bits))

let choose x y z =
  Int32.logxor (Int32.logand x y) (Int32.logand (Int32.lognot x) z)

let majority x y z =
  Int32.logxor
    (Int32.logxor (Int32.logand x y) (Int32.logand x z))
    (Int32.logand y z)

let big_sigma0 x =
  Int32.logxor
    (Int32.logxor (rotate_right x 2) (rotate_right x 13))
    (rotate_right x 22)

let big_sigma1 x =
  Int32.logxor
    (Int32.logxor (rotate_right x 6) (rotate_right x 11))
    (rotate_right x 25)

let small_sigma0 x =
  Int32.logxor
    (Int32.logxor (rotate_right x 7) (rotate_right x 18))
    (Int32.shift_right_logical x 3)

let small_sigma1 x =
  Int32.logxor
    (Int32.logxor (rotate_right x 17) (rotate_right x 19))
    (Int32.shift_right_logical x 10)

let padded value =
  let length = String.length value in
  let with_marker = length + 1 in
  let zeroes = (56 - (with_marker mod 64) + 64) mod 64 in
  let result = Bytes.make (with_marker + zeroes + 8) '\000' in
  Bytes.blit_string value 0 result 0 length;
  Bytes.set result length (Char.chr 0x80);
  let bit_length = Int64.mul (Int64.of_int length) 8L in
  for index = 0 to 7 do
    let shift = (7 - index) * 8 in
    let byte =
      Int64.shift_right_logical bit_length shift
      |> Int64.logand 0xffL |> Int64.to_int
    in
    Bytes.set result (Bytes.length result - 8 + index) (Char.chr byte)
  done;
  result

let word bytes offset =
  let byte index =
    Char.code (Bytes.get bytes (offset + index)) |> Int32.of_int
  in
  Int32.logor
    (Int32.shift_left (byte 0) 24)
    (Int32.logor
       (Int32.shift_left (byte 1) 16)
       (Int32.logor (Int32.shift_left (byte 2) 8) (byte 3)))

let digest_string value =
  let bytes = padded value in
  let hash = Array.copy initial in
  let chunks = Bytes.length bytes / 64 in
  for chunk = 0 to chunks - 1 do
    let words = Array.make 64 0l in
    for index = 0 to 15 do
      words.(index) <- word bytes ((chunk * 64) + (index * 4))
    done;
    for index = 16 to 63 do
      words.(index) <-
        Int32.add
          (Int32.add
             (Int32.add words.(index - 16) (small_sigma0 words.(index - 15)))
             words.(index - 7))
          (small_sigma1 words.(index - 2))
    done;
    let a = ref hash.(0) in
    let b = ref hash.(1) in
    let c = ref hash.(2) in
    let d = ref hash.(3) in
    let e = ref hash.(4) in
    let f = ref hash.(5) in
    let g = ref hash.(6) in
    let h = ref hash.(7) in
    for index = 0 to 63 do
      let first =
        Int32.add
          (Int32.add
             (Int32.add (Int32.add !h (big_sigma1 !e)) (choose !e !f !g))
             constants.(index))
          words.(index)
      in
      let second = Int32.add (big_sigma0 !a) (majority !a !b !c) in
      h := !g;
      g := !f;
      f := !e;
      e := Int32.add !d first;
      d := !c;
      c := !b;
      b := !a;
      a := Int32.add first second
    done;
    hash.(0) <- Int32.add hash.(0) !a;
    hash.(1) <- Int32.add hash.(1) !b;
    hash.(2) <- Int32.add hash.(2) !c;
    hash.(3) <- Int32.add hash.(3) !d;
    hash.(4) <- Int32.add hash.(4) !e;
    hash.(5) <- Int32.add hash.(5) !f;
    hash.(6) <- Int32.add hash.(6) !g;
    hash.(7) <- Int32.add hash.(7) !h
  done;
  Array.to_list hash |> List.map (Printf.sprintf "%08lx") |> String.concat ""

let digest_file path =
  try
    In_channel.with_open_bin path In_channel.input_all
    |> digest_string |> Result.ok
  with Sys_error message -> Error ("could not hash scenario: " ^ message)
