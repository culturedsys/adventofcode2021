open OUnit2
open Adventofcode2021.Day16

let suite1 = "Part 1" >::: [
  "can decode hex" >:: (fun _ ->
    assert_equal [true; false; true; false; false; true; true; true] (binary_of_hex "A7") 
  );
  "can parse literal" >:: (fun _ ->
    let input = binary_of_hex "D2FE28" in
    let (result, _, _) = parse_packet input in
    assert_equal result (Literal { version = 6; value = 2021 })
  );
  "can parse operation" >:: (fun _ ->
    let input = binary_of_hex "38006F45291200" in
    let (result, _, _) = parse_packet input in
    assert_equal result (Operation {
      version = 1;
      operation_id = LessThan;
      contents = [
        Literal {
          version = 6;
          value = 10;
        };
        Literal {
          version = 2;
          value = 20
        }
      ]
    })
  );
  "can sum version" >:: (fun _ ->
    let input = binary_of_hex "A0016C880162017C3686B18A3D4780" in
    let (packet, _, _) =  parse_packet input in
    assert_equal 31 (sum_versions packet)
  )
]

let suite2 = "Part 2" >::: [
  "can evaluate" >:: (fun _ ->
    let input = binary_of_hex "9C0141080250320F1802104A08" in
    let (packet, _, _) = parse_packet input in
    assert_equal 1 (evaluate packet)  
  )
]

let () = run_test_tt_main suite1 ; run_test_tt_main suite2