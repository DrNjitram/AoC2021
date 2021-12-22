-module(day16_test).
-include_lib("eunit/include/eunit.hrl").

part1_test() ->
    ?assertEqual(16, day16:part1(["8A004A801A8002F478"])),
    ?assertEqual(12, day16:part1(["620080001611562C8802118E34"])),
    ?assertEqual(23, day16:part1(["C0015000016115A2E0802F182340"])),
    ?assertEqual(31, day16:part1(["A0016C880162017C3686B18A3D4780"])),
    ok.

part2_test() ->
    ?assertEqual(3, day16:part2(["C200B40A82"])),
    ?assertEqual(54, day16:part2(["04005AC33890"])),
    ?assertEqual(7, day16:part2(["880086C3E88112"])),
    ?assertEqual(9, day16:part2(["CE00C43D881120"])),
    ?assertEqual(1, day16:part2(["D8005AC2A8F0"])),
    ?assertEqual(0, day16:part2(["F600BC2D8F"])),
    ?assertEqual(0, day16:part2(["9C005AC2F8F0"])),
    ?assertEqual(1, day16:part2(["9C0141080250320F1802104A08"])),
    ok.