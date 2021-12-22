-module(day16).

-export([part1/1, part2/1]).


-record(packet, {
        version,
        type,
        value,
        sub_packets=[]
    }).

parse(Binary) -> parse(Binary, []).

parse(<<V:3, 4:3, Binary/bits>>, Acc) -> parse(<<Binary/bits>>, #packet{version = V, type=4}, Acc, <<>>);
parse(<<V:3, T:3, 0:1, Length:15, Payload:Length/bits, Binary/bits>>, Acc) -> 
    parse(Binary, Acc ++ [#packet{version=V, type=T, sub_packets=parse(Payload)}]);

parse(<<V:3, T:3, 1:1, Packets:11/integer, Binary/bits>>, Acc) -> 
    {Sub, Rest} = lists:split(Packets, parse(Binary)),
    parse(done, Acc ++ [ #packet{version=V, type=T, sub_packets=Sub}] ++  Rest);

parse(_, Acc) -> Acc.

parse(<<1:1, V:4/bits, Binary/bits>>, Packet, Acc, VAcc) -> parse(Binary, Packet, Acc, <<VAcc/bits, V/bits>>);
parse(<<0:1, V:4/bits, Binary/bits>>, Packet, Acc, VAcc) -> 
    VC = <<VAcc/bits, V/bits>>,
    <<Value:(bit_size(VC))/integer>> = VC,
    parse(Binary, Acc ++ [Packet#packet{value=Value}]).

answer(Packets) -> answer(Packets, 0).
answer([], Acc) -> Acc;
answer([Packet|Packets], Acc) -> answer(Packets, Acc + Packet#packet.version) + answer(Packet#packet.sub_packets).

parse_packets(Packets) -> parse_packets(Packets, []).
parse_packets([#packet{type=0, sub_packets=Sub}|Packets], Acc) -> parse_packets(Packets, Acc ++ [lists:sum(parse_packets(Sub))]);
parse_packets([#packet{type=1, sub_packets=Sub}|Packets], Acc) -> parse_packets(Packets, Acc ++ [lists:foldl(fun(V, Prod) -> V * Prod end, 1, parse_packets(Sub))]);
parse_packets([#packet{type=2, sub_packets=Sub}|Packets], Acc) -> parse_packets(Packets, Acc ++ [lists:min(parse_packets(Sub))]);
parse_packets([#packet{type=3, sub_packets=Sub}|Packets], Acc) -> parse_packets(Packets, Acc ++ [lists:max(parse_packets(Sub))]);
parse_packets([#packet{type=4, value=Value}    |Packets], Acc) -> parse_packets(Packets, Acc ++ [Value]);
parse_packets([#packet{type=5, sub_packets=Sub}|Packets], Acc) -> parse_packets(Packets, Acc ++ [gt(parse_packets(Sub))]);
parse_packets([#packet{type=6, sub_packets=Sub}|Packets], Acc) -> parse_packets(Packets, Acc ++ [lt(parse_packets(Sub))]);
parse_packets([#packet{type=7, sub_packets=Sub}|Packets], Acc) -> parse_packets(Packets, Acc ++ [e(parse_packets(Sub))]);
parse_packets(_, Acc) -> Acc.

gt([V1, V2]) when V1 > V2 -> 1;
gt(_) -> 0.
lt([V1, V2]) when V1 < V2 -> 1;
lt(_) -> 0.
e([V1, V2]) when V1 == V2 -> 1;
e(_) -> 0.


part1([Lines]) ->
    Binary = binary:decode_hex(list_to_binary(Lines)),
    Packets = parse(Binary),
    answer(Packets).


part2(Lines) ->
    Binary = binary:decode_hex(list_to_binary(Lines)),
    Packets = parse(Binary),
    hd(parse_packets(Packets)).
    