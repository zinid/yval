%%%-------------------------------------------------------------------
%%% Created : 26 Sep 2018 by Evgeny Khramtsov <ekhramtsov@process-one.net>
%%%
%%% Copyright (C) 2002-2020 ProcessOne, SARL. All Rights Reserved.
%%%
%%% Licensed under the Apache License, Version 2.0 (the "License");
%%% you may not use this file except in compliance with the License.
%%% You may obtain a copy of the License at
%%%
%%%     http://www.apache.org/licenses/LICENSE-2.0
%%%
%%% Unless required by applicable law or agreed to in writing, software
%%% distributed under the License is distributed on an "AS IS" BASIS,
%%% WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
%%% See the License for the specific language governing permissions and
%%% limitations under the License.
%%%
%%%-------------------------------------------------------------------
-module(yval_test).

%% API
-import(yval, [validate/2]).
-import(yval, [format_error/2]).
%% Simple types
-import(yval, [pos_int/0, pos_int/1, non_neg_int/0, non_neg_int/1]).
-import(yval, [int/0, int/2, number/1, number/2, pos_number/0, octal/0]).
-import(yval, [binary/0, binary/1, binary/2]).
-import(yval, [string/0, string/1, string/2]).
-import(yval, [enum/1, bool/0, atom/0, any/0]).
%% Complex types
-import(yval, [url/0, url/1]).
-import(yval, [file/0, file/1]).
-import(yval, [directory/0, directory/1]).
-import(yval, [ip/0, ipv4/0, ipv6/0, ip_mask/0, port/0]).
-import(yval, [re/0, re/1, glob/0, glob/1]).
-import(yval, [path/0, binary_sep/1]).
-import(yval, [beam/0, beam/1, base64/0]).
-import(yval, [timeout/1, timeout/2]).
-import(yval, [rfc3339_time/1]).
-import(yval, [term/0, percent/0, percent/2]).
%% Composite types
-import(yval, [list/1, list/2]).
-import(yval, [list_or_single/1, list_or_single/2]).
-import(yval, [map/2, map/3]).
-import(yval, [either/2, and_then/2, non_empty/1]).
-import(yval, [options/1, options/2]).

-include_lib("eunit/include/eunit.hrl").

-define(checkError(Pattern, Expression),
	(fun() ->
		 ?assertError({yval, Pattern, _}, Expression)
	 end)()).

%%%===================================================================
%%% Tests
%%%===================================================================
any_test() ->
    lists:foreach(
      fun(T) ->
              ?assertEqual(T, v(any(), T))
      end,
      [1, [], <<"foo">>, 0.5, a, {}, fun() -> ok end, make_ref()]).

error_validate_test() ->
    ?checkError({bad_int, _}, v(int(), foo)).

enum_atom_test() ->
    ?assertEqual(foo, v(enum([foo, bar]), <<"foo">>)).

enum_binary_test() ->
    ?assertEqual(<<"foo">>, v(enum([<<"foo">>, <<"bar">>]), <<"foo">>)).

bad_enum_test() ->
    ?checkError({bad_enum, [foo, bar], baz}, (enum([foo, bar]))(<<"baz">>)).

bool_test() ->
    ?assertEqual(true, v(bool(), <<"true">>)),
    ?assertEqual(true, v(bool(), <<"on">>)),
    ?assertEqual(true, v(bool(), <<"yes">>)),
    ?assertEqual(true, v(bool(), <<"y">>)),
    ?assertEqual(false, v(bool(), <<"false">>)),
    ?assertEqual(false, v(bool(), <<"off">>)),
    ?assertEqual(false, v(bool(), <<"no">>)),
    ?assertEqual(false, v(bool(), <<"n">>)).

bad_bool_test() ->
    ?checkError({bad_bool, bad}, v(bool(), <<"bad">>)).

int_test() ->
    ?assertEqual(5, v(int(), 5)),
    ?assertEqual(0, v(int(), 0)),
    ?assertEqual(-7, v(int(), -7)).

bad_int_test() ->
    ?checkError({bad_int, _}, v(int(), <<"bad">>)).

int_range_test() ->
    ?assertEqual(5, v(int(4, 5), 5)),
    ?assertEqual(0, v(int(-1, 5), 0)),
    ?assertEqual(-10, v(int(-10, 0), -10)).

bad_int_range_test() ->
    ?checkError({bad_int, 10, 20, 5}, v(int(10, 20), 5)).

pos_int_test() ->
    ?assertEqual(1, v(pos_int(), 1)).

bad_pos_int_test() ->
    ?checkError({bad_pos_int, 0}, v(pos_int(), 0)).

pos_int_infinity_test() ->
    ?assertEqual(1, v(pos_int(infinity), 1)),
    ?assertEqual(infinite, v(pos_int(infinite), infinity)),
    ?assertEqual(unlimited, v(pos_int(unlimited), infinite)),
    ?assertEqual(infinity, v(pos_int(infinity), unlimited)).

bad_pos_int_infinity_test() ->
    ?checkError({bad_pos_int, infinity, 0}, v(pos_int(infinity), 0)),
    ?checkError({bad_int, foo}, v(pos_int(infinity), foo)),
    ?checkError({bad_int, _},
                v(pos_int(infinity),
                         list_to_binary(lists:duplicate(256, $z)))).

non_neg_int_test() ->
    ?assertEqual(0, v(non_neg_int(), 0)).

bad_non_neg_int_test() ->
    ?checkError({bad_non_neg_int, -1}, v(non_neg_int(), -1)).

non_neg_int_infinity_test() ->
    ?assertEqual(0, v(non_neg_int(infinity), 0)),
    ?assertEqual(infinite, v(non_neg_int(infinite), infinity)),
    ?assertEqual(unlimited, v(non_neg_int(unlimited), infinite)),
    ?assertEqual(infinity, v(non_neg_int(infinity), unlimited)).

bad_non_neg_int_infinity_test() ->
    ?checkError({bad_non_neg_int, infinity, -1}, v(non_neg_int(infinity), -1)).

number_test() ->
    ?assertEqual(0.5, v(number(0.5), 0.5)).

bad_number_test() ->
    ?checkError({bad_number, _}, v(number(1.0), <<"bad">>)),
    ?checkError({bad_number, 0.5, infinity, 0.4}, v(number(0.5), 0.4)).

binary_test() ->
    ?assertEqual(<<"a">>, v(binary(), <<"a">>)),
    ?assertEqual(<<"a">>, v(binary(), a)).

bad_binary_test() ->
    ?checkError({bad_binary, 1}, v(binary(), 1)).

binary_re_test() ->
    ?assertEqual(<<"foo">>, v(binary("^[a-z]+$"), <<"foo">>)),
    ?assertEqual(<<"BAR">>, v(binary("^[A-Z]+$"), <<"BAR">>)),
    ?assertEqual(<<"123">>, v(binary("^[0-9]+$"), <<"123">>)).

bad_binary_re_test() ->
    ?checkError(
       {nomatch, "^[a-z]+$", <<"fooBAR">>},
       v(binary("^[a-z]+$"), <<"fooBAR">>)).

base64_test() ->
    ?assertEqual(<<"foo">>, v(base64(), <<"Zm9v">>)).

bad_base64_test() ->
    ?checkError({bad_base64, <<"foo">>}, v(base64(), <<"foo">>)).

atom_test() ->
    ?assertEqual(atom, v(atom(), atom)),
    ?assertEqual(atom, v(atom(), <<"atom">>)).

bad_atom_test() ->
    ?checkError({bad_atom, []}, v(atom(), [])).

bad_atom_length_test() ->
    Bad = list_to_binary(lists:duplicate(256, $z)),
    ?checkError({bad_length, 255}, v(atom(), Bad)).

string_test() ->
    ?assertEqual("foo", v(string(), <<"foo">>)).

bad_string_test() ->
    ?checkError({bad_binary, []}, v(string(), [])).

string_re_test() ->
    ?assertEqual("foo", v(string("^[a-z]+$"), <<"foo">>)),
    ?assertEqual("BAR", v(string("^[A-Z]+$"), <<"BAR">>)),
    ?assertEqual("123", v(string("^[0-9]+$"), <<"123">>)).

bad_string_re_test() ->
    ?checkError(
       {nomatch, "^[a-z]+$", <<"fooBAR">>},
       v(string("^[a-z]+$"), <<"fooBAR">>)).

binary_sep_test() ->
    ?assertEqual(
       [<<"b">>, <<"c">>, <<"d">>],
       v(binary_sep("/"), <<"b/c//d//">>)).

path_test() ->
    Path = filename:join([<<"/">>, <<"foo">>, <<"bar">>, <<"baz">>]),
    ?assertMatch(Path, v(path(), Path)).

empty_path_test() ->
    ?checkError(empty_binary, v(path(), <<"">>)).

file_read_test() ->
    File = file(""),
    File = file(["a: " ++ File]),
    ?assertMatch(
       [{a, _}],
       v(File, #{a => file()})).

bad_file_read_test() ->
    File = file(["a: non_existent"]),
    ?checkError(
       {read_file, enoent, _},
       v(File, #{a => file()})).

file_write_test() ->
    File = file(""),
    File = file(["a: " ++ File]),
    ?assertMatch(
       [{a, _}],
       v(File, #{a => file(write)})).

bad_file_write_test() ->
    File = file(["a: " ++ test_dir()]),
    ?checkError(
       {create_file, eisdir, _},
       v(File, #{a => file(write)})),
    File = file(["a: " ++ filename:join(File, "foo")]),
    ?checkError(
       {create_dir, eexist, _},
       v(File, #{a => file(write)})).

directory_read_test() ->
    File = file(["a: " ++ test_dir()]),
    ?assertMatch(
       [{a, _}],
       v(File, #{a => directory()})).

bad_directory_read_test() ->
    File = file(["a: non_existent"]),
    ?checkError(
       {read_dir, enoent, _},
       v(File, #{a => directory()})).

directory_write_test() ->
    File = file(["a: " ++ test_dir()]),
    ?assertMatch(
       [{a, _}],
       v(File, #{a => directory(write)})).

bad_directory_write_test() ->
    File = file(""),
    File = file(["a: " ++ File]),
    ?checkError(
       {create_dir, eexist, _},
       v(File, #{a => directory(write)})).

url_test() ->
    ?assertEqual(<<"http://domain.tld">>, v(url(), <<"http://domain.tld">>)),
    ?assertEqual(<<"https://domain.tld">>, v(url(), <<"https://domain.tld">>)).

url_any_test() ->
    ?assertEqual(
       <<"wss://domain.tld:8443">>, v(url([]), <<"wss://domain.tld:8443">>)).

bad_url_scheme_test() ->
    ?checkError(
       {bad_url, {unsupported_scheme, http}, <<"http://domain.tld">>},
       v(url([https]), <<"http://domain.tld">>)).

bad_url_host_test() ->
    ?checkError(
       {bad_url, empty_host, <<"http:///path">>}, v(url(), <<"http:///path">>)).

bad_url_no_default_port_test() ->
    ?checkError(
       {bad_url, {no_default_port, foo, _}, <<"foo://domain.tld">>},
       v(url([]), <<"foo://domain.tld">>)).

bad_url_bad_port_test() ->
    ?checkError(
       {bad_url, bad_port, <<"http://domain.tld:0">>},
       v(url(), <<"http://domain.tld:0">>)),
    ?checkError(
       {bad_url, bad_port, <<"http://domain.tld:-1">>},
       v(url(), <<"http://domain.tld:-1">>)),
    ?checkError(
       {bad_url, bad_port, <<"http://domain.tld:65536">>},
       v(url(), <<"http://domain.tld:65536">>)).

bad_url_test() ->
    ?checkError({bad_url, _, <<"bad">>}, v(url(), <<"bad">>)).

octal_test() ->
    File = file(["a: \"644\""]),
    ?assertEqual(
       [{a, 420}],
       v(File, #{a => octal()})).

bad_octal_test() ->
    File = file(["a: \"9\""]),
    ?checkError(
       {bad_octal, <<"9">>},
       v(File, #{a => octal()})).

ipv4_test() ->
    File = file(["a: 127.0.0.1"]),
    ?assertEqual(
       [{a, {127,0,0,1}}],
       v(File, #{a => ipv4()})).

bad_ipv4_test() ->
    File = file(["a: '::1'"]),
    ?checkError(
       {bad_ipv4, "::1"},
       v(File, #{a => ipv4()})).

ipv6_test() ->
    File = file(["a: '::1'"]),
    ?assertEqual(
       [{a, {0,0,0,0,0,0,0,1}}],
       v(File, #{a => ipv6()})).

bad_ipv6_test() ->
    File = file(["a: 127.0.0.1"]),
    ?checkError(
       {bad_ipv6, "127.0.0.1"},
       v(File, #{a => ipv6()})).

ip_test() ->
    File = file(["a: 127.0.0.1",
		 "b: '::1'"]),
    ?assertEqual(
       [{a, {127,0,0,1}}, {b, {0,0,0,0,0,0,0,1}}],
       v(File, #{a => ip(), b => ip()})).

bad_ip_test() ->
    File = file(["a: bad"]),
    ?checkError(
       {bad_ip, "bad"},
       v(File, #{a => ip()})).

ip_mask_test() ->
    File = file(["a: 127.0.0.1",
		 "b: 127.0.0.1/0",
		 "c: 127.0.0.1/32",
		 "d: '::1'",
		 "e: '::1/0'",
		 "f: '::1/128'"]),
    ?assertEqual(
       [{a, {{127,0,0,1}, 32}},
        {b, {{127,0,0,1}, 0}},
        {c, {{127,0,0,1}, 32}},
        {d, {{0,0,0,0,0,0,0,1}, 128}},
        {e, {{0,0,0,0,0,0,0,1}, 0}},
        {f, {{0,0,0,0,0,0,0,1}, 128}}],
       v(File, #{a => ip_mask(),
			   b => ip_mask(),
			   c => ip_mask(),
			   d => ip_mask(),
			   e => ip_mask(),
			   f => ip_mask()})).

bad_ip_mask_test() ->
    File = file(["a: 127.0.0.1/128"]),
    ?checkError(
       {bad_ip_mask, "127.0.0.1/128"},
       v(File, #{a => ip_mask()})).

port_test() ->
    File = file(["a: 1",
		 "b: 65535"]),
    ?assertEqual(
       [{a, 1}, {b, 65535}],
       v(File, #{a => port(), b => port()})).

timeout_test() ->
    File = file(["millisecond: 1",
		 "second: 1",
		 "minute: 1",
		 "hour: 1",
		 "day: 1"]),
    ?assertEqual(
       [{millisecond, 1},
        {second, 1000},
        {minute, 60000},
        {hour, 3600000},
        {day, 86400000}],
       v(File, #{millisecond => timeout(millisecond),
			   second => timeout(second),
			   minute => timeout(minute),
			   hour => timeout(hour),
			   day => timeout(day)})).

timeout_atom_test() ->
    ?assertEqual(5, v(timeout(millisecond), '5')).

timeout_format_test() ->
    File = file(["ms: 1 ms",
		 "msec: 1 msec",
		 "msecs: 1 msecs",
		 "millisec: 1 millisec",
		 "millisecs: 1 millisecs",
		 "millisecond: 1 millisecond",
		 "s: 1 s",
		 "sec: 1 sec",
		 "secs: 1 secs",
		 "second: 1 second",
		 "seconds: 1 seconds",
		 "m: 1 m",
		 "min: 1 min",
		 "mins: 1 mins",
		 "minute: 1 minute",
		 "minutes: 1 minutes",
		 "h: 1 h",
		 "hour: 1 hour",
		 "hours: 1 hours",
		 "d: 1 d",
		 "day: 1 day",
		 "days: 1 days"]),
    ?assertEqual(
       [{ms,1},
        {msec,1},
        {msecs,1},
        {millisec,1},
        {millisecs,1},
        {millisecond,1},
        {s,1000},
        {sec,1000},
        {secs,1000},
        {second,1000},
        {seconds,1000},
        {m,60000},
        {min,60000},
        {mins,60000},
        {minute,60000},
        {minutes,60000},
        {h,3600000},
        {hour,3600000},
        {hours,3600000},
        {d,86400000},
        {day,86400000},
        {days,86400000}],
       v(File, #{'_' => timeout(millisecond)})).

timeout_infinity_test() ->
    File = file(["a: infinity",
		 "b: infinite",
		 "c: unlimited"]),
    ?assertEqual(
       [{a, infinite}, {b, unlimited}, {c, infinity}],
       v(File, #{a => timeout(day, infinite),
			   b => timeout(day, unlimited),
			   c => timeout(day, infinity)})).

bad_timeout_test() ->
    File = file(["a: []"]),
    ?checkError(
       {bad_timeout, []},
       v(File, #{a => timeout(second)})),
    ?checkError(
       {bad_timeout, infinity, []},
       v(File, #{a => timeout(second, infinity)})).

bad_timeout_zero_test() ->
    File = file(["a: 0"]),
    ?checkError(
       {bad_pos_int, 0},
       v(File, #{a => timeout(second)})),
    ?checkError(
       {bad_pos_int, infinity, 0},
       v(File, #{a => timeout(second, infinity)})).

bad_timeout_infinity_test() ->
    File = file(["a: foo"]),
    ?checkError(
       {bad_int, <<"foo">>},
       v(File, #{a => timeout(second)})),
    ?checkError(
       {bad_enum, _, foo},
       v(File, #{a => timeout(second, infinity)})).

bad_timeout_unit_test() ->
    File = file(["a: 1foo"]),
    ?checkError(
       {bad_timeout_unit, "foo"},
       v(File, #{a => timeout(second)})).

bad_timeout_min_test() ->
    File = file(["a: 1ms"]),
    ?checkError(
       {bad_timeout_min, second},
       v(File, #{a => timeout(second)})).

bad_timeout_negative_test() ->
    File = file(["a: -1s"]),
    ?checkError(
       {bad_pos_int, -1},
       v(File, #{a => timeout(second)})),
    ?checkError(
       {bad_pos_int, infinity, -1},
       v(File, #{a => timeout(second, infinity)})).

re_test() ->
    File = file(["a: ^[0-9]+$"]),
    ?assertMatch(
       [{a, _}],
       v(File, #{a => re()})).

bad_re_test() ->
    File = file(["a: '['"]),
    ?checkError(
       {bad_regexp, {_, _}, _},
       v(File, #{a => re()})).

glob_test() ->
    File = file(["a: '*'"]),
    ?assertMatch(
       [{a, _}],
       v(File, #{a => glob()})).

bad_glob_test() ->
    File = file(["a: '['"]),
    ?checkError(
       {bad_glob, {_, _}, _},
       v(File, #{a => glob()})).

beam_test() ->
    Exports = [[{foo, 1}, {parse, 2}], {parse, 3}, []],
    File = file(["a: yconf"]),
    ?assertMatch(
       [{a, yconf}],
       v(File, #{a => beam(Exports)})).

bad_beam_test() ->
    File = file(["a: foo"]),
    ?checkError(
       {bad_module, foo},
       v(File, #{a => beam()})),
    File = file(["a: yconf"]),
    ?checkError(
       {bad_export, {foo, 1}, yconf},
       v(File, #{a => beam([[{foo, 1}, {bar, 2}]])})),
    ?checkError(
       {bad_export, {foo, 1}, yconf},
       v(File, #{a => beam([{foo, 1}])})).

non_empty_test() ->
    File = file(["a: [1,2,3]",
		 "b: 1",
		 "c: foo",
		 "d: {e: f}"]),
    ?assertMatch(
       [{a, [1,2,3]}, {b, 1}, {c, foo}, {d, [_]}],
       v(File, #{a => non_empty(list(int())),
			   b => non_empty(int()),
			   c => non_empty(atom()),
			   d => non_empty(map(any(), any()))})).

empty_atom_test() ->
    File = file(["a: ''"]),
    ?checkError(
       empty_atom,
       v(File, #{a => non_empty(atom())})).

empty_binary_test() ->
    File = file(["a: ''"]),
    ?checkError(
       empty_binary,
       v(File, #{a => non_empty(binary())})).

empty_list_test() ->
    File = file(["a: []"]),
    ?checkError(
       empty_list,
       v(File, #{a => non_empty(list(any()))})).

empty_map_test() ->
    File = file(["a: {}"]),
    ?checkError(
       empty_list,
       v(File, #{a => non_empty(
				  map(any(), any()))})).

list_test() ->
    File = file(["a: [1,2,3]"]),
    ?assertMatch(
       [{a, [1,2,3]}],
       v(File, #{a => list(any())})).

bad_list_test() ->
    File = file(["a: 1"]),
    ?checkError(
       {bad_list, 1},
       v(File, #{a => list(any())})).

sorted_list_test() ->
    File = file(["a: [3,2,1]"]),
    ?assertMatch(
       [{a, [1,2,3]}],
       v(File, #{a => list(any(), [sorted])})).

bad_sorted_list_test() ->
    File = file(["a: 1"]),
    ?checkError(
       {bad_list, 1},
       v(File, #{a => list(any(), [sorted])})).

unique_list_test() ->
    File = file(["a: [1,2,3]"]),
    ?assertMatch(
       [{a, [1,2,3]}],
       v(File, #{a => list(any(), [unique])})).

bad_unique_list_test() ->
    File = file(["a: [1,2,1,3]"]),
    ?checkError(
       {duplicated_value, 1},
       v(File, #{a => list(any(), [unique])})),
    File = file(["a: [foo, bar, foo]"]),
    ?checkError(
       {duplicated_value, foo},
       v(File, #{a => list(atom(), [unique])})),
    File = file(["a: [[1], [2], [1]]"]),
    ?checkError(
       {duplicated_value, [1]},
       v(File, #{a => list(any(), [unique])})).

list_or_single_test() ->
    File = file(["a: 1",
		 "b: [1,2,3]"]),
    ?assertMatch(
       [{a, [1]}, {b, [1,2,3]}],
       v(File, #{a => list_or_single(any()),
			   b => list_or_single(any())})).

sorted_list_or_single_test() ->
    File = file(["a: 1",
		 "b: [3,2,1]"]),
    ?assertMatch(
       [{a, [1]}, {b, [1,2,3]}],
       v(File, #{a => list_or_single(any(), [sorted]),
			   b => list_or_single(any(), [sorted])})).

unique_list_or_single_test() ->
    File = file(["a: 1",
		 "b: [1,2,3]"]),
    ?assertMatch(
       [{a, [1]}, {b, [1,2,3]}],
       v(File, #{a => list_or_single(any(), [unique]),
			   b => list_or_single(any(), [unique])})).

bad_unique_list_or_single_test() ->
    File = file(["a: 1",
		 "b: [1,2,1,3]"]),
    ?checkError(
       {duplicated_value, 1},
       v(File, #{a => list_or_single(any(), [unique]),
			   b => list_or_single(any(), [unique])})).

map_test() ->
    File = file(["a: {c: 2, b: 1}"]),
    ?assertEqual(
       [{a, [{c, 2}, {b, 1}]}],
       v(File, #{a => map(atom(), any())})),
    ?assertEqual(
       [{a, [{c, 2}, {b, 1}]}],
       v(File, #{a => map(atom(), any(),
					  [unique])})),
    ?assertEqual(
       [{a, [{b, 1}, {c, 2}]}],
       v(File, #{a => map(atom(), any(),
					  [{return, orddict}])})),
    ?assertEqual(
       [{a, #{b => 1, c => 2}}],
       v(File, #{a => map(atom(), any(),
					  [{return, map}])})),
    Ret = v(File, #{a => map(atom(), any(),
					     [{return, dict}])}),
    ?assertMatch([{a, _}], Ret),
    ?assertEqual(
       [{b, 1}, {c, 2}],
       lists:keysort(1, dict:to_list(element(2, hd(element(2, Ret)))))).

bad_map_test() ->
    V = map(atom(), any()),
    File = file(["a: 1"]),
    ?checkError(
       {bad_map, 1},
       v(File, #{a => V})),
    File = file(["a: [1,2,3]"]),
    ?checkError(
       {bad_map, [1,2,3]},
       v(File, #{a => V})).

bad_unique_map_test() ->
    File = file(["a: {c: 2, b: 1, c: 3}"]),
    ?checkError(
       {duplicated_key, c},
       v(File, #{a => map(atom(), any(),
					  [unique])})).

either_test() ->
    V = either(bool(), int()),
    File = file(["a: true",
		 "b: 5"]),
    ?assertEqual(
       [{a, true}, {b, 5}],
       v(File, #{a => V, b => V})).

either_atom_test() ->
    V = either(atom, int()),
    File = file(["a: atom",
		 "b: 1"]),
    ?assertEqual(
       [{a, atom}, {b, 1}],
       v(File, #{a => V, b => V})).

and_then_test() ->
    V = and_then(
	  list(int()),
	  fun lists:sum/1),
    File = file(["a: [1,2,3]"]),
    ?assertEqual(
       [{a, 6}],
       v(File, #{a => V})).

options_test() ->
    File = file(["a: {b: 1, c: true}"]),
    ?assertEqual(
       [{a, [{b, 1}, {c, true}]}],
       v(File, #{a => options(
				  #{b => int(),
				    c => bool(),
				    d => atom()})})).

options_return_map_test() ->
    ?assertEqual(
       #{a => 1, b => 2},
       v(options(#{a => any(), b => any()}, [{return, map}]),
         [{<<"a">>, 1}, {<<"b">>, 2}])).

options_return_dict_test() ->
    Ret = v(options(#{a => any(), b => any()}, [{return, dict}]),
            [{<<"a">>, 1}, {<<"b">>, 2}]),
    ?assertEqual(
       [{a, 1}, {b, 2}],
       lists:keysort(1, dict:to_list(element(2, Ret)))).

options_return_orddict_test() ->
    ?assertEqual(
       [{a, 2}, {b, 1}],
       v(options(#{a => any(), b => any()}, [{return, orddict}]),
                [{<<"b">>, 1}, {<<"a">>, 2}])).

options_default_validator_test() ->
    File = file(["a: {b: 1, c: true}"]),
    ?assertEqual(
       [{a, [{b, 1}, {c, true}]}],
       v(File, #{a => options(
				  #{b => int(),
				    '_' => bool()})})).

bad_options_test() ->
    File = file(["a: 1"]),
    ?checkError(
       {bad_map, 1},
       v(File, #{a => options(#{})})),
    File = file(["a: [1,2,3]"]),
    ?checkError(
       {bad_map, [1,2,3]},
       v(File, #{a => options(#{})})).

bad_binary_map_option_test() ->
    File = file(["a: {b: foo}"]),
    ?checkError(
       {bad_bool, foo},
       v(File, #{a => map(binary(), bool())})).

bad_integer_map_option_test() ->
    File = file(["a: {1: foo}"]),
    ?checkError(
       {bad_bool, foo},
       v(File, #{a => map(int(), bool())})).

unknown_option_test() ->
    ?checkError({unknown_option, [], a}, v(options(#{}), [{<<"a">>, 1}])).

missing_option_test() ->
    ?checkError(
       {missing_option, b},
       v(options(#{a => int(), b => any()}, [{required, [b]}]),
                [{<<"a">>, 1}])).

disallowed_option_test() ->
    Y = [{<<"a">>, 1}, {<<"b">>, 2}],
    ?checkError(
       {disallowed_option, b},
       v(options(#{a => int()}, [{disallowed, [b]}]), Y)),
    ?checkError(
       {disallowed_option, b},
       v(options(#{a => int(), b => int()}, [{disallowed, [b]}]), Y)),
    ?checkError(
       {disallowed_option, b},
       v(options(#{a => int(), b => int()}, [{required, [b]}, {disallowed, [b]}]), Y)).

unknown_option_with_disallowed_test() ->
    ?checkError(
       {unknown_option, [a], c},
       v(options(#{a => int(), b => int()}, [{disallowed, [b]}]),
                [{<<"a">>, 1}, {<<"c">>, 2}])).

duplicated_option_test() ->
    Y = [{<<"a">>, 1}, {<<"b">>, 2}, {<<"a">>, 3}],
    ?checkError(
       {duplicated_option, a},
       v(options(#{a => int(), b => int()}, [unique]), Y)),
    ?assertEqual(
       [{a, 1}, {b, 2}, {a, 3}],
       v(options(#{a => int(), b => int()}, []), Y)).

duplicated_unspecified_option_test() ->
    ?checkError(
       {duplicated_option, b},
       v(options(#{a => int(), '_' => any()}, [unique]),
                [{<<"a">>, 1}, {<<"b">>, 2}, {<<"b">>, 3}])).

bad_cwd_test() ->
    test_format_error({error, {bad_cwd, eaccess}, []}).

unknown_reason_test() ->
    test_format_error({error, foo, []}).

unicode_test() ->
    UTF8CharList = [209, 134],
    UTF8CharBin = list_to_binary(UTF8CharList),
    UTF8CharAtom = list_to_atom(UTF8CharList),
    ?assertEqual(
       [{a, UTF8CharAtom}, {b, UTF8CharBin}],
       v(options(#{a => atom(), b => binary()}),
                [{<<"a">>, UTF8CharBin}, {<<"b">>, UTF8CharBin}])).

%%%===================================================================
%%% Internal functions
%%%===================================================================
test_dir() ->
    {ok, Cwd} = file:get_cwd(),
    filename:join(filename:dirname(Cwd), "test").

test_format_error({error, Why, Ctx}) ->
    ?assertMatch([_|_], format_error(Why, Ctx)).

v(Fun, Arg) ->
    apply(Fun, [Arg]).
