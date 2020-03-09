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
-include_lib("eunit/include/eunit.hrl").

-define(checkError(Pattern, Expression),
	(fun() ->
		 Ret = Expression,
		 ?assertMatch({error, Pattern, _}, Ret),
		 test_format_error(Ret)
	 end)()).

%%%===================================================================
%%% Tests
%%%===================================================================
start_test() ->
    ?assertEqual(ok, yval:start()).

any_test() ->
    ?assertEqual(
       {ok, 1},
       yval:validate(yval:any(), 1)).

error_validate_test() ->
    ?checkError(
       {bad_int, _},
       yval:validate(yval:int(), foo)).

empty_yaml_test() ->
    ?assertEqual(
       {ok, []},
       yval:validate(yval:options(#{}), [])).

enum_atom_test() ->
    ?assertEqual(
       {ok, foo},
       yval:validate(yval:enum([foo, bar]), <<"foo">>)).

enum_binary_test() ->
    ?assertEqual(
       {ok, <<"foo">>},
       yval:validate(yval:enum([<<"foo">>, <<"bar">>]), <<"foo">>)).

bad_enum_test() ->
    ?checkError(
       {bad_enum, [foo, bar], baz},
       yval:validate(yval:enum([foo, bar]), <<"baz">>)).

bool_test() ->
    Y = [{<<"a">>, <<"true">>},
         {<<"b">>, <<"false">>},
         {<<"c">>, <<"on">>},
         {<<"d">>, <<"off">>},
         {<<"e">>, <<"yes">>},
         {<<"f">>, <<"no">>},
         {<<"g">>, <<"y">>},
         {<<"h">>, <<"n">>}],
    ?assertEqual(
       {ok, [{a, true}, {b, false}, {c, true}, {d, false},
	     {e, true}, {f, false}, {g, true}, {h, false}]},
       yval:validate(
         yval:options(
           #{a => yval:bool(),
             b => yval:bool(),
             c => yval:bool(),
             d => yval:bool(),
             e => yval:bool(),
             f => yval:bool(),
             g => yval:bool(),
             h => yval:bool()}),
         Y)).

bad_bool_test() ->
    ?checkError(
       {bad_bool, bad},
       yval:validate(yval:bool(), <<"bad">>)).

int_test() ->
    Y = [{<<"a">>, 5},
         {<<"b">>, 0},
         {<<"c">>, -7}],
    ?assertEqual(
       {ok, [{a, 5}, {b, 0}, {c, -7}]},
       yval:validate(
         yval:options(#{a => yval:int(),
                        b => yval:int(),
                        c => yval:int()}),
         Y)).

bad_int_test() ->
    ?checkError(
       {bad_int, _},
       yval:validate(yval:int(), <<"bad">>)).

int_range_test() ->
    File = file(["a: 5",
		 "b: 0",
		 "c: -10"]),
    ?assertEqual(
       {ok, [{a, 5}, {b, 0}, {c, -10}]},
       yval:validate(File, #{a => yval:int(4, 5),
			   b => yval:int(-1, 5),
			   c => yval:int(-10, 0)})).

bad_int_range_test() ->
    File = file(["a: 5"]),
    ?checkError(
       {bad_int, 10, 20, 5},
       yval:validate(File, #{a => yval:int(10, 20)})).

pos_int_test() ->
    File = file(["a: 1"]),
    ?assertEqual(
       {ok, [{a, 1}]},
       yval:validate(File, #{a => yval:pos_int()})).

bad_pos_int_test() ->
    File = file(["a: 0"]),
    ?checkError(
       {bad_pos_int, 0},
       yval:validate(File, #{a => yval:pos_int()})).

pos_int_infinity_test() ->
    File = file(["a: 1",
		 "b: infinity",
		 "c: infinite",
		 "d: unlimited"]),
    ?assertEqual(
       {ok, [{a, 1}, {b, infinite}, {c, unlimited}, {d, infinity}]},
       yval:validate(File, #{a => yval:pos_int(infinity),
			   b => yval:pos_int(infinite),
			   c => yval:pos_int(unlimited),
			   d => yval:pos_int(infinity)})).

bad_pos_int_infinity_test() ->
    File = file(["a: 0"]),
    ?checkError(
       {bad_pos_int, infinity, 0},
       yval:validate(File, #{a => yval:pos_int(infinity)})),
    ?checkError(
       {bad_int, foo},
       yval:validate(yval:pos_int(infinity), foo)),
    ?checkError(
       {bad_int, _},
       yval:validate(
	 yval:pos_int(infinity),
	 list_to_binary(lists:duplicate(256, $z)))).

non_neg_int_test() ->
    File = file(["a: 0"]),
    ?assertEqual(
       {ok, [{a, 0}]},
       yval:validate(File, #{a => yval:non_neg_int()})).

bad_non_neg_int_test() ->
    File = file(["a: -1"]),
    ?checkError(
       {bad_non_neg_int, -1},
       yval:validate(File, #{a => yval:non_neg_int()})).

non_neg_int_infinity_test() ->
    File = file(["a: 0",
		 "b: infinity",
		 "c: infinite",
		 "d: unlimited"]),
    ?assertEqual(
       {ok, [{a, 0}, {b, infinite}, {c, unlimited}, {d, infinity}]},
       yval:validate(File, #{a => yval:non_neg_int(infinity),
			   b => yval:non_neg_int(infinite),
			   c => yval:non_neg_int(unlimited),
			   d => yval:non_neg_int(infinity)})).

bad_non_neg_int_infinity_test() ->
    File = file(["a: -1"]),
    ?checkError(
       {bad_non_neg_int, infinity, -1},
       yval:validate(File, #{a => yval:non_neg_int(infinity)})).

number_test() ->
    File = file(["a: 0.5"]),
    ?assertEqual(
       {ok, [{a, 0.5}]},
       yval:validate(File, #{a => yval:number(0.5)})).

bad_number_test() ->
    File = file(["a: bad"]),
    ?checkError(
       {bad_number, _},
       yval:validate(File, #{a => yval:number(1.0)})),
    File = file(["a: 0.4"]),
    ?checkError(
       {bad_number, 0.5, 0.4},
       yval:validate(File, #{a => yval:number(0.5)})).

binary_test() ->
    File = file(["a: foo",
		 "b: \"bar\"",
		 "c: 'baz'"]),
    ?assertEqual(
       {ok, [{a, <<"foo">>}, {b, <<"bar">>}, {c, <<"baz">>}]},
       yval:validate(File, #{a => yval:binary(),
			   b => yval:binary(),
			   c => yval:binary()})),
    ?assertEqual(<<"foo">>, (yval:binary())(foo)).

bad_binary_test() ->
    File = file(["a: 1"]),
    ?checkError(
       {bad_binary, 1},
       yval:validate(File, #{a => yval:binary()})).

binary_re_test() ->
    File = file(["a: foo",
		 "b: BAR",
		 "c: \"123\""]),
    ?assertEqual(
       {ok, [{a, <<"foo">>}, {b, <<"BAR">>}, {c, <<"123">>}]},
       yval:validate(File, #{a => yval:binary("^[a-z]+$"),
			   b => yval:binary("^[A-Z]+$"),
			   c => yval:binary("^[0-9]+$")})).

bad_binary_re_test() ->
    File = file(["a: fooBAR"]),
    ?checkError(
       {nomatch, "^[a-z]+$", <<"fooBAR">>},
       yval:validate(File, #{a => yval:binary("^[a-z]+$")})).

base64_test() ->
    File = file(["a: Zm9v"]),
    ?assertEqual(
       {ok, [{a, <<"foo">>}]},
       yval:validate(File, #{a => yval:base64()})).

bad_base64_test() ->
    File = file(["a: foo"]),
    ?checkError(
       {bad_base64, <<"foo">>},
       yval:validate(File, #{a => yval:base64()})).

atom_test() ->
    File = file(["a: atom"]),
    ?assertEqual(
       {ok, [{a, atom}]},
       yval:validate(File, #{a => yval:atom()})).

bad_atom_test() ->
    File = file(["a: []"]),
    ?checkError(
       {bad_atom, []},
       yval:validate(File, #{a => yval:atom()})).

bad_atom_length_test() ->
    Bad = list_to_binary(lists:duplicate(256, $z)),
    ?checkError(
       {bad_length, 255},
       yval:validate(yval:atom(), Bad)).

string_test() ->
    File = file(["a: foo"]),
    ?assertEqual(
       {ok, [{a, "foo"}]},
       yval:validate(File, #{a => yval:string()})).

bad_string_test() ->
    File = file(["a: []"]),
    ?checkError(
       {bad_binary, []},
       yval:validate(File, #{a => yval:string()})).

string_re_test() ->
    File = file(["a: foo",
		 "b: BAR",
		 "c: \"123\""]),
    ?assertEqual(
       {ok, [{a, "foo"}, {b, "BAR"}, {c, "123"}]},
       yval:validate(File, #{a => yval:string("^[a-z]+$"),
			   b => yval:string("^[A-Z]+$"),
			   c => yval:string("^[0-9]+$")})).

bad_string_re_test() ->
    File = file(["a: fooBAR"]),
    ?checkError(
       {nomatch, "^[a-z]+$", "fooBAR"},
       yval:validate(File, #{a => yval:string("^[a-z]+$")})).

binary_sep_test() ->
    File = file(["a: b/c//d//"]),
    ?assertEqual(
       {ok, [{a, [<<"b">>, <<"c">>, <<"d">>]}]},
       yval:validate(File, #{a => yval:binary_sep("/")})).

path_test() ->
    File = file(["a: foo"]),
    ?assertMatch(
       {ok, [{a, _}]},
       yval:validate(File, #{a => yval:path()})).

empty_path_test() ->
    File = file(["a: ''"]),
    ?checkError(
       empty_binary,
       yval:validate(File, #{a => yval:path()})).

file_read_test() ->
    File = file(""),
    File = file(["a: " ++ File]),
    ?assertMatch(
       {ok, [{a, _}]},
       yval:validate(File, #{a => yval:file()})).

bad_file_read_test() ->
    File = file(["a: non_existent"]),
    ?checkError(
       {read_file, enoent, _},
       yval:validate(File, #{a => yval:file()})).

file_write_test() ->
    File = file(""),
    File = file(["a: " ++ File]),
    ?assertMatch(
       {ok, [{a, _}]},
       yval:validate(File, #{a => yval:file(write)})).

bad_file_write_test() ->
    File = file(["a: " ++ test_dir()]),
    ?checkError(
       {create_file, eisdir, _},
       yval:validate(File, #{a => yval:file(write)})),
    File = file(["a: " ++ filename:join(File, "foo")]),
    ?checkError(
       {create_dir, eexist, _},
       yval:validate(File, #{a => yval:file(write)})).

directory_read_test() ->
    File = file(["a: " ++ test_dir()]),
    ?assertMatch(
       {ok, [{a, _}]},
       yval:validate(File, #{a => yval:directory()})).

bad_directory_read_test() ->
    File = file(["a: non_existent"]),
    ?checkError(
       {read_dir, enoent, _},
       yval:validate(File, #{a => yval:directory()})).

directory_write_test() ->
    File = file(["a: " ++ test_dir()]),
    ?assertMatch(
       {ok, [{a, _}]},
       yval:validate(File, #{a => yval:directory(write)})).

bad_directory_write_test() ->
    File = file(""),
    File = file(["a: " ++ File]),
    ?checkError(
       {create_dir, eexist, _},
       yval:validate(File, #{a => yval:directory(write)})).

url_test() ->
    File = file(["a: http://domain.tld",
		 "b: https://domain.tld"]),
    ?assertEqual(
       {ok, [{a, <<"http://domain.tld">>}, {b, <<"https://domain.tld">>}]},
       yval:validate(File, #{a => yval:url(), b => yval:url()})).

url_any_test() ->
    File = file(["a: wss://domain.tld:8443"]),
    ?assertEqual(
       {ok, [{a, <<"wss://domain.tld:8443">>}]},
       yval:validate(File, #{a => yval:url([])})).

bad_url_scheme_test() ->
    File = file(["a: http://domain.tld"]),
    ?checkError(
       {bad_url, {unsupported_scheme, http}, <<"http://domain.tld">>},
       yval:validate(File, #{a => yval:url([https])})).

bad_url_host_test() ->
    File = file(["a: http:///path"]),
    ?checkError(
       {bad_url, empty_host, <<"http:///path">>},
       yval:validate(File, #{a => yval:url()})).

bad_url_no_default_port_test() ->
    File = file(["a: foo://domain.tld"]),
    ?checkError(
       {bad_url, {no_default_port, foo, _}, _},
       yval:validate(File, #{a => yval:url([])})).

bad_url_bad_port_test() ->
    File = file(["a: http://domain.tld:0"]),
    ?checkError(
       {bad_url, bad_port, _},
       yval:validate(File, #{a => yval:url([])})),
    File = file(["a: http://domain.tld:-1"]),
    ?checkError(
       {bad_url, bad_port, _},
       yval:validate(File, #{a => yval:url([])})),
    File = file(["a: http://domain.tld:65536"]),
    ?checkError(
       {bad_url, bad_port, _},
       yval:validate(File, #{a => yval:url([])})).

bad_url_test() ->
    File = file(["a: bad"]),
    ?checkError(
       {bad_url, _, <<"bad">>},
       yval:validate(File, #{a => yval:url()})).

octal_test() ->
    File = file(["a: \"644\""]),
    ?assertEqual(
       {ok, [{a, 420}]},
       yval:validate(File, #{a => yval:octal()})).

bad_octal_test() ->
    File = file(["a: \"9\""]),
    ?checkError(
       {bad_octal, <<"9">>},
       yval:validate(File, #{a => yval:octal()})).

ipv4_test() ->
    File = file(["a: 127.0.0.1"]),
    ?assertEqual(
       {ok, [{a, {127,0,0,1}}]},
       yval:validate(File, #{a => yval:ipv4()})).

bad_ipv4_test() ->
    File = file(["a: '::1'"]),
    ?checkError(
       {bad_ipv4, "::1"},
       yval:validate(File, #{a => yval:ipv4()})).

ipv6_test() ->
    File = file(["a: '::1'"]),
    ?assertEqual(
       {ok, [{a, {0,0,0,0,0,0,0,1}}]},
       yval:validate(File, #{a => yval:ipv6()})).

bad_ipv6_test() ->
    File = file(["a: 127.0.0.1"]),
    ?checkError(
       {bad_ipv6, "127.0.0.1"},
       yval:validate(File, #{a => yval:ipv6()})).

ip_test() ->
    File = file(["a: 127.0.0.1",
		 "b: '::1'"]),
    ?assertEqual(
       {ok, [{a, {127,0,0,1}}, {b, {0,0,0,0,0,0,0,1}}]},
       yval:validate(File, #{a => yval:ip(), b => yval:ip()})).

bad_ip_test() ->
    File = file(["a: bad"]),
    ?checkError(
       {bad_ip, "bad"},
       yval:validate(File, #{a => yval:ip()})).

ip_mask_test() ->
    File = file(["a: 127.0.0.1",
		 "b: 127.0.0.1/0",
		 "c: 127.0.0.1/32",
		 "d: '::1'",
		 "e: '::1/0'",
		 "f: '::1/128'"]),
    ?assertEqual(
       {ok, [{a, {{127,0,0,1}, 32}},
	     {b, {{127,0,0,1}, 0}},
	     {c, {{127,0,0,1}, 32}},
	     {d, {{0,0,0,0,0,0,0,1}, 128}},
	     {e, {{0,0,0,0,0,0,0,1}, 0}},
	     {f, {{0,0,0,0,0,0,0,1}, 128}}]},
       yval:validate(File, #{a => yval:ip_mask(),
			   b => yval:ip_mask(),
			   c => yval:ip_mask(),
			   d => yval:ip_mask(),
			   e => yval:ip_mask(),
			   f => yval:ip_mask()})).

bad_ip_mask_test() ->
    File = file(["a: 127.0.0.1/128"]),
    ?checkError(
       {bad_ip_mask, "127.0.0.1/128"},
       yval:validate(File, #{a => yval:ip_mask()})).

port_test() ->
    File = file(["a: 1",
		 "b: 65535"]),
    ?assertEqual(
       {ok, [{a, 1}, {b, 65535}]},
       yval:validate(File, #{a => yval:port(), b => yval:port()})).

timeout_test() ->
    File = file(["millisecond: 1",
		 "second: 1",
		 "minute: 1",
		 "hour: 1",
		 "day: 1"]),
    ?assertEqual(
       {ok, [{millisecond, 1},
	     {second, 1000},
	     {minute, 60000},
	     {hour, 3600000},
	     {day, 86400000}]},
       yval:validate(File, #{millisecond => yval:timeout(millisecond),
			   second => yval:timeout(second),
			   minute => yval:timeout(minute),
			   hour => yval:timeout(hour),
			   day => yval:timeout(day)})).

timeout_atom_test() ->
    File = file(["a: '5'"]),
    ?assertEqual(
       {ok, [{a, 5}]},
       yval:validate(File, #{a => yval:timeout(millisecond)},
		   [plain_as_atom])).

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
       {ok, [{ms,1},
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
	     {days,86400000}]},
       yval:validate(File, #{'_' => yval:timeout(millisecond)})).

timeout_infinity_test() ->
    File = file(["a: infinity",
		 "b: infinite",
		 "c: unlimited"]),
    ?assertEqual(
       {ok, [{a, infinite}, {b, unlimited}, {c, infinity}]},
       yval:validate(File, #{a => yval:timeout(day, infinite),
			   b => yval:timeout(day, unlimited),
			   c => yval:timeout(day, infinity)})).

bad_timeout_test() ->
    File = file(["a: []"]),
    ?checkError(
       {bad_timeout, []},
       yval:validate(File, #{a => yval:timeout(second)})),
    ?checkError(
       {bad_timeout, infinity, []},
       yval:validate(File, #{a => yval:timeout(second, infinity)})).

bad_timeout_zero_test() ->
    File = file(["a: 0"]),
    ?checkError(
       {bad_pos_int, 0},
       yval:validate(File, #{a => yval:timeout(second)})),
    ?checkError(
       {bad_pos_int, infinity, 0},
       yval:validate(File, #{a => yval:timeout(second, infinity)})).

bad_timeout_infinity_test() ->
    File = file(["a: foo"]),
    ?checkError(
       {bad_int, <<"foo">>},
       yval:validate(File, #{a => yval:timeout(second)})),
    ?checkError(
       {bad_enum, _, foo},
       yval:validate(File, #{a => yval:timeout(second, infinity)})).

bad_timeout_unit_test() ->
    File = file(["a: 1foo"]),
    ?checkError(
       {bad_timeout_unit, "foo"},
       yval:validate(File, #{a => yval:timeout(second)})).

bad_timeout_min_test() ->
    File = file(["a: 1ms"]),
    ?checkError(
       {bad_timeout_min, second},
       yval:validate(File, #{a => yval:timeout(second)})).

bad_timeout_negative_test() ->
    File = file(["a: -1s"]),
    ?checkError(
       {bad_pos_int, -1},
       yval:validate(File, #{a => yval:timeout(second)})),
    ?checkError(
       {bad_pos_int, infinity, -1},
       yval:validate(File, #{a => yval:timeout(second, infinity)})).

re_test() ->
    File = file(["a: ^[0-9]+$"]),
    ?assertMatch(
       {ok, [{a, _}]},
       yval:validate(File, #{a => yval:re()})).

bad_re_test() ->
    File = file(["a: '['"]),
    ?checkError(
       {bad_regexp, {_, _}, _},
       yval:validate(File, #{a => yval:re()})).

glob_test() ->
    File = file(["a: '*'"]),
    ?assertMatch(
       {ok, [{a, _}]},
       yval:validate(File, #{a => yval:glob()})).

bad_glob_test() ->
    File = file(["a: '['"]),
    ?checkError(
       {bad_glob, {_, _}, _},
       yval:validate(File, #{a => yval:glob()})).

beam_test() ->
    Exports = [[{foo, 1}, {parse, 2}], {parse, 3}, []],
    File = file(["a: yconf"]),
    ?assertMatch(
       {ok, [{a, yconf}]},
       yval:validate(File, #{a => yval:beam(Exports)})).

bad_beam_test() ->
    File = file(["a: foo"]),
    ?checkError(
       {bad_module, foo},
       yval:validate(File, #{a => yval:beam()})),
    File = file(["a: yconf"]),
    ?checkError(
       {bad_export, {foo, 1}, yconf},
       yval:validate(File, #{a => yval:beam([[{foo, 1}, {bar, 2}]])})),
    ?checkError(
       {bad_export, {foo, 1}, yconf},
       yval:validate(File, #{a => yval:beam([{foo, 1}])})).

non_empty_test() ->
    File = file(["a: [1,2,3]",
		 "b: 1",
		 "c: foo",
		 "d: {e: f}"]),
    ?assertMatch(
       {ok, [{a, [1,2,3]}, {b, 1}, {c, foo}, {d, [_]}]},
       yval:validate(File, #{a => yval:non_empty(yval:list(yval:int())),
			   b => yval:non_empty(yval:int()),
			   c => yval:non_empty(yval:atom()),
			   d => yval:non_empty(yval:map(yval:any(), yval:any()))})).

empty_atom_test() ->
    File = file(["a: ''"]),
    ?checkError(
       empty_atom,
       yval:validate(File, #{a => yval:non_empty(yval:atom())})).

empty_binary_test() ->
    File = file(["a: ''"]),
    ?checkError(
       empty_binary,
       yval:validate(File, #{a => yval:non_empty(yval:binary())})).

empty_list_test() ->
    File = file(["a: []"]),
    ?checkError(
       empty_list,
       yval:validate(File, #{a => yval:non_empty(yval:list(yval:any()))})).

empty_map_test() ->
    File = file(["a: {}"]),
    ?checkError(
       empty_list,
       yval:validate(File, #{a => yval:non_empty(
				  yval:map(yval:any(), yval:any()))})).

list_test() ->
    File = file(["a: [1,2,3]"]),
    ?assertMatch(
       {ok, [{a, [1,2,3]}]},
       yval:validate(File, #{a => yval:list(yval:any())})).

bad_list_test() ->
    File = file(["a: 1"]),
    ?checkError(
       {bad_list, 1},
       yval:validate(File, #{a => yval:list(yval:any())})).

sorted_list_test() ->
    File = file(["a: [3,2,1]"]),
    ?assertMatch(
       {ok, [{a, [1,2,3]}]},
       yval:validate(File, #{a => yval:list(yval:any(), [sorted])})).

bad_sorted_list_test() ->
    File = file(["a: 1"]),
    ?checkError(
       {bad_list, 1},
       yval:validate(File, #{a => yval:list(yval:any(), [sorted])})).

unique_list_test() ->
    File = file(["a: [1,2,3]"]),
    ?assertMatch(
       {ok, [{a, [1,2,3]}]},
       yval:validate(File, #{a => yval:list(yval:any(), [unique])})).

bad_unique_list_test() ->
    File = file(["a: [1,2,1,3]"]),
    ?checkError(
       {duplicated_value, 1},
       yval:validate(File, #{a => yval:list(yval:any(), [unique])})),
    File = file(["a: [foo, bar, foo]"]),
    ?checkError(
       {duplicated_value, foo},
       yval:validate(File, #{a => yval:list(yval:atom(), [unique])})),
    File = file(["a: [[1], [2], [1]]"]),
    ?checkError(
       {duplicated_value, [1]},
       yval:validate(File, #{a => yval:list(yval:any(), [unique])})).

list_or_single_test() ->
    File = file(["a: 1",
		 "b: [1,2,3]"]),
    ?assertMatch(
       {ok, [{a, [1]}, {b, [1,2,3]}]},
       yval:validate(File, #{a => yval:list_or_single(yval:any()),
			   b => yval:list_or_single(yval:any())})).

sorted_list_or_single_test() ->
    File = file(["a: 1",
		 "b: [3,2,1]"]),
    ?assertMatch(
       {ok, [{a, [1]}, {b, [1,2,3]}]},
       yval:validate(File, #{a => yval:list_or_single(yval:any(), [sorted]),
			   b => yval:list_or_single(yval:any(), [sorted])})).

unique_list_or_single_test() ->
    File = file(["a: 1",
		 "b: [1,2,3]"]),
    ?assertMatch(
       {ok, [{a, [1]}, {b, [1,2,3]}]},
       yval:validate(File, #{a => yval:list_or_single(yval:any(), [unique]),
			   b => yval:list_or_single(yval:any(), [unique])})).

bad_unique_list_or_single_test() ->
    File = file(["a: 1",
		 "b: [1,2,1,3]"]),
    ?checkError(
       {duplicated_value, 1},
       yval:validate(File, #{a => yval:list_or_single(yval:any(), [unique]),
			   b => yval:list_or_single(yval:any(), [unique])})).

map_test() ->
    File = file(["a: {c: 2, b: 1}"]),
    ?assertEqual(
       {ok, [{a, [{c, 2}, {b, 1}]}]},
       yval:validate(File, #{a => yval:map(yval:atom(), yval:any())})),
    ?assertEqual(
       {ok, [{a, [{c, 2}, {b, 1}]}]},
       yval:validate(File, #{a => yval:map(yval:atom(), yval:any(),
					  [unique])})),
    ?assertEqual(
       {ok, [{a, [{b, 1}, {c, 2}]}]},
       yval:validate(File, #{a => yval:map(yval:atom(), yval:any(),
					  [{return, orddict}])})),
    ?assertEqual(
       {ok, [{a, #{b => 1, c => 2}}]},
       yval:validate(File, #{a => yval:map(yval:atom(), yval:any(),
					  [{return, map}])})),
    Ret = yval:validate(File, #{a => yval:map(yval:atom(), yval:any(),
					     [{return, dict}])}),
    ?assertMatch({ok, [{a, _}]}, Ret),
    ?assertEqual(
       [{b, 1}, {c, 2}],
       lists:keysort(1, dict:to_list(element(2, hd(element(2, Ret)))))).

bad_map_test() ->
    V = yval:map(yval:atom(), yval:any()),
    File = file(["a: 1"]),
    ?checkError(
       {bad_map, 1},
       yval:validate(File, #{a => V})),
    File = file(["a: [1,2,3]"]),
    ?checkError(
       {bad_map, [1,2,3]},
       yval:validate(File, #{a => V})).

bad_unique_map_test() ->
    File = file(["a: {c: 2, b: 1, c: 3}"]),
    ?checkError(
       {duplicated_key, c},
       yval:validate(File, #{a => yval:map(yval:atom(), yval:any(),
					  [unique])})).

either_test() ->
    V = yval:either(yval:bool(), yval:int()),
    File = file(["a: true",
		 "b: 5"]),
    ?assertEqual(
       {ok, [{a, true}, {b, 5}]},
       yval:validate(File, #{a => V, b => V})).

either_atom_test() ->
    V = yval:either(atom, yval:int()),
    File = file(["a: atom",
		 "b: 1"]),
    ?assertEqual(
       {ok, [{a, atom}, {b, 1}]},
       yval:validate(File, #{a => V, b => V})).

and_then_test() ->
    V = yval:and_then(
	  yval:list(yval:int()),
	  fun lists:sum/1),
    File = file(["a: [1,2,3]"]),
    ?assertEqual(
       {ok, [{a, 6}]},
       yval:validate(File, #{a => V})).

options_test() ->
    File = file(["a: {b: 1, c: true}"]),
    ?assertEqual(
       {ok, [{a, [{b, 1}, {c, true}]}]},
       yval:validate(File, #{a => yval:options(
				  #{b => yval:int(),
				    c => yval:bool(),
				    d => yval:atom()})})).

options_return_map_test() ->
    File = file(["a: 1",
		 "b: 2"]),
    ?assertEqual(
       {ok, #{a => 1, b => 2}},
       yval:validate(File, #{a => yval:any(),
			   b => yval:any()},
		   [{return, map}])).

options_return_dict_test() ->
    File = file(["a: 1",
		 "b: 2"]),
    Ret = yval:validate(File, #{a => yval:any(),
			      b => yval:any()},
		      [{return, dict}]),
    ?assertMatch({ok, _}, Ret),
    ?assertEqual(
       [{a, 1}, {b, 2}],
       lists:keysort(1, dict:to_list(element(2, Ret)))).

options_return_orddict_test() ->
    File = file(["b: 1",
		 "a: 2"]),
    ?assertEqual(
       {ok, [{a, 2}, {b, 1}]},
       yval:validate(File, #{a => yval:any(),
			   b => yval:any()},
		   [{return, orddict}])).

options_default_validator_test() ->
    File = file(["a: {b: 1, c: true}"]),
    ?assertEqual(
       {ok, [{a, [{b, 1}, {c, true}]}]},
       yval:validate(File, #{a => yval:options(
				  #{b => yval:int(),
				    '_' => yval:bool()})})).

bad_options_test() ->
    File = file(["a: 1"]),
    ?checkError(
       {bad_map, 1},
       yval:validate(File, #{a => yval:options(#{})})),
    File = file(["a: [1,2,3]"]),
    ?checkError(
       {bad_map, [1,2,3]},
       yval:validate(File, #{a => yval:options(#{})})).

bad_binary_map_option_test() ->
    File = file(["a: {b: foo}"]),
    ?checkError(
       {bad_bool, foo},
       yval:validate(File, #{a => yval:map(yval:binary(), yval:bool())})).

bad_integer_map_option_test() ->
    File = file(["a: {1: foo}"]),
    ?checkError(
       {bad_bool, foo},
       yval:validate(File, #{a => yval:map(yval:int(), yval:bool())})).

unknown_option_test() ->
    File = file(["a: 1"]),
    ?checkError(
       {unknown_option, [define_macro], a},
       yval:validate(File, #{}, [replace_macros])).

missing_option_test() ->
    File = file(["a: 1"]),
    ?checkError(
       {missing_option, b},
       yval:validate(File, #{a => yval:int(),
			   b => yval:any()},
		   [{required, [b]}])).

disallowed_option_test() ->
    File = file(["a: 1",
		 "b: 2"]),
    ?checkError(
       {disallowed_option, b},
       yval:validate(File, #{a => yval:int()},
		   [{disallowed, [b]}])),
    ?checkError(
       {disallowed_option, b},
       yval:validate(File, #{a => yval:int(), b => yval:int()},
		   [{disallowed, [b]}])),
    ?checkError(
       {disallowed_option, b},
       yval:validate(File, #{a => yval:int(), b => yval:int()},
		   [{required, [b]}, {disallowed, [b]}])).

unknown_option_with_disallowed_test() ->
    File = file(["a: 1",
		 "c: 2"]),
    ?checkError(
       {unknown_option, [a], c},
       yval:validate(File, #{a => yval:int(), b => yval:int()},
		   [{disallowed, [b]}])).

duplicated_option_test() ->
    File = file(["a: 1",
		 "b: 2",
		 "a: 3"]),
    ?checkError(
       {duplicated_option, a},
       yval:validate(File, #{a => yval:int(), b => yval:int()},
		   [unique])),
    ?assertEqual(
       {ok, [{a, 1}, {b, 2}, {a, 3}]},
       yval:validate(File, #{a => yval:int(), b => yval:int()}, [])).

duplicated_unknown_option_test() ->
    File = file(["a: 1",
		 "b: 2",
		 "b: 3"]),
    ?checkError(
       {duplicated_option, b},
       yval:validate(File, #{a => yval:int(),
			   '_' => yval:any()},
		   [unique])).

bad_cwd_test() ->
    test_format_error({error, {bad_cwd, eaccess}, []}).

unknown_reason_test() ->
    test_format_error({error, foo, []}).

unicode_test() ->
    UTF8CharList = [209,134],
    UTF8CharBin = list_to_binary(UTF8CharList),
    UTF8CharAtom = list_to_atom(UTF8CharList),
    File = file(["a: " ++ UTF8CharList,
		 "b: " ++ UTF8CharList]),
    ?assertEqual(
       {ok, [{a, UTF8CharAtom}, {b, UTF8CharBin}]},
       yval:validate(File, #{a => yval:atom(),
			   b => yval:binary()},
		   [plain_as_atom])),
    ?assertEqual(
       {ok, [{a, UTF8CharAtom}, {b, UTF8CharBin}]},
       yval:validate(File, #{a => yval:atom(),
			   b => yval:binary()})).

stop_test() ->
    ?assertEqual(ok, yval:stop()).

%%%===================================================================
%%% Internal functions
%%%===================================================================
test_dir() ->
    {ok, Cwd} = file:get_cwd(),
    filename:join(filename:dirname(Cwd), "test").

file(Data) ->
    file("test.yml", Data).

included_file(Data) ->
    file("included.yml", Data).

file(FileName, Data) ->
    Path = filename:join(test_dir(), FileName),
    ok = file:write_file(Path, string:join(Data, io_lib:nl())),
    Path.

test_format_error({error, Why, Ctx}) ->
    ?assertMatch([_|_], yval:format_error(Why, Ctx)).
