
-define(PROJECT, "PROJECT").

-ifdef(prod).
-define(PROD, true).
-define(DEV,  false).
-else.
-define(PROD, false).
-define(DEV,  true).
-endif.


%% IF
-define(IF(Cond, TrueVal, FalseVal), case Cond of true -> TrueVal; false -> FalseVal end).


-define(SERVER, case node() of
                  'project_name@127.0.0.1' -> <<"node worker one">>;
                  Node                     -> list_to_binary(atom_to_list(Node))
                end
).


% NOW time in seconds
-define(now,   erlang:system_time(seconds)).
-define(mnow,  erlang:system_time(millisecond)).
-define(stime, misc:format("Y-m-d H:i:s",{date(),time()})).

% binary to atom
-define(b2a(B), list_to_atom(binary_to_list(B))).
% atom to binary
-define(a2b(A), list_to_binary(atom_to_list(A))).


%% Point
-define(p,         list_to_binary(io_lib:format("Mod:~w line:~w",       [?MODULE, ?LINE]))).
-define(p(Reason), list_to_binary(io_lib:format("Mod:~w line:~w ~100P", [?MODULE, ?LINE, Reason, 300]))).
-define(e(ErrCode), {err, {ErrCode, ?p}}).
-define(e(ErrCode, Reason), {err, {ErrCode, ?p(Reason)}}).
-define(f, list_to_binary(atom_to_list(?FUNCTION_NAME))).
-define(err_term(C,R), #{<<"status">> => <<"err">>, <<"code">> => ?a2b(C), <<"desc">> => R}). 

-type err() :: {err, {atom(), binary()}}.
-type bool_err() :: true|false|err().


%% Log messages
-define(INF(Str, Term), io:format("~p ~p: ~p:~p ~p ~100P~n",
                                  [?stime, ?PROJECT, ?MODULE, ?LINE, Str, Term, 300])).
-define(INF(Cond, Str, Term),   ?IF(Cond, ?INF(Str, Term), do_nothing)).
-define(INF(X1, X2, Str, Term), ?IF(X1 == X2, ?INF(Str, Term), do_nothing)).


%% c2s s2s
%% cmd macroses
-define(cmd(CmdName),
  #{<<"command">>  => CmdName,
    <<"args">>     => #{},
    <<"hash">>     => misc:hash()}).
-define(cmd(CmdName, Args),
  #{<<"command">>  => CmdName,
    <<"args">>     => Args,
    <<"hash">>     => misc:hash()}).
-define(cmd(CmdName, Args, Hash),
  #{<<"command">>  => CmdName,
    <<"args">>     => Args,
    <<"hash">>     => Hash}).


% CMD answers
-define(ok_cmd(CmdName, Status, Return), %% else_cmd 
  #{<<"command">> => CmdName,
    <<"status">>  => list_to_binary(atom_to_list(Status)),
    <<"args">>    => Return}).
-define(ok_cmd(CmdName, Return),
  #{<<"command">> => CmdName,
    <<"status">>  => <<"ok">>,
    <<"args">>    => Return}).
-define(ok_cmd(CmdName),
  #{<<"command">> => CmdName,
    <<"status">>  => <<"ok">>}).
%
-define(else_cmd(CmdName, Status, Return),
  ?ok_cmd(CmdName, Status, Return)).
-define(else_cmd(CmdName, Status),
  #{<<"command">> => CmdName,
    <<"status">>  => list_to_binary(atom_to_list(Status))}).


%
-define(er_cmd(CmdName, Code, Reason),
  #{<<"command">> => CmdName,
    <<"status">>  => <<"err">>,
    <<"code">>    => Code,
    <<"desc">>    => Reason}).
-define(er_cmd(CmdName, Code), ?er_cmd(CmdName, Code, ?p)).


