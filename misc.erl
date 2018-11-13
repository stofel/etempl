-module(misc).

%
-compile(export_all).

-include("../include/etempl.hrl").

-import(calendar,[last_day_of_the_month/2, day_of_the_week/1,
                  datetime_to_gregorian_seconds/1, date_to_gregorian_days/1,
                  gregorian_days_to_date/1, is_leap_year/1]).

% recursion for function list
c_r(FunList, Args) ->
  case_recursion(FunList, Args).
case_recursion(FunList, Args) ->
  Fun = fun
            (F, [N|R], Acc = #{status := ok}) -> F(F, R, apply(N, [Acc]));
            (_F, _,    Acc = #{status := done}) -> Acc#{status := ok};
            (_F, _,    Acc) -> Acc
        end,
  Fun(Fun, FunList, Args).

%% pmap example
pmap(Fun, List) ->
  Parent = self(),
  %% spawn the processes
  Refs = lists:map(
    fun(Elem) ->
      Ref = make_ref(),
      spawn(
        fun() ->
          Parent ! {Ref, Fun(Elem)}
        end),
      Ref
    end, List),

  %% collect the results
  lists:map(
    fun(Ref) ->
      receive
        {Ref, Elem} -> Elem
      end
    end, Refs).


%%
increment_prefix(Inc) when Inc >= 0, Inc < 100000000 ->
  300000000 + Inc;
increment_prefix(Inc) when Inc >= 100000000, Inc < 1000000000 ->
  3000000000 + Inc;
increment_prefix(Inc) when Inc >= 100000000 ->
  30000000000 + Inc.

  



%%
version_compare(A, B, lte) ->
    case version_compare(A, B) of
        eq -> true;
        lt -> true;
        gt -> false
    end;
version_compare(A, B, gte) ->
    case version_compare(A, B) of
        eq -> true;
        gt -> true;
        lt -> false
    end;
version_compare(A, B, Result) ->
    Result =:= version_compare(A, B).

version_compare(A, A) ->
    eq;
version_compare([], [$0 | B]) ->
    version_compare([], dropdot(B));
version_compare([], _) ->
    lt; %% 2.3 < 2.3.1
version_compare([$0 | A], []) ->
    version_compare(dropdot(A), []);
version_compare(_, []) ->
    gt; %% 2.3.1 > 2.3
version_compare(A,  B) ->
    {AStr, ATl} = lists:splitwith(fun (X) -> X =/= $. end, A),
    {BStr, BTl} = lists:splitwith(fun (X) -> X =/= $. end, B),
    ANum = list_to_integer(AStr),
    BNum = list_to_integer(BStr),
    if ANum =:= BNum -> version_compare(dropdot(ATl), dropdot(BTl));
       ANum < BNum   -> lt;
       ANum > BNum   -> gt
    end.

dropdot(A) -> lists:dropwhile(fun (X) -> X =:= $. end, A).
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%               
%% Randoms                                           
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%               
%
hash() -> random_bin(4).
%
random_bin(N)  -> list_to_binary(random_str(N)).
%% random string
random_str(short) -> random_str(4);
random_str(long)  -> random_str(8);
random_str(Length) ->
  AllowedChars = "abcdefghijklmnopqrstuvwxyzABCDEFGHIJKLMNOPQRSTUVWXYZ123456789",
  lists:foldl(
    fun(_, Acc) ->
      [lists:nth(rand:uniform(length(AllowedChars)), AllowedChars)] ++ Acc
    end, [], lists:seq(1, Length)).

%
random_int(1) -> 1;
random_int(N) -> rand:uniform(N).
random_int(S, T) when S > 0, T > 0, T > S -> rand:uniform(T-S+1)+S-1.


%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% MD5
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
md5_hex(Bin) ->
  list_to_binary(lists:flatten(list_to_hex(binary_to_list(erlang:md5(Bin))))).

list_to_hex(L) ->
  lists:map(fun(X) -> int_to_hex(X) end, L).

int_to_hex(N) when N < 256 ->
  [hex(N div 16), hex(N rem 16)].

hex(N) when N < 10 ->
  $0+N;
hex(N) when N >= 10, N < 16 ->
  $a + (N-10).


%% base64 url safe
base64_url_save_encode(S) -> << << (urlencode_digit(D)) >> || <<D>> <= base64:encode(S), D =/= $= >>.
urlencode_digit($/) -> $_;
urlencode_digit($+) -> $-;
urlencode_digit(D) -> D.
base64_url_save_decode(Bin) when is_binary(Bin) ->
  Bin2 =
    case byte_size(Bin) rem 4 of
      % 1 -> << Bin/binary, "===" >>;
      2 -> << Bin/binary, "==" >>;
      3 -> << Bin/binary, "=" >>;
      _ -> Bin
    end,
  base64:decode(<< << (urldecode_digit(D)) >> || <<D>> <= Bin2 >>).
urldecode_digit($_) -> $/;
urldecode_digit($-) -> $+;
urldecode_digit(D) -> D.


http_enc(Bin) -> http_uri:encode(binary_to_list(Bin)).

%% get included maps value
map_value(Keys, Map, Default) ->
  Fun = fun
    (_F, [Key|[]], M)   -> maps:get(Key, M, Default);
    (Fu, [Key|Ks], M) ->
      case maps:get(Key, M, u) of
        V when is_map(V) -> Fu(Fu, Ks, V);
        _ -> Default
      end
  end,
  Fun(Fun, Keys, Map).


%% IPv6 encodings
str_to_tuple6(Str) when is_list(Str)->
    case io_lib:fread("~16u.~16u.~16u.~16u.~16u.~16u.~16u.~16u", Str) of
    {ok, Lst, _} ->
        list_to_tuple(Lst);
    _ ->
        error
    end.
    
bin_to_tuple6(Str) when is_binary(Str)->
    case io_lib:fread("~16u:~16u:~16u:~16u:~16u:~16u:~16u:~16u", binary_to_list(Str)) of
    {ok, Lst, _} ->
        list_to_tuple(Lst);
    _ ->
        error
    end.
    
tuple_to_str6(Tuple)->
    lists:flatten(io_lib:format("~4.16.0B:~4.16.0B:~4.16.0B:~4.16.0B:~4.16.0B:~4.16.0B:~4.16.0B:~4.16.0B", tuple_to_list(Tuple))).
    
tuple_to_bin6(Tuple)->
    list_to_binary(io_lib:format("~4.16.0B:~4.16.0B:~4.16.0B:~4.16.0B:~4.16.0B:~4.16.0B:~4.16.0B:~4.16.0B", tuple_to_list(Tuple))).






get_ugly_pid(cpu) ->
  Pids =
    [{-Reduc, Pid, case process_info(Pid, registered_name) of {registered_name,Name} -> Name; _ -> '_' end} ||
    {Reduc, Pid} <-
    lists:foldl(
        fun(Pid, L) when length(L) > 40 ->
                SL = lists:sublist(lists:keysort(1, L), 20),
                case process_info(Pid, reductions) of
                    {reductions,Reduc} -> [{-Reduc, Pid} | SL];
                    undefined -> L
                end;
            (Pid, L) ->
                case process_info(Pid, reductions) of
                    {reductions,Reduc} -> [{-Reduc, Pid} | L];
                    undefined -> L
                end
        end, [], erlang:processes())],
  lists:sublist(lists:reverse(Pids), 1, 5);

get_ugly_pid(mem) ->
  Pids =
    [{-Reduc, Pid, case process_info(Pid, registered_name) of {registered_name,Name} -> Name; _ -> '_' end} ||
    {Reduc, Pid} <-
    lists:foldl(
        fun(Pid, L) when length(L) > 40 ->
                SL = lists:sublist(lists:keysort(1, L), 20),
                case process_info(Pid, heap_size) of
                    {heap_size,Reduc} -> [{-Reduc, Pid} | SL];
                    undefined -> L
                end;
            (Pid, L) ->
                case process_info(Pid, heap_size) of
                    {heap_size,Reduc} -> [{-Reduc, Pid} | L];
                    undefined -> L
                end
        end, [], erlang:processes())],
  Pids.



%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% DataTime formatting
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

-define( is_num(X),      (X >= $0 andalso X =< $9) ).
-define( is_meridian(X), (X==[] orelse X==[am] orelse X==[pm]) ).
-define( is_sep(X),      (X==$- orelse X==$/ orelse X==$\.) ).

-define( is_day(X),      (X >= 1 andalso X =< 31)).
-define( is_month(X),    (X >= 1 andalso X =< 12)).

-define(GREGORIAN_SECONDS_1970, 62167219200).

-type year()     :: non_neg_integer().
-type month()    :: 1..12.
-type day()      :: 1..31.
-type hour()     :: 0..23.
-type minute()   :: 0..59.
-type second()   :: 0..59.
-type daynum()   :: 1..7.
-type date()     :: {year(),month(),day()}.
-type time()     :: {hour(),minute(),second()}.
-type datetime() :: {date(),time()}.
-type now()      :: {integer(),integer(),integer()}.


-spec format(string()) -> string().
%% @doc format current local time as Format
format(Format) ->
    format(Format, calendar:universal_time(),[]).

-spec format(string(),datetime() | now()) -> string().
%% @doc format Date as Format
format(Format, {_,_,_}=Now) ->
    format(Format, calendar:now_to_datetime(Now), []);
format(Format, Date) ->
    format(Format, Date, []).

-spec parse(string() | binary()) -> datetime() | {error, bad_date}.
%% @doc parses the datetime from a string
parse(Date) when is_binary(Date) ->
    parse(binary_to_list(Date));
parse(Date) ->
    do_parse(Date, calendar:universal_time(),[]).

-spec parse(string(),datetime() | now()) -> datetime() | {error, bad_date}.
%% @doc parses the datetime from a string
parse(Date, {_,_,_}=Now) ->
    do_parse(Date, calendar:now_to_datetime(Now), []);
parse(Date, Now) ->
    do_parse(Date, Now, []).

do_parse(Date, Now, Opts) ->
    case parse(tokenise(string:to_upper(Date), []), Now, Opts) of
        {error, Reason} ->
            {error, Reason};
        {D1, T1} = {{Y, M, D}, {H, M1, S}}
        when is_number(Y),  ?is_month(M),
             ?is_day(D),  is_number(H),
             is_number(M1), is_number(S) ->
            case calendar:valid_date(D1) of
                true  -> {D1, T1};
                false -> {error, bad_date}
            end;
        _ ->
            {error, bad_date}
    end.

-spec nparse(string()) -> now().
%% @doc parses the datetime from a string into 'now' format
nparse(Date) ->
    DateTime = parse(Date),
    GSeconds = calendar:datetime_to_gregorian_seconds(DateTime),
    ESeconds = GSeconds - ?GREGORIAN_SECONDS_1970,
    {ESeconds div 1000000, ESeconds rem 1000000, 0}.


%%
%% LOCAL FUNCTIONS
%%

%% Times - 21:45, 13:45:54, 13:15PM etc
parse([Hour,$:,Min,$:,Sec | PAM], {Date, _Time}, _O) when ?is_meridian(PAM) ->
    {Date, {hour(Hour, PAM), Min, Sec}};
parse([Hour,$:,Min | PAM], {Date, _Time}, _Opts) when ?is_meridian(PAM) ->
    {Date, {hour(Hour, PAM), Min, 0}};

%% Dates 23/april/1963
parse([Day,Month,Year], {_Date, Time}, _Opts) when ?is_day(Day), ?is_month(Month) ->
    {{to_year(Year), Month, Day}, Time};
parse([Day,X,Month,X,Year], {_Date, Time}, _Opts) when ?is_sep(X), ?is_day(Day), ?is_month(Month) ->
    {{to_year(Year), Month, Day}, Time};

parse([Year,Day,Month], {_Date, Time}, _Opts) when ?is_day(Day), ?is_month(Month) ->
    {{to_year(Year), Month, Day}, Time};
parse([Year,X,Month,X,Day], {_Date, Time}, _Opts) when ?is_sep(X), ?is_day(Day), ?is_month(Month) ->
    {{to_year(Year), Month, Day}, Time};

%% Date/Times 22 Aug 2008 6:35 PM
parse([Day,X,Month,X,Year,Hour,$:,Min | PAM], _Date, _Opts)
  when ?is_meridian(PAM), ?is_sep(X), ?is_day(Day), ?is_month(Month) ->
    {{to_year(Year), Month, Day}, {hour(Hour, PAM), Min, 0}};
parse([Day,X,Month,X,Year,Hour,$:,Min,$:,Sec | PAM], _Now, _Opts)
  when ?is_meridian(PAM), ?is_sep(X), ?is_day(Day), ?is_month(Month) ->
    {{to_year(Year), Month, Day}, {hour(Hour, PAM), Min, Sec}};

parse([Day,Month,Year,Hour,$:,Min | PAM], _Now, _Opts)
  when ?is_meridian(PAM), ?is_day(Day), ?is_month(Month) ->
    {{to_year(Year), Month, Day}, {hour(Hour, PAM), Min, 0}};
parse([Day,Month,Year,Hour,$:,Min,$:,Sec | PAM], _Now, _Opts)
  when ?is_meridian(PAM), ?is_day(Day), ?is_month(Month) ->
    {{to_year(Year), Month, Day}, {hour(Hour, PAM), Min, Sec}};

%% Date/Times 2008-08-22 18:35:00
parse([Year,$-,Month,$-,Day,Hour,$:,Min,$:,Sec], _, _Opts) ->
    {{to_year(Year),Month,Day}, {Hour,Min,Sec}};
parse([Year,$-,Month,$-,Day,Hour,$:,Min], _, _Opts) ->
    {{to_year(Year),Month,Day}, {Hour,Min,0}};


%% ISO8601: "2012-04-23T17:04:29+02:00"
parse([Year,$-,Month,$-,Day,$T,Hour,$:,Min,$:,Sec,PM,TZHour,$:,TZMin], _, _Opts) when PM =:= $-; PM =:= $+ ->
    LocalSecs = calendar:datetime_to_gregorian_seconds({{Year, Month, Day}, {Hour, Min, Sec}}),
    TZDiff = TZHour * 3600 + TZMin * 60,
    UniversalSecs = case PM of $- -> LocalSecs+TZDiff; $+ -> LocalSecs-TZDiff end,
    calendar:universal_time_to_local_time(calendar:gregorian_seconds_to_datetime(UniversalSecs));
parse([Year,$-,Month,$-,Day,$T,Hour,$:,Min,$:,Sec,PM,TZHour], DT, Opts) when PM =:= $-; PM =:= $+ ->
    parse([Year,$-,Month,$-,Day,$T,Hour,$:,Min,$:,Sec,PM,TZHour,$:,0], DT, Opts);
parse([Year,$-,Month,$-,Day,$T,Hour,$:,Min,$:,Sec,$Z], _, _Opts) ->
    calendar:universal_time_to_local_time({{Year, Month, Day}, {Hour, Min, Sec}});

parse(_Tokens, _Now, _Opts) ->
    {error, bad_date}.

tokenise([], Acc) ->
    lists:reverse(Acc);

tokenise([N1, N2, N3, N4 | Rest], Acc)
  when ?is_num(N1), ?is_num(N2), ?is_num(N3), ?is_num(N4) ->
    tokenise(Rest, [ ltoi([N1, N2, N3, N4]) | Acc]);
tokenise([N1, N2 | Rest], Acc)
  when ?is_num(N1), ?is_num(N2) ->
    tokenise(Rest, [ ltoi([N1, N2]) | Acc]);
tokenise([N1 | Rest], Acc)
  when ?is_num(N1)  ->
    tokenise(Rest, [ ltoi([N1]) | Acc]);

tokenise("JANUARY"++Rest, Acc)   -> tokenise(Rest, [1 | Acc]);
tokenise("JAN"++Rest, Acc)       -> tokenise(Rest, [1 | Acc]);
tokenise("FEBUARY"++Rest, Acc)   -> tokenise(Rest, [2 | Acc]);
tokenise("FEBRUARY"++Rest, Acc)  -> tokenise(Rest, [2 | Acc]);
tokenise("FEB"++Rest, Acc)       -> tokenise(Rest, [2 | Acc]);
tokenise("MARCH"++Rest, Acc)     -> tokenise(Rest, [3 | Acc]);
tokenise("MAR"++Rest, Acc)       -> tokenise(Rest, [3 | Acc]);
tokenise("APRIL"++Rest, Acc)     -> tokenise(Rest, [4 | Acc]);
tokenise("APR"++Rest, Acc)       -> tokenise(Rest, [4 | Acc]);
tokenise("MAY"++Rest, Acc)       -> tokenise(Rest, [5 | Acc]);
tokenise("JUNE"++Rest, Acc)      -> tokenise(Rest, [6 | Acc]);
tokenise("JUN"++Rest, Acc)       -> tokenise(Rest, [6 | Acc]);
tokenise("JULY"++Rest, Acc)      -> tokenise(Rest, [7 | Acc]);
tokenise("JUL"++Rest, Acc)       -> tokenise(Rest, [7 | Acc]);
tokenise("AUGUST"++Rest, Acc)    -> tokenise(Rest, [8 | Acc]);
tokenise("AUG"++Rest, Acc)       -> tokenise(Rest, [8 | Acc]);
tokenise("SEPTEMBER"++Rest, Acc) -> tokenise(Rest, [9 | Acc]);
tokenise("SEPT"++Rest, Acc)      -> tokenise(Rest, [9 | Acc]);
tokenise("SEP"++Rest, Acc)       -> tokenise(Rest, [9 | Acc]);
tokenise("OCTOBER"++Rest, Acc)   -> tokenise(Rest, [10 | Acc]);
tokenise("OCT"++Rest, Acc)       -> tokenise(Rest, [10 | Acc]);
tokenise("NOVEMBER"++Rest, Acc)  -> tokenise(Rest, [11 | Acc]);
tokenise("NOVEM"++Rest, Acc)     -> tokenise(Rest, [11 | Acc]);
tokenise("NOV"++Rest, Acc)       -> tokenise(Rest, [11 | Acc]);
tokenise("DECEMBER"++Rest, Acc)  -> tokenise(Rest, [12 | Acc]);
tokenise("DECEM"++Rest, Acc)     -> tokenise(Rest, [12 | Acc]);
tokenise("DEC"++Rest, Acc)       -> tokenise(Rest, [12 | Acc]);

tokenise([$: | Rest], Acc) -> tokenise(Rest, [ $: | Acc]);
tokenise([$/ | Rest], Acc) -> tokenise(Rest, [ $/ | Acc]);
tokenise([$- | Rest], Acc) -> tokenise(Rest, [ $- | Acc]);
tokenise([$\. | Rest], Acc) -> tokenise(Rest, [ $\. | Acc]);
tokenise([$+ | Rest], Acc) -> tokenise(Rest, [ $+ | Acc]);
tokenise("AM"++Rest, Acc)  -> tokenise(Rest, [am | Acc]);
tokenise("PM"++Rest, Acc)  -> tokenise(Rest, [pm | Acc]);

%% Postel's Law
%%
%%  be conservative in what you do,
%%  be liberal in what you accept from others.
%%
%% See RFC 793 Section 2.10 http://tools.ietf.org/html/rfc793
%%
%% Mebbies folk want to include Saturday etc in a date, nae borra
tokenise("MONDAY"++Rest, Acc)    -> tokenise(Rest, Acc);
tokenise("MON"++Rest, Acc)       -> tokenise(Rest, Acc);
tokenise("TUESDAY"++Rest, Acc)   -> tokenise(Rest, Acc);
tokenise("TUES"++Rest, Acc)      -> tokenise(Rest, Acc);
tokenise("TUE"++Rest, Acc)       -> tokenise(Rest, Acc);
tokenise("WEDNESDAY"++Rest, Acc) -> tokenise(Rest, Acc);
tokenise("WEDS"++Rest, Acc)      -> tokenise(Rest, Acc);
tokenise("WED"++Rest, Acc)       -> tokenise(Rest, Acc);
tokenise("THURSDAY"++Rest, Acc)  -> tokenise(Rest, Acc);
tokenise("THURS"++Rest, Acc)     -> tokenise(Rest, Acc);
tokenise("THUR"++Rest, Acc)      -> tokenise(Rest, Acc);
tokenise("THU"++Rest, Acc)       -> tokenise(Rest, Acc);
tokenise("FRIDAY"++Rest, Acc)    -> tokenise(Rest, Acc);
tokenise("FRI"++Rest, Acc)       -> tokenise(Rest, Acc);
tokenise("SATURDAY"++Rest, Acc)  -> tokenise(Rest, Acc);
tokenise("SAT"++Rest, Acc)       -> tokenise(Rest, Acc);
tokenise("SUNDAY"++Rest, Acc)    -> tokenise(Rest, Acc);
tokenise("SUN"++Rest, Acc)       -> tokenise(Rest, Acc);

%% Hmm Excel reports GMT in times so nuke that too
tokenise("GMT"++Rest, Acc) -> tokenise(Rest, Acc);
tokenise("UTC"++Rest, Acc) -> tokenise(Rest, Acc);
tokenise("DST"++Rest, Acc) -> tokenise(Rest, Acc);  % daylight saving time

tokenise([$, | Rest], Acc) -> tokenise(Rest, Acc);
tokenise([32 | Rest], Acc) -> tokenise(Rest, Acc);          % Spaces
tokenise("TH"++Rest, Acc)  -> tokenise(Rest, Acc);
tokenise("ND"++Rest, Acc)  -> tokenise(Rest, Acc);
tokenise("ST"++Rest, Acc)  -> tokenise(Rest, Acc);
tokenise("OF"++Rest, Acc)  -> tokenise(Rest, Acc);

tokenise([$T | Rest], Acc) -> tokenise(Rest, [$T | Acc]);
tokenise([$Z | Rest], Acc) -> tokenise(Rest, [$Z | Acc]);

tokenise([Else | Rest], Acc) ->
    tokenise(Rest, [{bad_token, Else} | Acc]).

hour(Hour, [])   -> Hour;
hour(Hour, [am]) -> Hour;
hour(Hour, [pm]) -> Hour+12.

-spec format(string(),datetime(),list()) -> string().
%% Finished, return
format([], _Date, Acc) ->
    lists:flatten(lists:reverse(Acc));

%% Escape backslashes
format([$\\,H|T], Dt, Acc) ->
    format(T,Dt,[H|Acc]);

%% Year Formats
format([$Y|T], {{Y,_,_},_}=Dt, Acc) ->
    format(T, Dt, [itol(Y)|Acc]);
format([$y|T], {{Y,_,_},_}=Dt, Acc) ->
    [_, _, Y3, Y4] = itol(Y),
    format(T, Dt, [[Y3,Y4]|Acc]);
format([$L|T], {{Y,_,_},_}=Dt, Acc) ->
    format(T, Dt, [itol(is_leap(Y))|Acc]);
format([$o|T], {Date,_}=Dt, Acc) ->
    format(T, Dt, [itol(iso_year(Date))|Acc]);

%% Month Formats
format([$n|T], {{_,M,_},_}=Dt, Acc) ->
    format(T, Dt, [itol(M)|Acc]);
format([$m|T], {{_,M,_},_}=Dt, Acc) ->
    format(T, Dt, [pad2(M)|Acc]);
format([$M|T], {{_,M,_},_}=Dt, Acc) ->
    format(T, Dt, [smonth(M)|Acc]);
format([$F|T], {{_,M,_},_}=Dt, Acc) ->
    format(T, Dt, [month(M)|Acc]);
format([$t|T], {{Y,M,_},_}=Dt, Acc) ->
    format(T, Dt, [itol(last_day_of_the_month(Y,M))|Acc]);

%% Week Formats
format([$W|T], {Date,_}=Dt, Acc) ->
    format(T, Dt, [pad2(iso_week(Date))|Acc]);

%% Day Formats
format([$j|T], {{_,_,D},_}=Dt, Acc) ->
    format(T, Dt, [itol(D)|Acc]);
format([$S|T], {{_,_,D},_}=Dt, Acc) ->
    format(T, Dt,[suffix(D)| Acc]);
format([$d|T], {{_,_,D},_}=Dt, Acc) ->
    format(T, Dt, [pad2(D)|Acc]);
format([$D|T], {Date,_}=Dt, Acc) ->
    format(T, Dt, [sdayd(Date)|Acc]);
format([$l|T], {Date,_}=Dt, Acc) ->
    format(T, Dt, [day(day_of_the_week(Date))|Acc]);
format([$N|T], {Date,_}=Dt, Acc) ->
    format(T, Dt, [itol(day_of_the_week(Date))|Acc]);
format([$w|T], {Date,_}=Dt, Acc) ->
    format(T, Dt, [itol(to_w(day_of_the_week(Date)))|Acc]);
format([$z|T], {Date,_}=Dt, Acc) ->
    format(T, Dt, [itol(days_in_year(Date))|Acc]);

%% Time Formats
format([$a|T], {_,{H,_,_}}=Dt, Acc) when H > 12 ->
    format(T, Dt, ["pm"|Acc]);
format([$a|T], Dt, Acc) ->
    format(T, Dt, ["am"|Acc]);
format([$A|T], {_,{H,_,_}}=Dt, Acc) when H > 12 ->
    format(T, Dt, ["PM"|Acc]);
format([$A|T], Dt, Acc) ->
    format(T, Dt, ["AM"|Acc]);
format([$g|T], {_,{H,_,_}}=Dt, Acc) when H == 12; H == 0 ->
    format(T, Dt, ["12"|Acc]);
format([$g|T], {_,{H,_,_}}=Dt, Acc) when H > 12 ->
    format(T, Dt, [itol(H-12)|Acc]);
format([$g|T], {_,{H,_,_}}=Dt, Acc) ->
    format(T, Dt, [itol(H)|Acc]);
format([$G|T], {_,{H,_,_}}=Dt, Acc) ->
    format(T, Dt, [itol(H)|Acc]);
format([$h|T], {_,{H,_,_}}=Dt, Acc) when H > 12 ->
    format(T, Dt, [pad2(H-12)|Acc]);
format([$h|T], {_,{H,_,_}}=Dt, Acc) ->
    format(T, Dt, [pad2(H)|Acc]);
format([$H|T], {_,{H,_,_}}=Dt, Acc) ->
    format(T, Dt, [pad2(H)|Acc]);
format([$i|T], {_,{_,M,_}}=Dt, Acc) ->
    format(T, Dt, [pad2(M)|Acc]);
format([$s|T], {_,{_,_,S}}=Dt, Acc) ->
    format(T, Dt, [pad2(S)|Acc]);

%% Whole Dates
format([$c|T], {{Y,M,D},{H,Min,S}}=Dt, Acc) ->
    Format = "~4.10.0B-~2.10.0B-~2.10.0B"
        ++" ~2.10.0B:~2.10.0B:~2.10.0B",
    Date = io_lib:format(Format, [Y, M, D, H, Min, S]),
    format(T, Dt, [Date|Acc]);
format([$r|T], {{Y,M,D},{H,Min,S}}=Dt, Acc) ->
    Format = "~s, ~p ~s ~p ~2.10.0B:~2.10.0B:~2.10.0B",
    Args   = [sdayd({Y,M,D}), D, smonth(M), Y, H, Min, S],
    format(T, Dt, [io_lib:format(Format, Args)|Acc]);
format([$U|T], Dt, Acc) ->
    Epoch = {{1970,1,1},{0,0,0}},
    Time  = datetime_to_gregorian_seconds(Dt) -
        datetime_to_gregorian_seconds(Epoch),
    format(T, Dt, [itol(Time)|Acc]);

%% Unrecognised, print as is
format([H|T], Date, Acc) ->
    format(T, Date, [H|Acc]).


%% @doc days in year
-spec days_in_year(date()) -> integer().
days_in_year({Y,_,_}=Date) ->
    date_to_gregorian_days(Date) -
        date_to_gregorian_days({Y,1,1}).

%% @doc is a leap year
-spec is_leap(year()) -> 1|0.
is_leap(Y) ->
    case is_leap_year(Y) of
        true  -> 1;
        false -> 0
    end.

%% @doc Made up numeric day of the week
%%      (0 Sunday -> 6 Saturday)
-spec to_w(daynum()) -> integer().
to_w(7) -> 0;
to_w(X) -> X.

-spec suffix(day()) -> string().
%% @doc English ordinal suffix for the day of the
%%      month, 2 characters
suffix(1) -> "st";
suffix(2) -> "nd";
suffix(3) -> "rd";
suffix(_) -> "th".

-spec sdayd(date()) -> string().
%% @doc A textual representation of a day, three letters
sdayd({Y,M,D}) ->
    sday(day_of_the_week({Y,M,D})).

-spec sday(daynum()) -> string().
%% @doc A textual representation of a day, three letters
sday(1) -> "Mon";
sday(2) -> "Tue";
sday(3) -> "Wed";
sday(4) -> "Thu";
sday(5) -> "Fri";
sday(6) -> "Sat";
sday(7) -> "Sun".

-spec day(daynum()) -> string().
%% @doc A full textual representation of a day
day(1) -> "Monday";
day(2) -> "Tuesday";
day(3) -> "Wednesday";
day(4) -> "Thursday";
day(5) -> "Friday";
day(6) -> "Saturday";
day(7) -> "Sunday".

-spec smonth(month()) -> string().
%% @doc A short textual representation of a
%%      month, three letters
smonth(1)  -> "Jan";
smonth(2)  -> "Feb";
smonth(3)  -> "Mar";
smonth(4)  -> "Apr";
smonth(5)  -> "May";
smonth(6)  -> "Jun";
smonth(7)  -> "Jul";
smonth(8)  -> "Aug";
smonth(9)  -> "Sep";
smonth(10) -> "Oct";
smonth(11) -> "Nov";
smonth(12) -> "Dec".

-spec month(month()) -> string().
%% @doc A full textual representation of a month
month(1)  -> "January";
month(2)  -> "February";
month(3)  -> "March";
month(4)  -> "April";
month(5)  -> "May";
month(6)  -> "June";
month(7)  -> "July";
month(8)  -> "August";
month(9)  -> "September";
month(10) -> "October";
month(11) -> "November";
month(12) -> "December".

-spec iso_week(date()) -> integer().
%% @doc The week of the years as defined in ISO 8601
%%      http://en.wikipedia.org/wiki/ISO_week_date
iso_week(Date) ->
    Week = iso_week_one(iso_year(Date)),
    Days = date_to_gregorian_days(Date) -
        date_to_gregorian_days(Week),
    trunc((Days / 7) + 1).

-spec iso_year(date()) -> integer().
%% @doc The year number as defined in ISO 8601
%%      http://en.wikipedia.org/wiki/ISO_week_date
iso_year({Y, _M, _D}=Dt) ->
    case Dt >= {Y, 12, 29} of
        true ->
            case Dt < iso_week_one(Y+1) of
                true  -> Y;
                false -> Y+1
            end;
        false ->
            case Dt < iso_week_one(Y) of
                true  -> Y-1;
                false -> Y
            end
    end.

-spec iso_week_one(year()) -> date().
%% @doc The date of the the first day of the first week
%%      in the ISO calendar
iso_week_one(Y) ->
    Day1 = calendar:day_of_the_week({Y,1,4}),
    Days = date_to_gregorian_days({Y,1,4}) + (1-Day1),
    gregorian_days_to_date(Days).

-spec itol(integer()) -> list().
%% @doc short hand
itol(X) ->
    integer_to_list(X).

-spec pad2(integer()) -> list().
%% @doc int padded with 0 to make sure its 2 chars
pad2(X) when is_integer(X) ->
    io_lib:format("~2.10.0B",[X]);
pad2(X) when is_float(X) ->
    io_lib:format("~2.10.0B",[trunc(X)]).

ltoi(X) ->
    list_to_integer(X).

%% Normalise two digit years
-spec to_year(integer()) -> integer().
to_year(Y) when Y >= 60, Y < 100 -> Y + 1900;
to_year(Y) when Y < 100 -> Y + 2000;
to_year(Y) -> Y.





