%%
%%  Copyright © 2023 Christopher Augustus
%%
%%  This Source Code Form is subject to the terms of the Mozilla Public
%%  License, v. 2.0. If a copy of the MPL was not distributed with this
%%  file, You can obtain one at http://mozilla.org/MPL/2.0/.
%%
-module(piermonfile).
-export([f_atom_with_file_path/2, p_monitor_file/2, init/2]).

-include_lib("kernel/include/file.hrl").
-define(INTERVAL, 1000).

f_file_path_components_without_parenting(FilePathComponents) ->
  case lists:splitwith(fun(E) -> E /= ".." end, FilePathComponents) of
    {[], _} -> FilePathComponents;
    {_, []} -> FilePathComponents;
    {L, R} -> f_file_path_components_without_parenting(
      lists:append(lists:droplast(L), lists:nthtail(1, R)))
  end.

f_shortest_absolute_file_path(FilePath) ->
  filename:join(
    f_file_path_components_without_parenting(
      lists:filter(fun(E) -> E /= "." end,
        filename:split(
          filename:absname(FilePath))))).

f_atom_with_file_path(PrefixAtom, FilePath) ->
  AbsFilePath = f_shortest_absolute_file_path(FilePath),
  list_to_atom(atom_to_list(PrefixAtom)
               ++ "_" ++ filename:basename(AbsFilePath)
               ++ "_" ++ filename:dirname(AbsFilePath)).

f_process_atom(FilePath) ->
  f_atom_with_file_path(?MODULE, FilePath).

p_file_mtime(FilePath) ->
  case file:read_file_info(FilePath) of
    {ok,FileInfo} -> FileInfo#file_info.mtime;
    _ -> undefined
  end.

p_monitor_file(FilePath, PidsObservers) when is_list(PidsObservers) ->
  ProcessAtom = f_process_atom(FilePath),
  case whereis(ProcessAtom) of
    Pid when is_pid(Pid) -> {ok, Pid, found, ProcessAtom};
    _ ->
      case PidsObservers of
        [] -> {error, passed_empty_list_of_observers};
        _ ->
          case file:read_file_info(FilePath) of
            {ok, _} -> {ok,
              spawn(?MODULE, init, [FilePath, PidsObservers]),
              spawned, ProcessAtom};
            _ -> {error, failed_to_read_file_info}
          end
      end
  end.

init(FilePath, PidsObservers) ->
  register(f_process_atom(FilePath), self()),
  loop(FilePath, p_file_mtime(FilePath), PidsObservers).

loop(FilePath, FileMtimePrev, PidsObservers) ->
  erlang:send_after(?INTERVAL, self(), examine),
  receive
    examine ->
      FileMtimeNow = p_file_mtime(FilePath),
      case FileMtimeNow of
        undefined -> terminate(FilePath, PidsObservers);
        _ ->
          case FileMtimeNow =:= FileMtimePrev of
            false -> lists:foreach(
              fun(Elem) ->
                  Elem ! {file_modified, FilePath}
                  %%,io:format("## <~s_...> ~s --> ~s ! {file_modified,\"~s\"}\n",
                  %%  [?MODULE, pid_to_list(self()), pid_to_list(Elem), FilePath])
              end,
              PidsObservers);
            true -> ok
          end
      end,
      loop(FilePath, FileMtimeNow, PidsObservers);
    {observe, PidObserver} ->
      loop(FilePath, FileMtimePrev,
        case lists:member(PidObserver, PidsObservers) of
          false -> [PidsObservers | PidObserver];
          true  -> PidsObservers
        end);
    {unobserve, PidObserver} ->
      case [X || X <- PidsObservers, X /= PidObserver] of
        [] -> ok; % !!! exit when out of observers
        FewerObservers ->
          loop(FilePath, FileMtimePrev, FewerObservers)
      end;
    stop -> terminate(FilePath, PidsObservers)
  end.

terminate(FilePath, PidsObservers) ->
  lists:foreach(
    fun(Elem) -> Elem ! {file_unmonitored, FilePath} end,
    PidsObservers),
  ok.
