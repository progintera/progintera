%%
%%  Copyright © 2023 Christopher Augustus
%%
%%  This Source Code Form is subject to the terms of the Mozilla Public
%%  License, v. 2.0. If a copy of the MPL was not distributed with this
%%  file, You can obtain one at http://mozilla.org/MPL/2.0/.
%%
-module(piermonsrc).
-export([p_monitor_source/1, init/1]).

f_process_atom(FilePath) ->
  piermonfile:f_atom_with_file_path(?MODULE, FilePath).

p_monitor_source(FilePath) when is_list(FilePath) ->
  ProcessAtom = f_process_atom(FilePath),
  case whereis(ProcessAtom) of
    Pid when is_pid(Pid) -> {ok, Pid, found, ProcessAtom};
    _ ->
      case file:read_file_info(FilePath) of
        {ok, _} -> {ok,
          spawn(?MODULE, init, [FilePath]),
          spawned, ProcessAtom};
        _ -> {error, failed_to_read_file_info}
      end
  end.

init(FilePath) ->
  case piermonfile:p_monitor_file(FilePath, [self()]) of
    {ok, PidFileMonitor, _, _} ->
      register(f_process_atom(FilePath), self()),
      loop(PidFileMonitor);
    _ ->
      io:format("FAILED ~s:init(~s)", [?MODULE, FilePath]),
      {error, failed_to_monitor_file_at_path}
  end.

loop(PidFileMonitor) ->
  receive
    {file_modified, FilePath} ->
      %%% TODO: send output to shell or ?
      shell_default:c(FilePath),
      loop(PidFileMonitor);
    {file_unmonitored, _} -> ok;
    stop ->
      PidFileMonitor ! {unobserve, self()},
      ok
  end.
