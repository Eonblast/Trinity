% Eonbeam Dev: Supervisor Skeleton
% --------------------------------
% file: eonbeam/dev/3/i_supervisortest2/hello_supervisor.erl
% see: http://www.erlang.org/doc/design_principles/sup_princ.html

-module(hello_supervisor).
-behaviour(supervisor).

-export([start_link/0, init/1]).

%%====================================================================
%% API
%%====================================================================
%%--------------------------------------------------------------------
%% Function: start_link() -> {ok,Pid} | ignore | {error,Error}
%% Description: Starts the server
%%--------------------------------------------------------------------

start_link() ->
	io:format("hello supervisor: start~n"),
    supervisor:start_link(?MODULE, []).

%%====================================================================
%% supervisor callbacks
%%====================================================================

%%--------------------------------------------------------------------
%% Function: init(Args) -> {ok, {SupervisorSpec, [ChildSpec]}}
%% SupervisorSpec = {RestartStrategy, MaxRetries, InSeconds}
%% 	RestartStrategy = one_for_one | one_for_all | rest_for_one
%% 	MaxRetries = integer()
%% 	InSeconds = integer()
%% ChildSpec = {Id, StartFunc, Restart, Shutdown, Type, Modules}
%%  Id = term()
%%  StartFunc = {M, F, A}
%%  	M = F = atom()
%%      A = [term()]
%%	Restart = permanent | transient | temporary
%%  Shutdown = brutal_kill | integer() >=0 (seconds) | infinity
%%  Type = worker | supervisor
%%  Modules = [Module] | dynamic
%%  	Module = atom()
%% Description: Initiates the supervisor
%%--------------------------------------------------------------------

init(_Args) ->

	io:format("hello supervisor: init (this one has no static child) **~n"),
    
    % return the config, with static supervisor and child spec:
    {ok,{ 
    	% supervisor spec
    	{one_for_one, 1, 60}, % restart strategy, times, time range
    	% child specs: none (** different from h **)
 		[]
 	}}.


%%--------------------------------------------------------------------
%% Stopping:
%% Since the supervisor is part of a supervision tree, it will automatically
%% be terminated by its supervisor. When asked to shutdown, it will terminate
%% all child processes in reversed start order according to the respective
%% shutdown specifications, and then terminate itself.
%%--------------------------------------------------------------------
