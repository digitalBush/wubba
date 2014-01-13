defmodule Wubba.Server do
  use GenServer.Behaviour

  @type callback_mod :: module
  @type callback_args :: any
  @type callback :: {callback_mod, callback_args}

  defrecordp :state, 
    socket: nil :: :inet.socket(),
    acceptors: [] :: [pid],
    open_reqs: 0 :: non_neg_integer,
    limits: [] :: [{any, any}],
    callback: nil :: callback


  def init({options,limits,callback}) do
    # Use the exit signal from the acceptor processes to know when they exit
    :erlang.process_flag(:trap_exit,true)

    {:ok,socket} = start_listener(options)

    acceptors = Enum.map 1..options[:min_acceptors], fn _ -> 
      Wubba.Http.start_link(self(),socket,limits,callback) 
    end

    {:ok,state(socket: socket,acceptors: acceptors,limits: limits,callback: callback)}
  end


  def handle_call(:stop, _from, state) do
    {:stop, :normal, :ok, state}
  end


  def handle_cast(:accepted, state) do
    {:noreply, start_add_acceptor(state)}
  end

  def handle_cast(_msg, state) do
    {:noreply, state}
  end


  def handle_info({:EXIT, _pid, {:error, :emfile}}, State) do
    :error_logger.error_msg('No more file descriptors, shutting down~n')
    {:stop, :emfile, state}
  end

  def handle_info({:EXIT, pid, :normal}, state) do
    {:noreply, remove_acceptor(state, pid)};
  end

  def handle_info({:EXIT, pid, reason}, state) do
    :error_logger.error_msg('Elli request (pid #{pid}) unexpectedly crashed:\n#{reason}\n', [Pid, Reason])
    {:noreply, remove_acceptor(state, pid)}
  end

  defp start_listener(options) do
    :gen_tcp.listen(options[:port], [
      :binary,
      {:ip, options[:ip]},
      {:reuseaddr, true},
      {:backlog, 32768},
      {:packet, :raw},
      {:active, false}])
  end

  defp remove_acceptor(state(acceptors: acceptors,open_reqs: open_reqs) = state, pid) do
    state(state, acceptors: List.delete(acceptors,pid),open_reqs: open_reqs-1)
  end

  defp start_add_acceptor(state(acceptors: acceptors,open_reqs: open_reqs, socket: socket,limits: limits, callback: callback) = state) do
    pid = Wubba.Http.start_link(self(),socket,limits,callback) 
    state(state, acceptors: [pid | acceptors],open_reqs: open_reqs+1)
  end
end