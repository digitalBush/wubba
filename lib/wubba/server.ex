defmodule Wubba.Server do
  use GenServer.Behaviour

  @type callback_mod :: module
  @type callback_args :: any
  @type callback :: {callback_mod, callback_args}

  defrecordp :state, 
    socket: nil :: :inet.socket(),
    acceptors: [] :: [pid],
    open_reqs: 0 :: non_neg_integer,
    limits: nil :: Wubba.Limits.t,
    callback: nil :: callback


  def init({Wubba.Options[min_acceptors: min_acceptors] = options,limits,callback}) do
    # Use the exit signal from the acceptor processes to know when they exit
    :erlang.process_flag(:trap_exit,true)

    {:ok,socket} = start_listener(options)

    acceptors = Enum.map 1..min_acceptors, fn _ -> 
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
    IO.puts("No more file descriptors, shutting down\n")
    {:stop, :emfile, state}
  end

  def handle_info({:EXIT, pid, :normal}, state) do
    {:noreply, remove_acceptor(state, pid)};
  end

  def handle_info({:EXIT, pid, reason}, state) do
    IO.puts("Elli request (pid #{inspect pid}) unexpectedly crashed:\n#{inspect reason}\n")
    {:noreply, remove_acceptor(state, pid)}
  end

  defp start_listener(Wubba.Options[ip: ip, port: port]) do
    :gen_tcp.listen(port, [
      :binary,
      {:ip, ip},
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