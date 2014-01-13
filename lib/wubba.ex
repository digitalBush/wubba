defmodule Wubba do
  use Application.Behaviour

  @default_options [
    callback: nil,
    callback_args: nil,
    ip: {0,0,0,0}, 
    port: 8080, 
    min_acceptors: 12]

  @default_limits [
    accept_timeout: 10000,
    request_timeout: 60000,
    header_timeout: 10000,
    body_timeout: 30000,
    max_body_size: 1024000]

  # See http://elixir-lang.org/docs/stable/Application.Behaviour.html
  # for more information on OTP Applications
  def start(_type, _args) do
    Wubba.Supervisor.start_link
  end

  def start_link(options) do
  	options = Wubba.Utils.make_options(options,@default_options)
  	limits = Wubba.Utils.make_options(options,@default_limits)
  	callback = {options[:callback],options[:callback_args]}

  	args = {options,limits,callback}

    case Keyword.get(options, :name) do
        nil ->
          :gen_server.start_link(Wubba.Server, args , []);
        name ->
          :gen_server.start_link(name, Wubba.Server, args, [])
    end
  end

end
