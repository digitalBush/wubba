defmodule Wubba do
  use Application.Behaviour

  # See http://elixir-lang.org/docs/stable/Application.Behaviour.html
  # for more information on OTP Applications
  def start(_type, _args) do
    Wubba.Supervisor.start_link
  end

  def start_link(config) do
  	options = Wubba.Options.new(config)
  	limits = Wubba.Limits.new(config)

  	case options do
  		Wubba.Options[callback: nil] -> raise "callback required"
  		Wubba.Options[callback: callback, callback_args: callback_args, name: name] ->
			args = {options,limits,{callback,callback_args}}
			start_server(args,name)
	end
  end

  defp start_server(args,nil), do: :gen_server.start_link(Wubba.Server, args , [])
  defp start_server(args,name), do: :gen_server.start_link(name, Wubba.Server, args, [])

end
