defmodule Wubba.Utils do

	def parse_path({:abs_path, raw_path}) do
	    case String.split(raw_path,"?") do
	        [url]       -> {:ok, {raw_path, split_path(url), []}}
	        [url, args] -> {:ok, {raw_path, split_path(url), split_args(args)}}
	    end
	end

	def parse_path({:absoluteURI, _scheme, _host, _port, path}), do: parse_path({:abs_path, path})
	def parse_path(_), do: {:error, :unsupported_uri}

	defp split_path(path) do
		lc p inlist String.split(path,"/"), p != "", do: p
	end

	

	defp split_args(<<>>) do
        []
	end

	defp split_args(query_string) do
		tokens = String.split(query_string,"&",[trim: true])
		lc token inlist tokens do 
			case String.split(token,"=") do
				[token] -> {token,true}
				[token, value] -> {token,value}
			end
		end
	end
end