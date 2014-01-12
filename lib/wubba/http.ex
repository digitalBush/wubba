defmodule Wubba.Http do
	#TODO: Break this into Request Processing / Response Delivery modules?
	#TODO: Add helper for generating http response? (related to above?)
	#TODO: Figure out replacement for :proplists
	#TODO: Figure out how to reduce passing around callback everywhere
	#TODO: Try to remove need to pass around limits collection (replace with record?)
	#TODO: Remove handover for now to simplify example?

	def start_link(_, _, _, {nil, _args}) do
	    raise "Callback module is required"
	end

	def start_link(server, listen_socket, limits, callback) do
	    :proc_lib.spawn_link(__MODULE__, :accept, [server, listen_socket, limits, callback])
	end

	def accept(server, listen_socket, limits, callback) do
	    case :gen_tcp.accept(listen_socket, limits[:accept_timeout]) do
	        {:ok, socket} ->
	            :gen_server.cast(server, :accepted)
	            __MODULE__.keepalive_loop(socket, limits, callback)
	        {:error, :timeout} ->
	            __MODULE__.accept(server, listen_socket, limits, callback)
	        {:error, :econnaborted} ->
	            __MODULE__.accept(server, listen_socket, limits, callback)
	        {:error, :closed} ->
	            :ok
	        {:error, other} ->
                exit({:error, other})
		end
	end

	def keepalive_loop(socket, limits, callback) do
	    keepalive_loop(socket, 0, <<>>, limits, callback)
	end

	def keepalive_loop(socket, num_requests, buffer, limits, callback) do
	    case __MODULE__.handle_request(socket, buffer, limits, callback) do
	        {:keep_alive, new_buffer} ->
	            __MODULE__.keepalive_loop(socket, num_requests, new_buffer, limits, callback)
	        {:close, _} ->
	            :gen_tcp.close(socket)
	            :ok
	    end
	end

	def handle_request(socket, buffer, limits, {module, args} = callback) do
		{method, raw_path, version, b0} = get_request(socket, buffer, limits)
	    {request_headers, b1} = get_headers(socket, version, b0, limits)
		request = Wubba.Request.new(
			method: method, 
			raw_path: raw_path, 
			headers: request_headers, 
			body: <<>>, 
			version: version,
			socket: socket, 
			callback: callback)
		

		{request_body, b2} = get_body(socket, request_headers, b1, limits)
        request = request.body(request_body)

        response = execute_callback(request)
        handle_response(request, b2, response)
	end

	defp handle_response(request, buffer, {:response, http_code, user_headers, body}) do
	    headers = [connection(request, user_headers), content_length(user_headers, body)| user_headers]
	    send_response(request, http_code, headers, body)
	    {close_or_keepalive(request, user_headers), buffer}
	end

	defp handle_response(request, buffer, {:file, response_code, user_headers, file_name, range}) do
	    headers = [connection(request, user_headers) | user_headers]
	    send_file(request, response_code, headers, file_name, range)
	    {close_or_keepalive(request, user_headers), buffer}
	end

	defp send_response(request, code, headers, body) do
	    body = case {request.method, code} do
			{:HEAD, _}  -> <<>>
			{_, 304}    -> <<>>
			{_, 204}    -> <<>>
			_           -> body
       	end

	    response = ["HTTP/1.1 ", status(code), "\r\n",
	                encode_headers(headers), "\r\n",
	                body]

	    :gen_tcp.send(request.socket, response) 
	   	:ok
	end

	defp send_file(request, code, headers, filename, {offset, length}) do
	    response_headers = ["HTTP/1.1 ", status(code), "\r\n",
	                       encode_headers(headers), "\r\n"]

	    {:ok, file_descriptor} = :file.open(filename, [:read, :raw, :binary])  
        try do
        	:ok = :gen_tcp.send(request.socket, response_headers)
            :ok = :file.sendfile(file_descriptor, request.socket, offset, length, [])
        after
            :file.close(file_descriptor)
        end
        :ok
	end


	defp connection(request, headers) do
	    case :proplists.get_value("Connection", headers) do
	        :undefined ->
	            {"Connection", connection_token(request)}
	        _ ->
	            []
	    end
	end

	defp encode_headers([]), do: []
	defp encode_headers([[] | tail]), do: encode_headers(tail)
	defp encode_headers([{key, value} | tail]), do: [encode_value(key), ": ", encode_value(value), "\r\n", encode_headers(tail)]


	defp encode_value(value) when is_integer(value), do: Kernel.integer_to_list(value)
	defp encode_value(value) when is_binary(value), do: value
	defp encode_value(value) when is_list(value), do: Kernel.list_to_bitstring(value)

	defp connection_token(Wubba.Request[version: {1, 1}, headers: headers]) do
	    case :proplists.get_value("Connection", headers) do
	        "close" -> "close"
	        "Close" -> "close"
	        _       -> "Keep-Alive"
	    end
	end

	defp connection_token(Wubba.Request[version: {1, 0}, headers: headers]) do
	    case :proplists.get_value("Connection", headers) do
	        "Keep-Alive" -> "Keep-Alive"
	        _                -> "close"
	    end
	end

	defp connection_token(_request), do: "close"

	defp close_or_keepalive(request, headers) do
	    case :proplists.get_value("Connection", headers) do
	        :undefined ->
	            case connection_token(request) do
	                "Keep-Alive" -> :keep_alive
	                "close"      -> :close
	            end
	        "close" -> :close
	        "Keep-Alive" -> :keep_alive
	    end
	end

	defp content_length(headers, body) do
	    case :proplists.is_defined("Content-Length", headers) do
	        true ->
	            []
	        false ->
	            {"Content-Length", iolist_size(body)}
	    end
	end

	defp execute_callback(Wubba.Request[callback: {module,arguments}] = request) do
	    try do
	    	case module.handle(request, arguments) do
		        {:ok, headers, {:file, file_name}}       -> {:file, 200, headers, file_name, {0, 0}}
		        {:ok, headers, {:file, file_name, range}}-> {:file, 200, headers, file_name, range}
		        {:ok, headers, body}                   -> {:response, 200, headers, body}
		        {:ok, body}                            -> {:response, 200, [], body}
		        {http_code, headers, {:file, file_name}} ->
		            {:file, http_code, headers, file_name, {0, 0}}
		        {http_code, headers, {:file, file_name, range}} ->
		            {:file, http_code, headers, file_name, range}
		        {http_code, headers, body}             -> {:response, http_code, headers, body}
		        {http_code, body}                      -> {:response, http_code, [], body}
		    end
	    catch
	        :throw, {http_code, headers, body} when is_integer(http_code) ->
	            {:response, http_code, headers, body}
	        _, _ ->
	            {:response, 500, [], "Internal server error"}
	    end
	end

	defp get_body(socket, headers, buffer, limits) do
	    case Dict.get(headers, "Content-Length") do
	        nil ->
	            {<<>>, buffer}
	        content_length_string ->
	        	clean_content_length = String.replace(content_length_string," ","",[:global])
	            content_length = Kernel.binary_to_integer(clean_content_length)
	            :ok = check_max_size(socket, content_length, buffer, limits)

	            case content_length - Kernel.byte_size(buffer) do
	                0 ->
	                    {buffer, <<>>}
	                n when n > 0 ->
	                    case :gen_tcp.recv(socket, n, limits[:body_timeout]) do
	                        {:ok, data} ->
	                            {buffer <> data, <<>>}
	                        {:error, _} ->
	                            :ok = :gen_tcp.close(socket)
	                            exit(:normal)
	                    end;
	                _ ->
	                    <<body :: [size(content_length), binary], rest :: binary>> = buffer
	                    {body, rest}
	            end
	    end
	end

	defp check_max_size(socket, content_length, buffer, limits) do
		max_body_size = limits[:max_body_size]
	    case content_length > max_body_size do
	        true ->
	            case content_length < max_body_size * 2 do
	                true ->
	                    remaining_bytes = content_length - size(buffer)
	                    :gen_tcp.recv(socket, remaining_bytes, 60000)
	                    response = ["HTTP/1.1 ", status(413), "\r\n",
	                                "Content-Length: 0", "\r\n\r\n"]
	                    :gen_tcp.send(socket, response)
	                    :gen_tcp.close(socket)
	                false ->
	                    :gen_tcp.close(socket)
	            end
	            exit(:normal)
	        false ->
	            :ok
	    end
	end

	defp get_request(socket, buffer, limits) do
	    case :erlang.decode_packet(:http_bin, buffer, []) do
	        {:more, _} ->
	            case :gen_tcp.recv(socket, 0, limits[:request_timeout]) do
	                {:ok, data} ->
	                    new_buffer = buffer <> data
	                    get_request(socket, new_buffer, limits)
	                {:error, _} ->
	                    :gen_tcp.close(socket)
	                    exit(:normal)
	            end
	        {:ok, {:http_request, method, raw_path, version}, rest} ->
	            {method, raw_path, version, rest}
	        {:ok, {:http_error, _}, _} ->
	            send_bad_request(socket)
	            :gen_tcp.close(socket)
	            exit(:normal)
	        {:ok, {:http_response, _, _, _}, _} ->
	            :gen_tcp.close(socket)
	            exit(:normal)
	    end
	end


	defp get_headers(_socket, {0, 9}, _, _), do: {[], <<>>}
	defp get_headers(socket, {1, _}, buffer, limits) do
	    get_headers(socket, buffer, [], 0, limits)
	end

	defp get_headers(socket, _, _headers, header_count, _limits) 
	  when header_count >= 100 do
	    send_bad_request(socket)
	    :gen_tcp.close(socket)
	    exit(:normal)
	end

	defp get_headers(socket, buffer, headers, header_count, limits) do
	    case :erlang.decode_packet(:httph_bin, buffer, []) do
	        {:ok, {:http_header, _, key, _, value}, rest} ->
	            new_headers = [{key, value} | headers]
	            get_headers(socket, rest, new_headers, header_count + 1, limits)
	        {:ok, :http_eoh, rest} ->
	            {headers, rest}
	        {:ok, {:http_error, _}, rest} ->
	            get_headers(socket, rest, headers, header_count, limits)
	        {:more, _} ->
	            case :gen_tcp.recv(socket, 0, limits[:header_timeout]) do
	                {:ok, data} ->
	                    get_headers(socket, buffer <> data, headers, header_count, limits)
	                {:error, _} ->
	                    :gen_tcp.close(socket)
	                    exit(:normal)
	            end
	    end
	end


	defp send_bad_request(socket) do
	    body = "Bad Request"
	    response = ["HTTP/1.1 ", status(400), "\r\n",
	               "Content-Length: ", integer_to_list(size(body)), "\r\n",
	                "\r\n"]
	    :gen_tcp.send(socket, response)
	end

	defp status(100),do: "100 Continue"
	defp status(101),do: "101 Switching Protocols"
	defp status(102),do: "102 Processing"
	defp status(200),do: "200 OK"
	defp status(201),do: "201 Created"
	defp status(202),do: "202 Accepted"
	defp status(203),do: "203 Non-Authoritative Information"
	defp status(204),do: "204 No Content"
	defp status(205),do: "205 Reset Content"
	defp status(206),do: "206 Partial Content"
	defp status(207),do: "207 Multi-defp Status"
	defp status(226),do: "226 IM Used"
	defp status(300),do: "300 Multiple Choices"
	defp status(301),do: "301 Moved Permanently"
	defp status(302),do: "302 Found"
	defp status(303),do: "303 See Other"
	defp status(304),do: "304 Not Modified"
	defp status(305),do: "305 Use Proxy"
	defp status(306),do: "306 Switch Proxy"
	defp status(307),do: "307 Temporary Redirect"
	defp status(400),do: "400 Bad Request"
	defp status(401),do: "401 Unauthorized"
	defp status(402),do: "402 Payment Required"
	defp status(403),do: "403 Forbidden"
	defp status(404),do: "404 Not Found"
	defp status(405),do: "405 Method Not Allowed"
	defp status(406),do: "406 Not Acceptable"
	defp status(407),do: "407 Proxy Authentication Required"
	defp status(408),do: "408 Request Timeout"
	defp status(409),do: "409 Conflict"
	defp status(410),do: "410 Gone"
	defp status(411),do: "411 Length Required"
	defp status(412),do: "412 Precondition Failed"
	defp status(413),do: "413 Request Entity Too Large"
	defp status(414),do: "414 Request-URI Too Long"
	defp status(415),do: "415 Unsupported Media Type"
	defp status(416),do: "416 Requested Range Not Satisfiable"
	defp status(417),do: "417 Expectation Failed"
	defp status(418),do: "418 I'm a teapot"
	defp status(422),do: "422 Unprocessable Entity"
	defp status(423),do: "423 Locked"
	defp status(424),do: "424 Failed Dependency"
	defp status(425),do: "425 Unordered Collection"
	defp status(426),do: "426 Upgrade Required"
	defp status(500),do: "500 Internal Server Error"
	defp status(501),do: "501 Not Implemented"
	defp status(502),do: "502 Bad Gateway"
	defp status(503),do: "503 Service Unavailable"
	defp status(504),do: "504 Gateway Timeout"
	defp status(505),do: "505 HTTP Version Not Supported"
	defp status(506),do: "506 Variant Also Negotiates"
	defp status(507),do: "507 Insufficient Storage"
	defp status(510),do: "510 Not Extended"
	defp status(b) when is_binary(b), do: b



end