defmodule Icfp2020 do
  def run(args) do
    Application.ensure_all_started(:inets)
    Application.ensure_all_started(:ssl)

    args
    |> String.split(" ")
    |> case do
      [server_url, player_key] ->
        try_run(server_url, player_key)

      _ ->
        IO.puts("Expected two args: server_url player_key")
        System.halt(2)
    end
  end

  def try_run(server_url, player_key) do
    try do
      IO.puts("ServerUrl: #{server_url}; PlayerKey: #{player_key}")

      {:ok, {{_, status_code, _}, _headers, body}} =
        :httpc.request(:post, {server_url |> to_charlist, [], [], player_key}, [], [])

      if status_code != 200 do
        IO.puts("Unexpected server response:")
        IO.puts("HTTP code: #{status_code}")
        IO.puts("Response body: #{body}")
        System.halt(2)
      end

      IO.puts("Server response: #{body}")
    rescue
      e ->
        IO.puts("Unexpected server response:")
        IO.puts(Exception.format(:error, e, __STACKTRACE__))
        System.halt(1)
    end
  end
end
