defmodule Icfp2020.ParserTest do
  alias Icfp2020.Parser

  use ExUnit.Case

  test "parse" do
    assert :boom == Parser.parse("boom")
  end
end
