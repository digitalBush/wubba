defmodule Wubba.Utils do

	def make_options(overrides,defaults) do
		keys = Keyword.keys(defaults)
		matching = Keyword.take(overrides,keys)
		Keyword.merge(defaults,matching)
	end
	
end