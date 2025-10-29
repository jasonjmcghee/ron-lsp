return {
	{
		"neovim/nvim-lspconfig",
		opts = function(_, opts)
			local configs = require("lspconfig.configs")

			if not configs.ron_lsp then
				configs.ron_lsp = {
					default_config = {
						cmd = { vim.fn.expand("ron-lsp") },
						filetypes = { "ron" },
						root_dir = function(fname)
							local util = require("lspconfig.util")
							return util.root_pattern("Cargo.toml", ".git")(fname) or vim.loop.cwd()
						end,
						settings = {},
					},
				}
			end

			opts.servers = opts.servers or {}
			opts.servers.ron_lsp = {
				-- Add any specific on_attach, capabilities, or handlers here if needed
			}

			return opts
		end,
	},
}
