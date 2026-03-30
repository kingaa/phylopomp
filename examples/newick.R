simulate("SIIR",time=0.3,sigma12=1) -> x

x |> plot(prune=FALSE,obscure=FALSE)

x |> newick(prune=FALSE,obscure=FALSE)

x |> newick()

x |> newick(extended=FALSE)

x |>
  newick() |>
  parse_newick(t0=0)

x |>
  newick(prune=FALSE,obscure=FALSE) |>
  parse_newick(t0=0)

x |>
  newick(extended=FALSE) |>
  parse_newick(t0=0)

"" |> parse_newick() |> newick()

";" |> parse_newick() |> newick()

"((:1,:1):1,:1):1;" |> parse_newick() |> newick()
