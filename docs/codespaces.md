# Codespaces

You can create a [GitHub Codespace][github-com-codespaces] for pgec
that has PostgreSQL and Prometheus setup with [instructions from
GitHub on creating a codespace][gihub-com-cacfar]. Some CLI
convenience snippets follow.

Firstly, follow the [GitHub CLI installation
instructions][github-com-installation] for your OS.

Clone the pgec repository:

```shell
gh repo clone shortishly/pgec
```

Create a codespace for pgec:

```shell
gh codespace create \
    --repo $(gh repo view \
        --json nameWithOwner \
        --jq .nameWithOwner) \
    --branch develop \
    --machine basicLinux32gb
```

Run a secure shell into the codespace:

```shell
gh codespace ssh \
    --codespace $(gh codespace ls \
        --repo $(gh repo view \
            --json nameWithOwner \
            --jq .nameWithOwner) \
        --json name \
        --jq '.[].name')
```

Once in the secure shell environment, you can check that
[PostgreSQL][postgresql-org] and [Prometheus][prometheus-io] are
running OK:

```shell
docker compose --profile all ps
```

To build pgec, [dialyze][erlang-org-dialyzer] and run the
[unit][erlang-org-eunit-ug] tests:

```shell
make deps app dialyze eunit
```

To run the [common tests][erlang-org-ctug] against the
[PostgreSQL][postgresql-org] already running in the
[codespace][github-com-codespaces]:

```shell
make ct
```

To run an interactive Erlang/OTP shell with the pgec application running:

```shell
make shell
```

Once you are finished you can delete the [codespace][github-com-codespaces]
using:

```shell
gh codespace delete \
    --codespace $(gh codespace ls \
        --repo $(gh repo view \
            --json nameWithOwner \
            --jq .nameWithOwner) \
        --json name \
        --jq '.[].name')
```

[erlang-org-ctug]: https://www.erlang.org/doc/apps/common_test/users_guide.html
[erlang-org-dialyzer]: https://www.erlang.org/doc/man/dialyzer.html
[erlang-org-eunit-ug]: https://www.erlang.org/doc/apps/eunit/users_guide.html
[gihub-com-cacfar]: https://docs.github.com/en/codespaces/developing-in-codespaces/creating-a-codespace-for-a-repository
[github-com-codespaces]: https://docs.github.com/en/codespaces
[github-com-installation]: https://cli.github.com/manual/installation
[postgresql-org]: https://www.postgresql.org/
[prometheus-io]: https://prometheus.io
