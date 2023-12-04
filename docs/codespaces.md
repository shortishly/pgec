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

Change directory to be within the repository:

```shell
cd pgec
```

Create a codespace for pgec:

```shell
./bin/codespace-create
```

Run a secure shell into the codespace, you may see `Starting
codespace` while it is provisioned (which can take a while):

```shell
./bin/codespace-ssh
```

## Running

Once in the codespace secure shell, you can check that
[PostgreSQL][postgresql-org], [Prometheus][prometheus-io],
[Grafana][grafana-com] and pgec are all running with:

```shell
./bin/ps
```

The services may still be provisioning after you have logged in. If
you see:

```shell
@shortishly ➜ /workspaces/pgec (develop) $ ./bin/ps
SERVICE   IMAGE     STATUS
```

Wait a couple of minutes until:

```shell
@shortishly ➜ /workspaces/pgec (develop) $ ./bin/ps
SERVICE          IMAGE                             STATUS
db               postgres:16.1                     Up 5 minutes (healthly)
grafana          grafana/grafana:10.1.5            Up 5 minutes
load-generator   postgres:16.1                     Up 5 minutes
pgec             ghcr.io/shortishly/pgec:develop   Up 5 minutes
prometheus       prom/prometheus:v2.47.2           Up 5 minutes
```

To run the smoke tests against the example data:

```shell
@shortishly ➜ /workspaces/pgec (develop) $ bats test/bats
```

The service ports are [published and
forwarded][codespace-port-forward] by the codespace. In a terminal on
your local machine you can look at the Grafana running remotely on the
codespace with sample dashboards installed with:

```shell
./bin/codespace-grafana
```

## Building

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
[grafana-com]: https://grafana.com
[postgresql-org]: https://www.postgresql.org/
[prometheus-io]: https://prometheus.io
[codespace-port-forward]: https://docs.github.com/en/codespaces/developing-in-a-codespace/forwarding-ports-in-your-codespace
