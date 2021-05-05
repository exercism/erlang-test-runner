FROM hexpm/erlang:22.3.4.12-ubuntu-focal-20200703 as ERLANG

WORKDIR /app

FROM ERLANG as DOWNLOADER

RUN apt update; apt install --yes curl; \
    mkdir -p /tmp/tools; \
    curl -L https://github.com/erlang/rebar3/releases/download/3.14.4/rebar3 > /tmp/tools/rebar3; \
    chmod +x /tmp/tools/*

FROM ERLANG as BUILDER

COPY --from=DOWNLOADER /tmp/tools/rebar3 rebar3
COPY rebar.config rebar.lock ./
RUN ./rebar3 get-deps

COPY src/ src/
RUN ./rebar3 escriptize; find . -type f -executable

FROM ERLANG as RUNNER

copy --from=BUILDER /app/_build/default/lib/erl_exercism /opt/erl_exercism
COPY --from=BUILDER /app/_build/default/bin/erlang_test_runner /opt/test-runner/bin/
COPY run.sh /opt/test-runner/bin/run.sh

ENV ERL_LIBS=/opt/erl_exercism

WORKDIR "/opt/test-runner"

ENTRYPOINT ["/opt/test-runner/bin/run.sh"]
