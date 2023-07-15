FROM hexpm/erlang:25.3.2.3-alpine-3.18.2 AS erlang

WORKDIR /app

ENV REBAR_VERSION=3.22.0

FROM erlang AS downloader

RUN apk --no-cache add curl; \
    mkdir -p /tmp/tools; \
    curl -L https://github.com/erlang/rebar3/releases/download/${REBAR_VERSION}/rebar3 > /tmp/tools/rebar3; \
    chmod +x /tmp/tools/*

FROM erlang AS builder

COPY --from=downloader /tmp/tools/rebar3 rebar3
COPY rebar.config rebar.lock ./
RUN ./rebar3 get-deps

COPY src/ src/
RUN ./rebar3 escriptize

FROM erlang AS runner

COPY --from=builder /app/_build/default/lib/erl_exercism /opt/erl_exercism
COPY --from=builder /app/_build/default/bin/erlang_test_runner /opt/test-runner/bin/
COPY run.sh /opt/test-runner/bin/run.sh

ENV ERL_LIBS=/opt/erl_exercism

WORKDIR "/opt/test-runner"

ENTRYPOINT ["/opt/test-runner/bin/run.sh"]
