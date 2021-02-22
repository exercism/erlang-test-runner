FROM hexpm/erlang:22.3.4.12-ubuntu-focal-20200703 as ERLANG

COPY run.sh /opt/test-runner/bin/run.sh

WORKDIR "/opt/test-runner"

ENTRYPOINT ["/opt/test-runner/bin/run.sh"]
