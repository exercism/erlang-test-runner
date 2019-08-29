FROM erlang:22.0.7-alpine

COPY run.sh /opt/test-runner/bin/run.sh

WORKDIR "/opt/test-runner"

ENTRYPOINT ["/opt/test-runner/bin/run.sh"]
