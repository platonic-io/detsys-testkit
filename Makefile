create-db:
	cd db && ./db.sh init

clean:
	cd db && ./db.sh drop_tables

test-scheduler:
	cd scheduler && clj "-A:test"

test-executor:
	cd executor && go test

.PHONY: run-scheduler
run-scheduler:
	cd scheduler && { clj -m scheduler.core & echo $$! > server.PID; } && sleep 7

stop-scheduler:
	cd scheduler && kill `cat server.PID` && rm server.PID

run-test-sut:
	cd sut && go test

# this should probably move to dummy-test
generate-test:
	cd generator && ./generator.sh

setup: create-db generate-test

test-sut: run-scheduler run-test-sut stop-scheduler

test: test-scheduler test-executor test-sut
