all: setup test

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
	cd scheduler && { clj -m scheduler.core & echo $$! > /tmp/server.PID; } && sleep 7

stop-scheduler:
	cd scheduler && kill $(shell cat /tmp/server.PID) && rm /tmp/server.PID

run-test-sut:
	cd sut && go test

setup: create-db

test-sut: run-scheduler run-test-sut stop-scheduler

test: test-scheduler test-executor test-sut
