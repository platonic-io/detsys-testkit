SHELL := /usr/bin/env bash

all: setup test

create-db:
	cd db && ./db.sh init

clean:
	cd db && ./db.sh drop_tables

test-scheduler:
	cd scheduler && clj "-A:test"

test-executor:
	cd executor && go test

.PHONY: start-scheduler
start-scheduler:
	cd scheduler && { clj -m scheduler.core & echo $$! > /tmp/server.PID; }

run-scheduler: start-scheduler wait-scheduler

wait-scheduler:
	until $$(curl --silent --output /dev/null -H "Content-Type: application/json" \
								--fail -X POST --data '{"command": "status", "parameters": {}}' \
								"http://localhost:3000/") ; do \
    printf '.' ; \
		sleep 2 ; \
	done

stop-scheduler:
	cd scheduler && kill $(shell cat /tmp/server.PID) && rm /tmp/server.PID

run-test-sut:
	cd sut && go test

setup: create-db

test-sut: run-scheduler run-test-sut stop-scheduler

test: test-scheduler test-executor test-sut
