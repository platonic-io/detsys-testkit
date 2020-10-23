package lib

import (
	"bytes"
	"encoding/json"
	"io/ioutil"
	"log"
	"net/http"
)

type TestId struct {
	TestId int `json:"test-id"`
}

const schedulerUrl string = "http://localhost:3000"

type SchedulerRequest struct {
	Command    string      `json:"command"`
	Parameters interface{} `json:"parameters"`
}

func Post(command string, parameters interface{}) []byte {
	json, err := json.Marshal(SchedulerRequest{
		Command:    command,
		Parameters: parameters})
	if err != nil {
		log.Panicln(err)
	}
	resp, err := http.Post(schedulerUrl, "application/json", bytes.NewBuffer(json))
	if err != nil {
		log.Panicln(err)
	}
	defer resp.Body.Close()
	body, err := ioutil.ReadAll(resp.Body)
	if err != nil {
		log.Panicln(err)
	}
	if resp.StatusCode != 200 {
		log.Panicln(string(body))
	}
	return body
}

func PostParse(command string, parameters interface{}, target interface{}) {
	body := Post(command, parameters)
	if err := json.Unmarshal(body, &target); err != nil {
		log.Panicln(err)
	}
}
