package lib

import (
	"bytes"
	// "encoding/json"
	"fmt"
	"io/ioutil"
	"net/http"
)

const schedulerUrl string = "http://localhost:3000"

func Execute(testId int) {

	//json, err := json.Marshal(map[string]string{"command": "status",
	//	"parameters": nil})
	//if err != nil {
	//	panic(err)
	//}
	body := bytes.NewBufferString(`{"command": "status", "parameters": {}}`)

	resp, err := http.Post(schedulerUrl, "application/json", body)
	if err != nil {
		panic(err)
	}
	defer resp.Body.Close()
	body2, _ := ioutil.ReadAll(resp.Body)
	fmt.Println(string(body2))
	// var data map[string]string
	// err := json.Unmarshal(resp.Body, &data)
	// fmt.Printf("Response: %s", data)
}
