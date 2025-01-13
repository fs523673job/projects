from locust import HttpUser, TaskSet, task, between
import json
import os

class MyTaskSet(TaskSet):
    def on_start(self):
        with open("add_direct_con_ocorrencias.json", "r", encoding="utf-8") as f:
            self.payload = json.load(f)

    @task
    def send_request(self):
        headers = {
            "Content-Type": "application/json"
        }
        
        self.client.post("/REST/API.APDATA.V1/ADDDIRECT",
                         data=json.dumps(self.payload),
                         headers=headers)

class MyLoadTest(HttpUser):
    wait_time = between(1, 5)
    tasks = [MyTaskSet]
