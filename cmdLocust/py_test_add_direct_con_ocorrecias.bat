@echo off

locust -f test_add_direct_con_ocorrencias.py --host http://localhost:7081 

timeout /t 2 /nobreak >nul

start "" "C:\Program Files\Google\Chrome\Application\chrome.exe" http://localhost:8089
