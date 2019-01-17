curl -s -X GET --header 'Accept: application/json' 'http://127.0.0.1:9085/debug/briefchain' > 0.json
curl -s -X GET --header 'Accept: application/json' 'http://127.0.0.2:9089/debug/briefchain' >> 0.json
curl -s -X GET --header 'Accept: application/json' 'http://127.0.0.3:9093/debug/briefchain' >> 0.json
sleep 10
curl -s -X GET --header 'Accept: application/json' 'http://127.0.0.1:9085/debug/briefchain' > 1.json
curl -s -X GET --header 'Accept: application/json' 'http://127.0.0.2:9089/debug/briefchain' >> 1.json
curl -s -X GET --header 'Accept: application/json' 'http://127.0.0.3:9093/debug/briefchain' >> 1.json
sleep 10
curl -s -X GET --header 'Accept: application/json' 'http://127.0.0.1:9085/debug/briefchain' > 2.json
curl -s -X GET --header 'Accept: application/json' 'http://127.0.0.2:9089/debug/briefchain' >> 2.json
curl -s -X GET --header 'Accept: application/json' 'http://127.0.0.3:9093/debug/briefchain' >> 2.json

