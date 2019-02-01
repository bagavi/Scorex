examples/src/main/resources/testbench/ConfigGenerator.sh topology.txt
examples/src/main/resources/testbench/ConfigRunner.sh topology.txt
echo "running, terminate after 60s"
sleep 60
examples/src/main/resources/testbench/Killer.sh
