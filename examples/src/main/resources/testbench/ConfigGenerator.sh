#!/bin/bash
my_dir="$(dirname "$0")"
display_usage() {
	echo "This script takes a network topology file and generate the configure file for each node."
	echo -e "\nUsage:\nScript [topology file name (no dir needed)] \n"
    echo -e "Topology file should be in the same dir as the script.\nTopology file format: each line should start with an integer i>=1, with the following content\n\ti: p1,p2,...\nwhere p1,p2... are its known peers. We recommend using i=1,2,3... incrementally. A constraint is a peer in earlier line should not know a later peer since we are starting peers in that order?\n\nAn example of topology file:\n\t1: \n\t2:1\n\t3:1,2"
	}
if [ "$#" -le "0" ]; then
	display_usage
	exit 1
fi
while read -r line || [[ -n "$line" ]]; do
    IFS=':, ' read -r -a array <<< "$line"
    if [ "${#array[@]}" -ge "1" ]; then
        conf00="scorex {\n  dataDir = /tmp/scorex/data${array[0]}/blockchain\n logDir = /tmp/scorex/data${array[0]}/log\n\n restApi {\n bindAddress = \"127.0.0.${array[0]}:9085\"\n api-key-hash = \"\"\n}\n\n network {\n  nodeName = \"generatorNode${array[0]}\"\n bindAddress = \"127.0.0.${array[0]}:9084\"\n knownPeers = ["
        peers=""
        for peer in "${array[@]: 1}"; do
            printf -v peers "$peers, \"127.0.0.$peer:9084\""
        done
        peers="${peers#,}"
        conf01="]\n agentName = \"2-Hop\"\n}\n miner {\n offlineGeneration = "
        if [ "${array[0]}" -eq "1" ]; then
            booleanvalue="true"
        else
            booleanvalue="false"
        fi
        #offlineGeneration = true or false?
        conf02="\n targetBlockDelay = 5s\n blockGenerationDelay = 100ms\n rParamX10 = 8\n initialDifficulty = 1\n posAttachmentSize = 100\n }\n wallet {\n seed = \"minerNode${array[0]}\"\n password = \"cookies${array[0]}\"\n walletDir = \"/tmp/scorex/data${array[0]}/wallet\"\n }\n }\n"
        printf "$conf00$peers$conf01$booleanvalue$conf02" > "$my_dir/settings${array[0]}.conf"
    fi
done < "$my_dir/$1"

#sbt "; project examples; runMain examples.hybrid.HybridApp src/main/resources/settings.conf"
