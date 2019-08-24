
DOCKER_NETWORK = docker_public_net

all: shell

ssh-keygen:
	ssh-keygen -t rsa -b 2048 -f docker/secrets/id_rsa -N gbs321

shell: up
	docker run --network $(DOCKER_NETWORK) \
	  -v $(PWD):/code \
	  --mount type=bind,source="$(PWD)"/docker/secrets/id_rsa,target=/root/id_rsa,readonly \
	  -w /code \
	  -ti erlang:22-alpine /bin/sh

up:
	docker-compose -f docker/docker-compose.yml up -d

down:
	docker-compose -f docker/docker-compose.yml down
