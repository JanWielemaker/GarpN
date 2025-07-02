RESTART=--restart unless-stopped
VOLUME=$(shell pwd)/data
PORT=3510

PUBLISH=--publish=${PORT}:3510
MOUNT=--volume=$(shell dirname `pwd`)/numeric:/garpn/numeric \
      --volume=$(shell dirname `pwd`)/tests:/garpn/tests
DOPTS=${PUBLISH} ${MOUNT}
IMG=garpn
SRV=garpn

all:
	@echo "Targets"
	@echo
	@echo "image            Build the plweb image"
	@echo "install          Run the image (detached)"
	@echo "run              Run the image (interactive)"
	@echo "restart          Stop and restart the image"

image::
	docker build --build-context garpn=.. -t $(IMG) .

install:
	docker run --name=$(SRV) -d ${RESTART} ${DOPTS} $(IMG)

run:
	docker run -it --rm ${DOPTS} $(IMG)

stop:
	docker stop $(SRV)

restart:
	-docker stop $(SRV)
	-docker rm $(SRV)
	make install

bash:
	docker run -it ${DOPTS} $(IMG) --bash

