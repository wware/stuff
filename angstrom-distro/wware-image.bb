require console-base-image.bb

DEPENDS += "task-base-extended \
	   "

IMAGE_INSTALL += "task-base-extended \
	python \
	helloworld \
	quux-module \
	    "

export IMAGE_BASENAME = "wware-image"
