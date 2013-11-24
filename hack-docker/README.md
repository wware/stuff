Docker
======

This directory includes a few scripts I've found handy for this, in addition to
these links and instructions.

* http://willware.blogspot.com/2013/11/whats-up-with-docker.html
* http://docs.docker.io/en/master/installation/ubuntulinux/
* http://docs.docker.io/en/master/use/basics/
* http://docs.docker.io/en/master/examples/python_web_app/#python-web-app
* http://stackoverflow.com/questions/17989306/

I went into an Ubuntu 12.04 virtual machine and did the following things as root.
Install prerequisites, reboot, install Docker.

    apt-get update
    apt-get install linux-image-generic-lts-raring linux-headers-generic-lts-raring
    reboot

    # Add the Docker repository key to your local keychain
    # using apt-key finger you can check the fingerprint matches
    # 36A1 D786 9245 C895 0F96 6E92 D857 6A8B A88D 21E9
    sudo sh -c "wget -qO- https://get.docker.io/gpg | apt-key add -"
    # Add the Docker repository to your apt sources list.
    sudo sh -c "echo deb http://get.docker.io/ubuntu docker main\
        > /etc/apt/sources.list.d/docker.list"
    # Update your sources
    sudo apt-get update
    # Install, you will see another warning that the package cannot be authenticated. Confirm install.
    sudo apt-get install lxc-docker

Now set up the guy's Python app example, which is a very small Flask app that sends back
"Hello world" when you do a HTTP GET to the port it's running on, which is 5000 for Flask.

    # Start with his standard python app builder
    docker pull shykes/pybuilder
    # Here's a tarball for the hello-world app, get the app and build it
    URL=http://github.com/shykes/helloflask/archive/master.tar.gz
    BUILD_JOB=$(sudo docker run -d -t shykes/pybuilder:latest /usr/local/bin/buildapp $URL)
    # Attach stdin/stdout to the app, proxify all received signal to the process
    docker attach -sig-proxy=false $BUILD_JOB
    # When the build is finished, grab a snapshot/image and store it with a long name
    BUILD_IMG=$(sudo docker commit $BUILD_JOB _/builds/github.com/shykes/helloflask/master)

At this point, you've built the server image that you're going to use, and it's just a matter
of running one or many instances of it.

    # Run the image as a background process
    WEB_WORKER=$(sudo docker run -d -p 5000 $BUILD_IMG /usr/local/bin/buildapp)
    docker logs $WEB_WORKER
    # Connect the image's port 5000 to a port on the host OS
    WEB_PORT=$(sudo docker port $WEB_WORKER 5000 | awk -F: '{ print $2 }')
    # Make sure everything worked
    apt-get install curl
    curl http://127.0.0.1:$WEB_PORT

Adding to existing images
-------------------------

Given an existing image, you can run /bin/bash, go in there, install stuff with
`apt-get`, and use `docker commit` to save the result as a new image.

    NEW_IMAGE=$(sudo docker run -d -t existing_image /bin/bash)
    [existing_image]$ apt-get install redis-server python-redis
    [existing_image]$ exit
    sudo docker commit $NEW_IMAGE my_new_image

Adding to existing images
-------------------------

You can create a new image from scratch. One approach is to start with an empty
Ubuntu distribution.

* http://docs.docker.io/en/latest/use/baseimages/
* http://stackoverflow.com/questions/18274088/

(Can you do this with other distributions? I'm guessing some dev-ops guys would
prefer CentOS.)

    sudo debootstrap precise precise > /dev/null
    NEWIMAGE=$(sudo tar -C precise -c . | sudo docker import -)
    sudo docker tag $NEWIMAGE my_new_image

Once this is done, you can give the image a name (document how that's done) and
then use `docker commit` to incrementally build images with the pieces you want
in them.

Creating a fresh image with a Dockerfile
----------------------------------------

The best way to create a fresh image is with `docker build` and a Dockerfile.

* http://docs.docker.io/en/latest/use/builder/

I've provided a small example in the wware-runapp directory. It does `apt-get
install` of a few packages, and puts a script in the `/bin` directory. To make
it go, type:

    sudo docker build -t wware/runapp wware-runapp

and a `wware/runapp` image will be created.

Using the stuff in the wware-runapp directory
---------------------------------------------

Let's suppose that we have a tarball containing a Flask app (which will run on
port 5000 of each Docker instance) and we want to create four Docker instances,
and have them on ports 80 through 83, all running the same Flask app. Then we
could do this. (NB: `seq` in Bash starts counting from _1_, not 0 like most
other software county things.)

    N=4
    for i in $(seq 0 $(($N - 1)))
    do
       PORT=$((80 + $i))
       sudo docker run -d -t -p $PORT:5000 \
           wware-runapp /bin/runapp.sh $URL $i $N
    done

Because we pass `$i` and `$N` as arguments to the `run.sh` script in the
tarball, we can customize each instance's behavior to be cognizant of its place
in the system.

It's generally a good idea to minimize network traffic in big parallel
computations such as molecular modeling. The values of `$i` and `$N` might be
used to assign each machine responsibility for a certain region of XYZ space,
and to make predictable routing when some piece of information must be sent
from one point in space to another.

Flask to serve static files
---------------------------

First get some prerequisites set up on the host machine.

    sudo apt-get install python-flask python-setuptools
    sudo easy_install pip virtualenv docopt

Now you can use `flask-serve.sh` to serve static files. Just run it with no
arguments. Then in another shell (assuming the host machine is at 192.168.1.4),
type:

    $ sudo docker run -i -t wware/runapp /bin/runapp.sh \
                              http://192.168.1.4:5000/example-tarball.tar.gz
      % Total    % Received % Xferd  Average Speed   Time    Time     Time  Current
                                     Dload  Upload   Total   Spent    Left  Speed
    100   219  100   219    0     0   8052      0 --:--:-- --:--:-- --:--:--  9125
    example/
    example/build.sh
    This is just an example, but it works!
    HAPPY HAPPY JOY JOY!!
