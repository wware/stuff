#!/bin/bash

SOURCE_ISO=ubuntu-mini-remix-16.04-i386.iso
SRC_ISO_URL=http://www.ubuntu-mini-remix.org/download/16.04/${SOURCE_ISO}
SRC_ISO_COPY=${HOME}/${SOURCE_ISO}

ROOT=${HOME}/livecdtmp
CHROOT_JAIL=${ROOT}/edit
DISK_IMAGE=${ROOT}/extract-cd
ISO_MOUNT=${ROOT}/mnt

TARGET_ISO=${HOME}/customized-umr-16.04-i386.iso

# First fetch the ISO if we don't already have it
if [ ! -f ${SRC_ISO_COPY} ]
then
    curl ${SRC_ISO_URL} > ${SRC_ISO_COPY} || exit 1
fi

EXPECTED_MD5=303abd1dd70907bf904174022818abe7
MD5=$(md5sum ${SRC_ISO_COPY} | cut -c -32)
if [ X${MD5} != X${EXPECTED_MD5} ]
then
    echo Bad MD5 for ${SRC_ISO_COPY}
    echo Expected ${EXPECTED_MD5} but instead got ${MD5}
    exit 1
fi

for x in $@
do
    if [ ! -f ${x} ]
    then
        echo "Cannot find deb file ${x}"
        exit 0
    fi
    MAYBE_DEB=$(echo ${x} | sed "s/^.*\.//")
    if [ X${MAYBE_DEB} != Xdeb ]
    then
	echo ${MAYBE_DEB}
        echo "This doesn't look like a deb file: ${x}"
        exit 0
    fi
done

rm -f ${TARGET_ISO} || exit 1

pushd . || exit 1

sudo rm -rf ${ROOT} || exit 1
mkdir -p ${ISO_MOUNT} ${DISK_IMAGE} || exit 1

# mount the iso file
sudo mount -o loop ${SRC_ISO_COPY} ${ISO_MOUNT} || exit 1

# Extract .iso contents into dir 'extract-cd'
rsync --exclude=/casper/filesystem.squashfs -a ${ISO_MOUNT} ${DISK_IMAGE} || exit 1

# Extract the Desktop system
# Extract the SquashFS filesystem
(cd ${ROOT} || exit 1
sudo unsquashfs ${ISO_MOUNT}/casper/filesystem.squashfs || exit 1
sudo umount ${ISO_MOUNT} || exit 1
sudo mv ${ROOT}/squashfs-root/ ${CHROOT_JAIL} || exit 1)

# Prepare to chroot
sudo cp /etc/resolv.conf ${CHROOT_JAIL}/etc || exit 1
sudo cp /etc/apt/sources.list ${CHROOT_JAIL}/etc/apt/sources.list || exit 1

# Install the debian packages inside the chroot jail
for x in $@
do
    RAW_NAME=$(echo ${x} | sed "s#^.*/##")
    sudo cp ${x} ${CHROOT_JAIL} || exit 1
    sudo chroot ${CHROOT_JAIL} dpkg -i /${RAW_NAME} || exit 1
    sudo rm -f ${CHROOT_JAIL}/${RAW_NAME} || exit 1
done

if [ X${ABORT_CD} != X ]
then
    echo Aborting before CD image production
    echo Disk image without squashed fs is at ${DISK_IMAGE}
    echo Chroot jail is at ${CHROOT_JAIL}
    exit 0
fi

# Producing the CD image
# Assembling the file system
# Regenerate manifest
chmod +w ${DISK_IMAGE}/mnt/casper/filesystem.manifest || exit 1
sudo chroot ${CHROOT_JAIL} dpkg-query -W \
     --showformat='${Package} ${Version}\n' \
     > ${DISK_IMAGE}/mnt/casper/filesystem.manifest || exit 1

# Compress filesystem
sudo rm ${DISK_IMAGE}/mnt/casper/filesystem.squashfs || exit 1
sudo mksquashfs ${CHROOT_JAIL} ${DISK_IMAGE}/mnt/casper/filesystem.squashfs || exit 1

# Remove old md5sum.txt and calculate new md5 sums
cd ${DISK_IMAGE}/mnt || exit 1
sudo rm md5sum.txt || exit 1
find -type f -print0 | sudo xargs -0 md5sum | \
    grep -v isolinux/boot.cat | sudo tee md5sum.txt || exit 1
# always choose the "run Linux without installing" option immediately, but 1 is too small
sudo sed -i 's/timeout 50/timeout 1/' ${DISK_IMAGE}/mnt/isolinux/isolinux.cfg || exit 1

# Create the ISO image
sudo mkisofs -D -r -V "Modified mini image" -cache-inodes -J -l \
    -b isolinux/isolinux.bin -c isolinux/boot.cat -no-emul-boot \
    -boot-load-size 4 -boot-info-table -o ${TARGET_ISO} . || exit 1

popd || exit 1
sudo chown ${USER}:${USER} ${TARGET_ISO} || exit 1
sudo rm -rf ${ROOT} || exit 1
