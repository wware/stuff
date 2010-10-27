#!/bin/bash

DVD=/mnt/dvd
DEST=/root/livecd

if [ ! -d ${DVD}/slackware ] || [ ! -d ${DVD}/kernels ] || \
   [ ! -d ${DVD}/isolinux ]
then
  echo Cannot find Slackware DVD
  exit 1
fi

rm -rf ${DEST}
#mkdir -p ${DEST}/isolinux ${DEST}/kernels ${DEST}/live
mkdir -p ${DEST}/live
installpkg -root ${DEST}/live/ ${DVD}/slackware/a/*.t?z
installpkg -root ${DEST}/live/ ${DVD}/slackware/ap/*.t?z
installpkg -root ${DEST}/live/ ${DVD}/slackware/d/*.t?z
installpkg -root ${DEST}/live/ ${DVD}/slackware/n/*.t?z

# the following is equivalent to
#   installpkg -root ${DEST}/live/ ${DVD}/slackware/l/*.t?z
# except that we skip some large and irrelevant libraries
#    PyQt-4.7.3-i486-1.txz \
#    QScintilla-2.4.3-i486-1.txz \
#    alsa-lib-1.0.23-i486-1.txz \
#    alsa-oss-1.0.17-i486-1.txz \
#    aspell-0.60.5-i486-2.txz \
#    aspell-en-6.0_0-noarch-4.txz \
#    esound-0.2.41-i486-1.txz \
#    freetype-2.3.12-i486-1.txz \
#    gd-2.0.35-i486-4.txz \
#    gnome-icon-theme-2.30.2-i486-1.txz \
#    gtk+-1.2.10-i486-5.txz \
#    gtk+2-2.18.9-i486-1.txz \
#    gtkspell-2.0.15-i486-1.txz \
#    hicolor-icon-theme-0.12-noarch-1.txz \
#    hunspell-1.2.9-i486-1.txz \
#    icon-naming-utils-0.8.90-i486-2.txz \
#    lesstif-0.95.2-i486-1.txz \
#    libglade-2.6.4-i486-4.txz \
#    libogg-1.1.4-i486-1.txz \
#    libtheora-1.1.1-i486-1.txz \
#    libvorbis-1.2.3-i486-1.txz \
#    phonon-4.4.1-i486-1.txz \
#    pil-1.1.7-i486-1.txz \
#    pycups-1.9.48-i486-1.txz \
#    pygtk-2.16.0-i486-1.txz \
#    qimageblitz-r948358-i486-1.txz \
#    qt-4.6.2_2d3d3e5-i486-1.txz \
#    qtscriptgenerator-0.1.0-i486-2.txz \
#    system-config-printer-1.2.2-i486-1.txz \
#    tango-icon-theme-0.8.90-noarch-1.txz \
#    tango-icon-theme-extras-0.1.0-noarch-1.txz \
#    wavpack-4.41.0-i486-1.txz \
#    wv2-0.4.2-i486-1.txz \

for x in \
    ConsoleKit-20100129-i486-1.txz \
    M2Crypto-0.19.1-i486-2.txz \
    aalib-1.4rc5-i486-2.txz \
    akonadi-1.3.1-i486-1.txz \
    apr-1.3.9-i486-1.txz \
    apr-util-1.3.9-i486-2.txz \
    atk-1.30.0-i486-1.txz \
    attica-0.1.3-i486-1.txz \
    audiofile-0.2.6-i486-2.txz \
    automoc4-0.9.88-i486-1.txz \
    babl-0.1.0-i486-1.txz \
    boost-1.42.0-i486-1.txz \
    cairo-1.8.8-i486-3.txz \
    chmlib-0.39-i486-1.txz \
    clucene-0.9.21b-i486-1.txz \
    db42-4.2.52-i486-3.txz \
    db44-4.4.20-i486-2.txz \
    dbus-glib-0.86-i486-1.txz \
    dbus-python-0.83.1-i486-1.txz \
    desktop-file-utils-0.16-i486-2.txz \
    djvulibre-3.5.22-i486-2.txz \
    ebook-tools-0.1.1-i486-2.txz \
    eggdbus-0.6-i486-1.txz \
    eigen2-2.0.10-i486-1.txz \
    enchant-1.5.0-i486-1.txz \
    exiv2-0.18.2-i486-1.txz \
    expat-2.0.1-i486-1.txz \
    fftw-3.2.2-i486-1.txz \
    fribidi-0.10.9-i486-2.txz \
    fuse-2.8.1-i486-1.txz \
    gamin-0.1.10-i486-2.txz \
    gdbm-1.8.3-i486-4.txz \
    gegl-0.1.0-i486-2.txz \
    giflib-4.1.6-i486-1.txz \
    glib-1.2.10-i486-3.txz \
    glib2-2.22.5-i486-1.txz \
    glibc-2.11.1-i486-3.txz \
    glibc-i18n-2.11.1-i486-3.txz \
    glibc-profile-2.11.1-i486-3.txz \
    gmime-2.4.15-i486-1.txz \
    gmm-3.1-noarch-1.txz \
    gmp-5.0.1-i486-1.txz \
    gst-plugins-base-0.10.29-i486-1.txz \
    gst-plugins-good-0.10.22-i486-1.txz \
    gstreamer-0.10.29-i486-1.txz \
    hal-0.5.14-i486-2.txz \
    hal-info-20091130-noarch-1.txz \
    ilmbase-1.0.1-i486-1.txz \
    imlib-1.9.15-i486-6.txz \
    iso-codes-3.8-noarch-1.txz \
    jasper-1.900.1-i486-3.txz \
    jre-6u20-i586-1.txz \
    lcms-1.19-i486-1.txz \
    libaio-0.3.109-i486-1.txz \
    libao-0.8.8-i486-1.txz \
    libarchive-2.8.0-i486-1.txz \
    libart_lgpl-2.3.20-i486-1.txz \
    libcaca-0.99.beta16-i486-3.txz \
    libcap-2.19-i486-1.txz \
    libcddb-1.3.0-i486-1.txz \
    libcdio-0.79-i486-1.txz \
    libdiscid-0.2.2-i486-1.txz \
    libdvdread-4.1.3-i486-1.txz \
    libexif-0.6.19-i486-1.txz \
    libgphoto2-2.4.9-i486-1.txz \
    libgpod-0.7.2-i486-1.txz \
    libgsf-1.14.16-i486-1.txz \
    libical-0.43-i486-1.txz \
    libid3tag-0.15.1b-i486-3.txz \
    libidl-0.8.10-i486-1.txz \
    libidn-1.5-i486-1.txz \
    libieee1284-0.2.11-i486-2.txz \
    libiodbc-3.52.7-i486-2.txz \
    libjpeg-v8a-i486-1.txz \
    libkarma-0.1.1-i486-1.txz \
    liblastfm-0.3.0-i486-2.txz \
    libmad-0.15.1b-i486-3.txz \
    libmcrypt-2.5.8-i486-1.txz \
    libmcs-0.7.1-i486-1.txz \
    libmng-1.0.10-i486-2.txz \
    libmowgli-0.6.0-i486-1.txz \
    libmsn-4.1-i486-1.txz \
    libmtp-1.0.1-i486-2.txz \
    libnjb-2.2.6-i486-3.txz \
    libnl-1.1-i486-1.txz \
    libnotify-0.4.5-i486-2.txz \
    liboil-0.3.16-i486-1.txz \
    libpng-1.4.2-i486-1.txz \
    libraw1394-2.0.5-i486-1.txz \
    librsvg-2.26.0-i486-3.txz \
    libsamplerate-0.1.7-i486-1.txz \
    libspectre-0.2.5-i486-1.txz \
    libtermcap-1.2.3-i486-7.txz \
    libtiff-3.9.2-i486-1.txz \
    libusb-1.0.6-i486-1.txz \
    libvisual-0.4.0-i486-2.txz \
    libvisual-plugins-0.4.0-i486-2.txz \
    libvncserver-0.9.7-i486-2.txz \
    libwmf-0.2.8.4-i486-5.txz \
    libwmf-docs-0.2.8.4-noarch-5.txz \
    libwnck-2.28.0-i486-1.txz \
    libwpd-0.8.14-i486-1.txz \
    libxklavier-5.0-i486-1.txz \
    libxml2-2.7.6-i486-1.txz \
    libxslt-1.1.26-i486-1.txz \
    libzip-0.9-i486-2.txz \
    loudmouth-1.4.3-i486-1.txz \
    lzo-2.02-i486-1.txz \
    mhash-0.9.9-i486-1.txz \
    mm-1.4.2-i486-2.txz \
    mpfr-2.4.2p03-i486-1.txz \
    ncurses-5.7-i486-1.txz \
    neon-0.29.0-i486-1.txz \
    netpbm-10.49.02-i486-1.txz \
    notify-python-0.1.1-i486-1.txz \
    openexr-1.6.1-i486-1.txz \
    pango-1.26.2-i486-1.txz \
    parted-2.2-i486-1.txz \
    pcre-8.02-i486-1.txz \
    pilot-link-0.12.5-i486-1.txz \
    polkit-1_14bdfd8-i486-1.txz \
    polkit-gnome-0.96-i486-1.txz \
    poppler-0.12.4-i486-1.txz \
    poppler-data-0.4.0-noarch-1.txz \
    popt-1.7-i486-3.txz \
    pycairo-1.8.8-i486-1.txz \
    pygobject-2.20.0-i486-1.txz \
    pyrex-0.9.8.5-i486-2.txz \
    qca-2.0.2-i486-1.txz \
    qca-cyrus-sasl-2.0.0_beta3-i486-1.txz \
    qca-gnupg-2.0.0_beta3-i486-1.txz \
    qca-ossl-2.0.0_beta3-i486-1.txz \
    raptor-1.4.21-i486-1.txz \
    rasqal-0.9.19-i486-1.txz \
    readline-5.2-i486-4.txz \
    redland-1.0.10-i486-1.txz \
    sdl-1.2.14-i486-2.txz \
    seamonkey-solibs-2.0.4-i486-1.txz \
    shared-desktop-ontologies-0.3-i486-1.txz \
    shared-mime-info-0.71-i486-1.txz \
    sip-4.10.2-i486-1.txz \
    slang-2.2.2-i486-1.txz \
    slang1-1.4.9-i486-1.txz \
    soprano-2.4.3-i486-1.txz \
    startup-notification-0.10-i486-1.txz \
    strigi-0.7.2-i486-1.txz \
    svgalib-1.9.25-i486-2.txz \
    t1lib-5.1.2-i486-1.txz \
    taglib-1.6.2-i486-1.txz \
    taglib-extras-1.0.1-i486-1.txz \
    urwid-0.9.9.1-i486-1.txz \
    v4l-utils-0.8.0-i486-1.txz \
    virtuoso-ose-6.1.1-i486-1.txz \
    vte-0.24.1-i486-1.txz \
    zlib-1.2.3-i486-2.txz
do
    installpkg -root ${DEST}/live/ ${DVD}/slackware/l/${x}
done

#cp -R ${DVD}/kernels/* ${DEST}/kernels
#cp -R ${DVD}/isolinux/* ${DEST}/isolinux

#mkzftree ${DEST}/live ${DEST}/live2
#mv ${DEST}/live /root/live
#mv ${DEST}/live2 ${DEST}/live

#mkisofs -o /root/liveslack.iso -R -J -hide-rr-moved -v -d -N -no-emul-boot \
#    -boot-load-size 32 -boot-info-table -sort ${DEST}/isolinux/iso.sort \
#    -b isolinux/isolinux.bin -c isolinux/isolinux.boot ${DEST}

tar cfz /root/liveslack.tgz ${DEST}/live
