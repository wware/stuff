ORIGINAL=http://distro.ibiblio.org/pub/linux/distributions/damnsmall/archive/dsl-4.4.9-embedded.zip

all:
	make detect-addition-changes
	make dsl-tweaked.zip

dsl-4.4.9-embedded.zip:
	if [ ! -f ${HOME}/dsl-4.4.9-embedded.zip ]; then \
		wget -P ${HOME} ${ORIGINAL}; \
	fi
	cp ${HOME}/dsl-4.4.9-embedded.zip .

fetched: dsl-4.4.9-embedded.zip
	sudo rm -rf fromzip knoppix-cloop-fs
	mkdir fromzip
	(cd fromzip; unzip ../dsl-4.4.9-embedded.zip)
	extract_compressed_fs fromzip/KNOPPIX/KNOPPIX - > knoppix-cloop-fs
	touch fetched

additions.md5:
	tar cf - additions | md5sum - | cut -c -32 > additions.md5

detect-addition-changes:
	if [ -f additions.md5 ]; then mv additions.md5 additions.md5.previous; fi
	make additions.md5
	if [ -f additions.md5.previous ]; then \
		if ( ! diff additions.md5.previous additions.md5 ) then \
			rm -f modified additions.md5.previous dsl-tweaked.zip; \
		fi; \
	fi

# a quick hack dependent upon the USB stick I happen to be using
remod:
	rm -f modified
	make detect-addition-changes
	make KNOPPIX2
	cp KNOPPIX2 /media/C9D0-3E18/KNOPPIX/KNOPPIX
	sync
	umount /media/C9D0-3E18

# some things must be done on a case-by-case basis
modified: fetched
	sudo rm -rf knoppix-cloop my-knoppix-tree
	mkdir       knoppix-cloop my-knoppix-tree
	sudo mount -o loop knoppix-cloop-fs knoppix-cloop
	tar -C knoppix-cloop -cf - . | sudo tar -C my-knoppix-tree -xvpf -
	sudo umount knoppix-cloop
	sudo rm -rf my-knoppix-tree/.rr_moved
	tar -C additions -cf - . | sudo tar -C my-knoppix-tree -xvpf -
	sudo mv -f my-knoppix-tree/etc/skel/dot-xinitrc my-knoppix-tree/etc/skel/.xinitrc
	touch modified

KNOPPIX2: modified
	sudo rm -rf tozip KNOPPIX2
	mkisofs -L -R -l -V "TWEAKED VERSION" -v -allow-multidot my-knoppix-tree | \
		create_compressed_fs - 65536 > KNOPPIX2

dsl-tweaked.zip: KNOPPIX2
	sudo rm -rf tozip
	sudo cp -R fromzip tozip
	sudo cp KNOPPIX2 tozip/KNOPPIX/KNOPPIX
	(cd tozip; sudo zip -r ../dsl-tweaked.zip *)

clean:
	- sudo umount knoppix-cloop
	sudo rm -rf fromzip tozip knoppix-cloop knoppix-cloop-fs my-knoppix-tree \
		KNOPPIX2 dsl-4.4.9-embedded.zip fetched modified dsl-tweaked.zip \
		additions.md5*
