radiod is a program that uses inotify to watch for radio and rotor device
nodes to appear and disappear.  When they appear, it starts up the appropriate
program to control that device (rigctld or rotctld for now).  When they
disappear, it stops those programs.  Obviously, you should have those installed
on your system first.

radiod can be used to manage several different radios and rotors hooked up to
the same computer, as long as you configure it correctly.

It does not handle the situation where it is started after radios are connected
and powered on.  It should be started from an init script at system boot time
and left running in the background, since it acts like a regular daemon.

# INSTALLATION

cabal is kind of annoying and I can't figure out how to make it install things
into /etc and /usr/libexec without significant amounts of work that I'm not
willing to do.  We'll just install it the old fashioned way.

Make sure you have the Haskell module dependencies installed first.  Then:

```
$ cabal configure --prefix=/usr --sysconfdir=/etc --libexecdir=/usr/libexec/radiod
$ cabal build
...
$ sudo install -m 755 -o root -g root dist/build/radiod/radiod /usr/bin
$ sudo install -m 644 -o root -g root data/radiod.conf /etc
$ sudo install -D -m 755 -o root -g root -t /usr/libexec/radiod data/*-wrapper
```

# CONFIGURATION

Configuration is fairly simple.  I have udev rules on my system to create more
meaningful symlinks.  I recommend you do the same.  For example, I have this in
/etc/udev/rules.d/80-radio-rules:

    # CP210x UART => /dev/ts-590sg
    ACTION=="add", ENV{ID_VENDOR_ID}=="10c4", ENV{ID_MODEL_ID}=="ea60", SYMLINK+="ts-590sg"

Then I don't need to remember which real device node (/dev/ttyUSB0?  USB1?) it
gets assigned to.  It's always the same thing.

Having done that, you then need to modify /etc/radiod.conf.  The parser for this
config file is fairly stupid and does not handle comments, blank lines, or spaces
at the beginning of a lines.  This is because writing config file parsers is
boring.  I'll get to it eventually.

You can have as many lines as you'd like.  radiod will watch for all of them to
appear, only running something for those that do.  There is one trick to watching
for multiple devices which will be mentioned later.

There are four columns to the config file:

* The device node - this should be the same as whatever you've got in udev.  You
can also use the real device node if you know it will always appear in the same
spot.

* The word "rig" or "rotor".  Capitalization does not matter here.  For now, these
are the only two types of things understood.

* The rig number as understood by rigctld, or the rotor number as understood by
rotctld.  Use `rigctld -l` and `rotctld -l` to figure out which matches your
device.

* The port number to use, or a dash if the default.  If you only have one radio
or one rotor hooked up, you can just use a dash.  Once multiple are attached and
could be present at the same time, you will need to specify this.  rigctld
suggests using even ports starting with 4532, and rotctld suggests using odd
ports starting with 4533.  Remember this number for later.

You'll also need to configure whatever loggers or digital mode programs or other
radio software you use to talk to rigctld instead of using the device directly.
How you do this is dependent upon the program, but the important thing to note
is that you will want to use radio # 2 (NET rigctl) and any port you specified
in the config file.

# RUNNING

Start it from an init script (the writing of which is left up to you, or your
distribution) at system boot time.  When a radio or rotor is turned on, radiod
will notice and start the appropriate program after a short delay.  When the
radio or rotor is turned off, radiod will notice that too and kill the
appropriate program.
