# Why
This project aims to add a package manager to ChromeOS running on
Cr-48. At present there are only two ways to install native Linux
applications:  
1. Build a ChromiumOS image and customize its packages,  
2. Install a real Linux distro.

The first approach is powerful but inflexible. After you've built the
image and boot the computer, you can no longer add new programs due to
ChromeOS' security policy. ChromeOS mounts writable partions as
non-executable, and executable partitions as non-writable. Did I
mention setting up the build environment is intimidating too?

The second approach is somewhat tedious (but now can be [automated](http://chromeos-cr48.blogspot.com/2010/12/easy-way-to-install-ubuntu-on-your-cr.html)), and throws all the ChromeOS
goodness away.

Since I am new to this ChromeOS thing, and I've been away from Debian
for a while, I need your help to make this project succeed.

# What
I chose to port `dpkg`/`apt` from Ubuntu Karmic, because its glibc
2.10.1 matches the version on ChromeOS. Using any rolling distro on
top of ChromeOS is impossible, although I like Debian and Arch so much
more.

Compilers (gcc and ghc), editors (emacs), x11-apps, and git have been
successfully ported. This project is now completely self hosted
without relying on an Ubuntu system for development. I personally
consider it to be more than enough for casual programming.

# How-to
<pre><code># Turn off rootfs verification
sudo /usr/share/vboot/bin/make_dev_ssd.sh --remove_rootfs_verification
sudo reboot

sudo mount -o remount,rw /

# Remount /var exec because dpkg wants to execute scripts extracted from .deb files
# To avoid running this command after each reboot, copy the modified /sbin/chromeos_startup
sudo mount -o remount,exec /var

# Download dpkg's data.tar.gz from Ubuntu Karmic or this git project, extract it to / to get the dpkg binary
cd /; sudo tar zxf /path/to/data.tar.gz

# Create files needed by dpkg
sudo touch /var/lib/dpkg/status /var/lib/dpkg/available

# Copy ldconfig and update-rc.d, they are needed by dpkg

# Satisfy dpkg's dependencies by installing fake debs

# Install dpkg.deb for real

# Install apt.deb and copy /etc/apt
</code></pre>

To download a .deb file, run `sudo apt-get -d install --reinstall foo`
and you'll find `/var/cache/apt/archives/foo*.deb`.

Before you `apt-get install` a program, double check if it's pulling
in any dependency that will overwrite existing files. Doing so
may break the login manager, the browser, or any other critical
program. I've created a simple tool `TestPkg` that looks at the
dependencies and try to guess if they already exist on the system. If
they do, a fake deb will be created. If it's unsure, i.e. some files
exist but others don't, it lists the files and let the user
decide. There are still some bugs in spawning child processes, so you
may have to run the tool first to download the dependencies and rerun
it again.

For example, before installing `git-core`, run `./tools/TestPkg git-core`
to test its dependencies and create fake debs. `dpkg -i` the fake debs
and then `apt-get install git-core`.

I'll be updating the [wiki](https://github.com/wh5a/uoc/wiki/Compatible-Packages) for compatible programs.

# Risks
Modifying your root file system will most likely disable ChromeOS'
auto update. Overwriting existing libraries may make your system fail
to start. But good thing is you're always able to restore to factory setting.

# Increasing free space
The root partition's size is only about 800 MB which is easily filled up
by new packages. One solution is to 
[resize](http://www.chromium.org/chromium-os/developer-information-for-chrome-os-devices/cr-48-chrome-notebook-developer-information/how-to-boot-ubuntu-on-a-cr-48#TOC-Free-up-some-SSD-space-for-Ubuntu)
the partitions. I chose to make use of the other inactive backup root
partition. Since ChromeOS hasn't been updated yet, the active root
partition is on `/dev/sda3`, and the inactive one on `/dev/sda5`.

I mounted `/dev/sda5`, erased its content, then moved the files under
`/usr/share` over to free up space on the active root partition. I
then need to tell the system to mount `/dev/sda5` under `/usr/share`
after booting up. If you're following through, you can uncomment the
`mount` line in the modified `/sbin/chromeos_startup`.

# How to contribute
I've created a simple tool `FakePkg` that automates the process of
generating fake packages. Simply add the name of the package to
`fake-debs/build-debs.sh`. If you've tested a fake package can be
safely added, please send me a pull request and explain what real
package was safely installed that requires the fake package you added.

You're also welcome to improve my simple tools, or report which packages can be safely installed.

I admit my approach is an ugly hack and if you know a better solution
please let me know.

# Home directory
The concept of home directory is a little confusing under
ChromeOS. Some programs use `/home/chronos/` and others use
`/home/chronos/user/`.

SSH client stores `known_hosts` under `/home/chronos/user/.ssh`,
but reads config and keys from `/home/chronos/.ssh`.

Git reads `.gitconfig` from `/home/chronos/user/`.

# More techincal stuff for hackers
ChromiumOS manages packages based on Gentoo's portage system. Its
overlay can be viewed on the [web](http://git.chromium.org/gitweb/)
and cloned from
`http://git.chromium.org/git/chromiumos-overlay`. `chromeos/scripts/setup_board`
sets the version of the system packages such as glibc.

To get an overview of the packages, we can run `ls *` at the top
level. The structure resembles Gentoo a lot, except that many packages
are removed. In fact I believe the ebuilds are taken from Gentoo
directly. ChromeOS-specific ebuilds are found under `chromeos-base/`.

When you emerge the virtual package
[chromeos](http://www.chromium.org/chromium-os/how-tos-and-troubleshooting/portage-build-faq#TOC-What-does-build_packages-actually-d)
(there are some variants like chromeos-dev and
chromeos-factoryinstall), image building starts and dependencies are
pulled in. Unfortunately, while the package manager (`sys-apps/portage`)
and the toolchain (`sys-devel/`) are in the portage, they don't get built into
the image.

The stock system on cr-48 seems to be built from ChromiumOS'
`0.9.128.B` branch (B for beta?), with some changes (some dev packages
removed, and some proprietary drivers added).

Another important piece of code is the verified bootloader and the disk
management utilities, available via `git` at `http://git.chromium.org/git/vboot_reference`.

For more information, see
[http://www.chromium.org/chromium-os/developer-guide](http://www.chromium.org/chromium-os/developer-guide)  
[http://www.chromium.org/tips-and-tricks-for-chromium-os-developers](http://www.chromium.org/tips-and-tricks-for-chromium-os-developers)  
[http://www.chromium.org/chromium-os/how-tos-and-troubleshooting](http://www.chromium.org/chromium-os/how-tos-and-troubleshooting)  
[http://www.chromium.org/chromium-os/building-chromium-os/directory-structure](http://www.chromium.org/chromium-os/building-chromium-os/directory-structure)

ChromeOS uses Ubuntu's [upstart](http://upstart.ubuntu.com/) to handle starting
of tasks and services during boot, stopping them during shutdown and
supervising them while the system is running. Unfortunately ChromeOS
seems to have reduced upstart's functionality. There's no `/etc/init.d` directory for managing
daemons. There are many interesting
scripts under `/etc/init`. For example, we can get a shell by pressing
Ctrl-Alt-F2. It's actually controlled by `/etc/init/tty2.conf`. We can
enable Ctrl-Alt-F3 by copying it to `tty3.conf` and replacing `tty2`
with `tty3` in the file. Another more interesting file is
`/etc/init/startup.conf`, which passes control to
`/sbin/chromeos_startup`. This script is run on startup and we've
already seen changing this file can be very useful.

# To-do
1. Port more packages
2. Write scripts for auto installation
3. Improve the tools, especially `TestPkg`.
