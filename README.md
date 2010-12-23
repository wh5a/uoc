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

The second approach is somewhat tedious, and throws all the ChromeOS
goodness away.

Since I am new to this ChromeOS thing, and I've been away from Debian
for a while, I need your help to make this project succeed.

# What
I chose to port `dpkg`/`apt` from Ubuntu Karmic, because its glibc
2.10.1 matches the version on ChromeOS. Using any rolling distro on
top of ChromeOS is impossible, although I like Debian and Arch so much
more.

# How-to
<pre><code># Turn off rootfs verification
sudo /usr/share/vboot/bin/make_dev_ssd --remove_rootfs_verification

# Reboot, and then remount / rw
sudo mount -o remount,rw /

# Remount /var exec because dpkg wants to execute scripts extracted from .deb files
sudo mount -o remount,exec /var

# Download dpkg's data.tar.gz from Ubuntu Karmic, extract it to / to get the dpkg binary
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
program.

I'll be updating the [wiki](https://github.com/wh5a/uoc/wiki/Compatible-Packages) for compatible programs.

# Risks
Modifying your root file system will most likely disable ChromeOS'
auto update. Overwriting existing libraries may make your system fail
to start. But good thing is you're always able to restore to factory setting.

# How to contribute
Please create more fake packages and send me a pull request. You'll
want to give your fake package a higher version number than Ubuntu
Karmic to prevent `apt-get` from upgrading it. I admit my approach is an ugly
hack and if you know a better solution please let me know. You're also
welcome to report which packages can be safely installed.

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

# To-do
1. Port more packages
2. Write scripts for auto installation
