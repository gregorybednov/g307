mkdir -p /home/student/.local
mkdir -p /home/student/.local/bin
wget -O /home/student/.local/bin/nix-user-chroot https://github.com/gregorybednov/g307/raw/main/nix-user-chroot
mkdir -m 0755 ~/.nix
wget -O /home/student/selectvm.nix https://github.com/gregorybednov/g307/raw/main/selectvm.nix
wget -O /home/student/selectvm.hs https://github.com/gregorybednov/g307/raw/main/selectvm.hs
nix-user-chroot ~/.nix bash -c "curl -L https://nixos.org/nix/install | bash"

cat > /home/student/.xsessionrc <<EOF
XDG_SESSION_TYPE=wayland # hack
while ! pgrep rofi;
do
	/home/student/.local/bin/nix-user-chroot /home/student/.nix bash -l -c "rofi -normal-window -show selectvm -modes \"selectvm:selectvm\" -show-icons"
done
EOF

echo "ping 10.0.174.12" >> /home/student/.profile

nix-user-chroot ~/.nix bash -c "nix-env -i rofi"
nix-user-chroot ~/.nix bash -c "nix-env -i rsync"
nix-user-chroot ~/.nix bash -c "nix-env -if selectvm.nix"
