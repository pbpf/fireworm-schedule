# fireworm-schedule
#课程表
wget https://mirror.racket-lang.org/installers/6.10.1/racket-minimal-6.10.1-x86_64-linux.sh
chmod +x *.sh
./*.sh
raco pkg install --binary-lib webserver-lib db-lib
git clone https://github.com/connor2059/fireworm-schedule
cd fireworm-schedule
chmod +x setup
./setup
