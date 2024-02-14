{config, pkgs, ...}:
let
	mysqldWrapped = (pkgs.writeShellScriptBin "mysqld_init_table"
''
mkdir -p ${config.home.homeDirectory}/.local/run
mkdir -p ${config.home.homeDirectory}/.local
mkdir -p ${config.home.homeDirectory}/.local/lib
mkdir -p ${config.home.homeDirectory}/.local/lib/mysql
if [ ! -f ${config.home.homeDirectory}/.local/run/mysqld.sock ]; then
${pkgs.mysql80}/bin/mysqld --datadir=${config.home.homeDirectory}/.local/lib/mysql --pid-file=${config.home.homeDirectory}/.local/run/mysqld.pid --socket=${config.home.homeDirectory}/.local/run/mysqld.sock
	rm -rf ${config.home.homeDirectory}/.local/lib/mysql
	${pkgs.mysql80}/bin/mysqld --datadir=${config.home.homeDirectory}/.local/lib/mysql --pid-file=${config.home.homeDirectory}/.local/run/mysqld/mysqld.pid --socket=${config.home.homeDirectory}/.local/run/mysqld/mysqld.sock --port=3306 --initialize-insecure --user=root
fi
${pkgs.mysql80}/bin/mysqld --datadir=${config.home.homeDirectory}/.local/lib/mysql --pid-file=${config.home.homeDirectory}/.local/run/mysqld.pid --socket=${config.home.homeDirectory}/.local/run/mysqld.sock --port=3306
''
);
in
{

  	systemd.user.services.mysqld = {
		Unit = {
			Description = "MySQL userspace server instance. To completely reload it remove ~/.local/run/mysqld ~/.local/lib/mysql.";
			Documentation = [ "mysqld(1)" ];
			After = [ "syslog.target" ];
		};
		Service = {
			Type="simple";
			ExecStart = "${mysqldWrapped}/bin/mysqld_init_table";
		};
		Install = {
			WantedBy = [ "default.target" ];
		};
	};

	home.file.".my.cnf".text = ''
	[client]
	port = 3306
	socket = ${config.home.homeDirectory}/.local/run/mysqld.sock
	user = root
	'';
}

