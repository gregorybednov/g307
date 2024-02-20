{config, pkgs, ...}:
let


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

