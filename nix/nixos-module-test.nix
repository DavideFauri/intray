{ sources ? import ./sources.nix
, pkgs ? import ./pkgs.nix { inherit sources; }
, intrayPackages ? pkgs.intrayPackages
}:
let
  intray-production = import (./nixos-module.nix) {
    inherit sources;
    inherit pkgs;
    inherit intrayPackages;
    envname = "production";
  };
  home-manager = import (sources.home-manager + "/nixos/default.nix");

  api-port = 8000;
  web-port = 8080;
in
pkgs.nixosTest (
  { lib, pkgs, ... }: {
    name = "intray-module-test";
    nodes = {
      apiserver = {
        imports = [
          intray-production
        ];
        services.intray.production = {
          enable = true;
          api-server = {
            enable = true;
            port = api-port;
            log-level = "Debug";
          };
        };
      };
      webserver = {
        imports = [
          intray-production
        ];
        services.intray.production = {
          enable = true;
          web-server = {
            enable = true;
            port = web-port;
            api-url = "apiserver:${builtins.toString api-port}";
          };
        };
      };
      client = {
        imports = [
          home-manager
        ];
        users.users.testuser.isNormalUser = true;
        home-manager = {
          useGlobalPkgs = true;
          users.testuser = { pkgs, ... }: {
            imports = [
              ./home-manager-module.nix
            ];
            home.stateVersion = "20.09";
            programs.intray = {
              enable = true;
              inherit intrayPackages;
              sync = {
                enable = true;
                url = "http://apiserver:${builtins.toString api-port}";
                username = "testuser";
                password = "testpassword";
              };
            };
          };
        };
      };
    };
    testScript = ''
      from shlex import quote

      apiserver.start()
      webserver.start()
      client.start()

      apiserver.wait_for_unit("multi-user.target")
      webserver.wait_for_unit("multi-user.target")
      client.wait_for_unit("multi-user.target")

      apiserver.wait_for_unit("intray-api-server-production.service")
      webserver.wait_for_unit("intray-web-server-production.service")

      apiserver.wait_for_open_port(${builtins.toString api-port})
      client.succeed("curl apiserver:${builtins.toString api-port}")

      webserver.wait_for_open_port(${builtins.toString web-port})
      client.succeed("curl webserver:${builtins.toString web-port}")

      client.wait_for_unit("home-manager-testuser.service")


      def su(user, cmd):
          return f"su - {user} -c {quote(cmd)}"


      client.succeed(su("testuser", "cat ~/.config/intray/config.yaml"))

      # client.succeed(su("testuser", "intray register"))
      # client.succeed(su("testuser", "intray login"))
      # client.succeed(su("testuser", "intray sync"))
    '';
  }
)
