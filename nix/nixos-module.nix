{ sources ? import ./sources.nix
, pkgs ? import ./pkgs.nix { inherit sources; }
, intrayPackages ? pkgs.intrayPackages
, envname
}:
{ lib, pkgs, config, ... }:
with lib;

let
  cfg = config.services.intray."${envname}";

  mergeListRecursively = pkgs.callPackage ./merge-lists-recursively.nix { };

  toYamlFile = pkgs.callPackage ./to-yaml.nix { };
in
{
  options.services.intray."${envname}" =
    {
      enable = mkEnableOption "Intray Service";
      api-server = mkOption {
        default = null;
        type = types.nullOr (types.submodule {
          options = {
            enable = mkEnableOption "Intray API Server";
            config = mkOption {
              default = { };
              description = "The contents of the config file, as an attribute set. This will be translated to Yaml and put in the right place along with the rest of the options defined in this submodule.";
            };
            hosts = mkOption {
              type = types.listOf types.str;
              example = [ "api.intray.cs-syd.eu" ];
              default = [ ];
              description = "The host to serve API requests on";
            };
            port = mkOption {
              type = types.int;
              example = 8001;
              description = "The port to serve API requests on";
            };
            log-level = mkOption {
              type = types.nullOr types.str;
              default = null;
              example = "LevelInfo";
              description = "The log level";
            };
            admins = mkOption {
              type = types.nullOr (types.listOf types.str);
              default = null;
              example = [ "syd" ];
              description = "A list of the usernames that will have admin privileges";
            };
            freeloaders = mkOption {
              type = types.nullOr (types.listOf types.str);
              default = null;
              example = [ "syd" ];
              description = "A list of the usernames that will will be able to use the service for free";
            };
            monetisation = mkOption {
              default = null;
              type =
                types.nullOr (
                  types.submodule {
                    options = {
                      stripe-plan = mkOption {
                        type = types.str;
                        example = "plan_XXXXXXXXXXXXXX";
                        description = "Stripe plan for subscriptions.";
                      };
                      stripe-secret-key = mkOption {
                        type = types.str;
                        example = "sk_test_XXXXXXXXXXXXXXXXXXXXXXXX";
                        description = "Stripe secret key.";
                      };
                      stripe-publishable-key = mkOption {
                        type = types.str;
                        example = "pk_test_XXXXXXXXXXXXXXXXXXXXXXXX";
                        description = "Stripe publishable key.";
                      };
                    };
                  }
                );
            };
          };
        });
      };
      web-server = mkOption {
        default = null;
        type = types.nullOr (types.submodule {
          options = {
            enable = mkEnableOption "Intray Web Server";
            config = mkOption {
              default = { };
              description = "The contents of the config file, as an attribute set. This will be translated to Yaml and put in the right place along with the rest of the options defined in this submodule.";
            };
            hosts = mkOption {
              type = types.listOf types.str;
              example = [ "intray.cs-syd.eu" ];
              default = [ ];
              description = "The host to serve web requests on";
            };
            port = mkOption {
              type = types.int;
              default = 8000;
              example = 8000;
              description = "The port to serve web requests on";
            };
            log-level = mkOption {
              type = types.nullOr types.str;
              default = null;
              example = "LevelInfo";
              description = "The log level";
            };
            api-url = mkOption {
              type = types.str;
              example = "http://api.intray.eu";
              description = "The url to the api server to call";
            };
            tracking-id = mkOption {
              type = types.nullOr types.str;
              default = null;
              example = "UA-53296133-1";
              description = "The tracking id for google analytics";
            };
            verification-tag = mkOption {
              type = types.nullOr types.str;
              default = null;
              example = "ADkAx2F-JQO9KJBBdLfAGuJ_OMqPOsX5MdGDsfd0Ggw";
              description = "The verification tag for google search console";
            };
          };
        });
      };
    };
  config =
    let
      workingDir = "/www/intray/${envname}/data/";
      attrOrNull = name: value: optionalAttrs (!builtins.isNull value) { "${name}" = value; };
      attrOrNullHead = name: value: optionalAttrs (!builtins.isNull value && value != [ ]) { "${name}" = builtins.head value; };
      api-server-config = with cfg.api-server; mergeListRecursively [
        (attrOrNullHead "host" hosts)
        (attrOrNull "port" port)
        (attrOrNull "admins" admins)
        (attrOrNull "freeloaders" freeloaders)
        (attrOrNull "log-level" log-level)
        (attrOrNull "monetisation" monetisation)
        cfg.api-server.config
      ];
      api-server-config-file = toYamlFile "intray-api-server-config" api-server-config;
      api-server-service = optionalAttrs (cfg.api-server.enable or false) {
        "intray-api-server-${envname}" = {
          description = "Intray ${envname} api server service";
          wantedBy = [ "multi-user.target" ];
          environment = {
            "INTRAY_SERVER_CONFIG_FILE" = "${api-server-config-file}";
          };
          script =
            ''
              mkdir -p "${workingDir}"
              cd "${workingDir}"
              ${intrayPackages.intray-server}/bin/intray-server serve
            '';
          serviceConfig =
            {
              Restart = "always";
              RestartSec = 1;
              Nice = 15;
            };
          unitConfig =
            {
              # ensure Restart=always is always honoured
              StartLimitIntervalSec = 0;
            };
        };
      };
      api-host = optionalAttrs (cfg.api-server.enable or false && cfg.api-server.hosts != [ ]) {
        "${builtins.head (cfg.api-server.hosts)}" =
          {
            enableACME = true;
            forceSSL = true;
            locations."/".proxyPass = "http://localhost:${builtins.toString (cfg.api-server.port)}";
            serverAliases = tail cfg.api-server.hosts;
          };
      };
      web-server-config = with cfg.web-server; mergeListRecursively [
        (attrOrNullHead "host" hosts)
        (attrOrNull "port" port)
        (attrOrNull "log-level" log-level)
        (attrOrNull "api-url" api-url)
        (attrOrNull "tracking" tracking-id)
        (attrOrNull "verification" verification-tag)
        cfg.web-server.config
      ];
      web-server-config-file = toYamlFile "intray-web-server-config" web-server-config;
      web-server-service = optionalAttrs (cfg.web-server.enable or false) {
        "intray-web-server-${envname}" = {
          description = "Intray ${envname} web server service";
          wantedBy = [ "multi-user.target" ];
          environment = {
            "INTRAY_WEB_SERVER_CONFIG_FILE" = "${web-server-config-file}";
          };
          script =
            ''
              mkdir -p "${workingDir}"
              cd "${workingDir}"
              ${intrayPackages.intray-web-server}/bin/intray-web-server serve
            '';
          serviceConfig =
            {
              Restart = "always";
              RestartSec = 1;
              Nice = 15;
            };
          unitConfig =
            {
              # ensure Restart=always is always honoured
              StartLimitIntervalSec = 0;
            };
        };
      };
      web-host = optionalAttrs (cfg.web-server.enable or false && cfg.web-server.hosts != [ ]) {
        "${builtins.head (cfg.web-server.hosts)}" =
          {
            enableACME = true;
            forceSSL = true;
            locations."/".proxyPass = "http://localhost:${builtins.toString (cfg.web-server.port)}";
            serverAliases = tail cfg.web-server.hosts;
          };
      };
    in
    mkIf cfg.enable {
      systemd.services = mergeListRecursively [
        api-server-service
        web-server-service
      ];
      networking.firewall.allowedTCPPorts = builtins.concatLists [
        (optional (cfg.api-server.enable or false) cfg.api-server.port)
        (optional (cfg.web-server.enable or false) cfg.web-server.port)
      ];
      services.nginx.virtualHosts = mergeListRecursively [
        api-host
        web-host
      ];
    };
}
