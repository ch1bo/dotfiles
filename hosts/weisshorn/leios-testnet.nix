# Block-producing node on the public Leios testnet (network magic 164).
#
# The node runs from the prebuilt cardano-node-leios container image; the
# observability stack (Prometheus, Loki, Grafana, Alloy) runs as NixOS services
# on the host. The working directory at /data/leios-testnet holds the upstream
# config snapshot (./pin-config.sh) plus chain DB / keys.
#
# Bring-up assumed already done by hand: genesis + config snapshot pinned
# under /data/leios-testnet, stake pool registered, keys (KES/VRF/op-cert)
# in /data/leios-testnet/keys/.
{
  config,
  pkgs,
  lib,
  ...
}:
let
  workingDir = "/data/leios-testnet";

  nodePort = 3010;
  metricsPort = 12798;
  grafanaPort = 3000;
  prometheusPort = 9090;
  lokiPort = 3100;

  nodeImage = "ghcr.io/input-output-hk/ouroboros-leios/cardano-node-leios:prototype-2026w26";

  # Derived from modules/user.nix so the node container matches the host
  # user that owns /data/leios-testnet.
  userCfg = config.users.users.${config.user.name};
  uid = userCfg.uid;
  gid = config.users.groups.${userCfg.group}.gid;

in
{
  networking.firewall.allowedTCPPorts = [
    nodePort
    grafanaPort
  ];

  # --- Block-producing node ------------------------------------------------
  #
  # cardano-node-leios:prototype-2026w26 just ships the statically-linked
  # cardano-node + cardano-cli binaries (no entrypoint script). We invoke
  # cardano-node directly with the KES/VRF/op-cert flags so the same image
  # serves as a producer. The config snapshot under /data/leios-testnet is
  # whatever upstream ./pin-config.sh produced — we don't re-stage it here.
  #
  # --network=host so node-to-node on :3010 binds directly without docker
  # NAT, and so the in-container Prometheus endpoint (127.0.0.1:12798,
  # hardcoded in config.json) is reachable by Alloy on the host.
  virtualisation.oci-containers.containers.leios-bp = {
    image = nodeImage;
    user = "${toString uid}:${toString gid}";
    extraOptions = [ "--network=host" ];
    volumes = [ "${workingDir}:/data" ];
    entrypoint = "cardano-node";
    cmd = [
      "run"
    ]
    ++ lib.cli.toGNUCommandLine { } {
      config = "/data/config.json";
      topology = "/data/topology.json";
      database-path = "/data/db";
      socket-path = "/data/node.socket";
      host-addr = "0.0.0.0";
      port = nodePort;
      shelley-kes-key = "/data/keys/kes.skey";
      shelley-vrf-key = "/data/keys/vrf.skey";
      shelley-operational-certificate = "/data/keys/opcert.cert";
    };
  };

  # --- Observability stack ------------------------------------------
  # Mirrors ouroboros-leios/demo/extras/x-ray + testnet/run.sh, each process as its own
  # NixOS service. State lives under the NixOS defaults
  # (/var/lib/{prometheus2,loki,private/alloy,grafana}); the upstream
  # /data/leios-testnet/{prometheus,loki,alloy,grafana} dirs from the
  # original process-compose run are no longer used and can be removed.

  services.prometheus = {
    enable = true;
    listenAddress = "127.0.0.1";
    port = prometheusPort;
    # Alloy pushes via remote_write; no local scrape jobs.
    extraFlags = [ "--web.enable-remote-write-receiver" ];
  };

  services.loki = {
    enable = true;
    configuration = {
      auth_enabled = false;
      server = {
        http_listen_port = lokiPort;
        log_level = "info";
        grpc_server_max_send_msg_size = 16777216;
        grpc_server_max_recv_msg_size = 16777216;
      };
      common = {
        ring = {
          instance_addr = "127.0.0.1";
          kvstore.store = "inmemory";
        };
        replication_factor = 1;
        path_prefix = "/var/lib/loki";
      };
      limits_config = {
        allow_structured_metadata = true;
        volume_enabled = true;
        max_streams_per_user = 100000;
      };
      schema_config.configs = [
        {
          from = "2020-05-15";
          store = "tsdb";
          object_store = "filesystem";
          schema = "v13";
          index = {
            prefix = "index_";
            period = "24h";
          };
        }
      ];
      storage_config = {
        tsdb_shipper = {
          active_index_directory = "/var/lib/loki/index";
          cache_location = "/var/lib/loki/index_cache";
        };
        filesystem.directory = "/var/lib/loki/chunks";
      };
      pattern_ingester.enabled = true;
    };
  };

  services.alloy = {
    enable = true;
    configPath = pkgs.writeText "config.alloy" ''
      prometheus.remote_write "prometheus" {
        endpoint {
          url = "http://127.0.0.1:${toString prometheusPort}/api/v1/write"
        }
      }

      prometheus.scrape "cardano_node_exporter" {
        targets = [{
          "__address__" = "127.0.0.1:${toString metricsPort}",
          "job"         = "cardano_node_metrics",
          "instance"    = "localhost",
          "alias"       = "node",
          "process"     = "Node",
        }]
        metrics_path    = "/metrics"
        scrape_timeout  = "1s"
        scrape_interval = "1s"
        forward_to = [prometheus.remote_write.prometheus.receiver]
      }

      loki.write "loki" {
        endpoint {
          url        = "http://127.0.0.1:${toString lokiPort}/loki/api/v1/push"
          batch_size = "2MB"
        }
      }

      local.file_match "local_files" {
        path_targets = [{"__path__" = "${workingDir}/node.log"}]
        sync_period  = "5s"
      }

      loki.source.file "cardano_node_log" {
        targets       = local.file_match.local_files.targets
        forward_to    = [loki.process.extract_cardano_node_logs.receiver]
        tail_from_end = false
      }

      loki.process "extract_cardano_node_logs" {
        stage.json {
          expressions = {
            at     = "at",
            sev    = "sev",
            host   = "host",
            thread = "thread",
            ns     = "ns",
            data   = "data",
          }
          drop_malformed = true
        }
        stage.timestamp {
          source = "at"
          format = "RFC3339"
        }
        stage.labels {
          values = { level = "sev", host = "host", sev = "sev", ns = "ns" }
        }
        stage.static_labels {
          values = { service = "cardano-node", type = "cardano-node" }
        }
        stage.output { source = "data" }
        forward_to = [loki.write.loki.receiver]
      }
    '';
  };
  # Alloy tails ${workingDir}/node.log; without this the service can't
  # read it through systemd's ProtectHome= sandbox.
  systemd.services.alloy.serviceConfig.ReadOnlyPaths = [ workingDir ];

  services.grafana = {
    enable = true;
    settings = {
      server = {
        http_addr = "0.0.0.0";
        http_port = grafanaPort;
        domain = "localhost";
      };
      "auth.anonymous" = {
        enabled = true;
        org_role = "Admin";
      };
      security = {
        admin_user = "admin";
        admin_password = "admin";
      };
      analytics = {
        reporting_enabled = false;
        check_for_updates = false;
        check_for_plugin_updates = false;
        feedback_links_enabled = false;
      };
    };
    provision = {
      enable = true;
      datasources.settings.datasources = [
        {
          name = "Prometheus";
          type = "prometheus";
          access = "proxy";
          url = "http://127.0.0.1:${toString prometheusPort}";
          isDefault = true;
        }
        {
          name = "Loki";
          type = "loki";
          access = "proxy";
          url = "http://127.0.0.1:${toString lokiPort}";
        }
      ];
      dashboards.settings.providers = [
        {
          name = "leios";
          orgId = 1;
          folder = "Leios";
          type = "file";
          disableDeletion = true;
          editable = false;
          options.path = ./grafana-dashboards;
        }
      ];
    };
  };

}
