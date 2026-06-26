# Block-producing node on the public Leios testnet (network magic 164).
#
# Mirrors the upstream ./testnet/run.sh layout from input-output-hk/ouroboros-leios:
# the node runs from the prebuilt cardano-node-leios image, with the X-ray
# observability stack (Prometheus, Loki, Grafana, Alloy) bolted on as sibling
# containers. The working directory at /data/leios-testnet keeps the same
# shape as the upstream WORKING_DIR so ./pin-config.sh refreshes still apply.
#
# Bring-up assumed already done by hand: genesis + config snapshot pinned under
# /data/leios-testnet, stake pool registered, keys (KES/VRF/op-cert) in
# /data/leios-testnet/keys/.
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

  # Default container images for the obs stack. Pinned tags so a `docker pull`
  # doesn't silently roll versions out from under us.
  alloyImage = "grafana/alloy:v1.5.1";
  prometheusImage = "prom/prometheus:v3.1.0";
  lokiImage = "grafana/loki:3.3.2";
  grafanaImage = "grafana/grafana:11.4.0";

  # Derived from modules/user.nix so the containers always match the host
  # user that owns /data/leios-testnet.
  userCfg = config.users.users.${config.user.name};
  uid = userCfg.uid;
  gid = config.users.groups.${userCfg.group}.gid;

  # All obs containers run with --network=host so they share 127.0.0.1 with
  # the node container (which also uses host networking) and can reach the
  # node's hardcoded 127.0.0.1:12798 Prometheus endpoint.
  hostNet = [ "--network=host" ];

  alloyConfig = pkgs.writeText "config.alloy" ''
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
      path_targets = [{"__path__" = "/data/node.log"}]
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
      stage.timestamp { source = "at"  format = "RFC3339" }
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

  # Minimal prometheus.yaml — Alloy remote_writes; no local scrape jobs.
  prometheusConfig = pkgs.writeText "prometheus.yaml" ''
    global:
      scrape_interval: 15s
    scrape_configs: []
  '';

  lokiConfig = pkgs.writeText "loki.yaml" ''
    auth_enabled: false
    limits_config:
      allow_structured_metadata: true
      volume_enabled: true
      max_streams_per_user: 100000
    server:
      http_listen_port: ${toString lokiPort}
      log_level: info
      grpc_server_max_send_msg_size: 16777216
      grpc_server_max_recv_msg_size: 16777216
    common:
      ring:
        instance_addr: 127.0.0.1
        kvstore: { store: inmemory }
      replication_factor: 1
      path_prefix: /loki
    schema_config:
      configs:
        - from: 2020-05-15
          store: tsdb
          object_store: filesystem
          schema: v13
          index: { prefix: index_, period: 24h }
    storage_config:
      tsdb_shipper:
        active_index_directory: /loki/index
        cache_location: /loki/index_cache
      filesystem:
        directory: /loki/chunks
    pattern_ingester:
      enabled: true
  '';

  grafanaDatasources = pkgs.writeText "datasources.yaml" ''
    apiVersion: 1
    datasources:
      - name: Prometheus
        type: prometheus
        access: proxy
        url: http://127.0.0.1:${toString prometheusPort}
        isDefault: true
      - name: Loki
        type: loki
        access: proxy
        url: http://127.0.0.1:${toString lokiPort}
  '';

  grafanaDashboardsProvider = pkgs.writeText "dashboards.yaml" ''
    apiVersion: 1
    providers:
      - name: leios
        orgId: 1
        folder: Leios
        type: file
        disableDeletion: true
        editable: false
        options:
          path: /etc/grafana/dashboards
  '';
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
  # --network=host so the in-container Prometheus endpoint (127.0.0.1:12798,
  # hardcoded in config.json) is reachable by Alloy on the host, and so
  # node-to-node on :3010 binds directly without docker NAT.
  virtualisation.oci-containers.containers.leios-bp = {
    image = nodeImage;
    user = "${toString uid}:${toString gid}";
    extraOptions = hostNet;
    volumes = [ "${workingDir}:/data" ];
    entrypoint = "cardano-node";
    cmd = [ "run" ] ++ lib.cli.toGNUCommandLine { } {
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

  # --- X-ray observability stack ------------------------------------------
  # Mirrors demo/extras/x-ray + testnet/run.sh, each process as its own
  # container instead of a process-compose entry.

  # All obs containers run as ch1bo (1000:100) so their state dirs under
  # /data/leios-testnet stay owned by the same user that manages the rest of
  # the working directory. The upstream images all tolerate a non-default
  # uid as long as the mounted paths are writable.

  virtualisation.oci-containers.containers.leios-prometheus = {
    image = prometheusImage;
    user = "${toString uid}:${toString gid}";
    extraOptions = hostNet;
    volumes = [
      "${prometheusConfig}:/etc/prometheus/prometheus.yml:ro"
      "${workingDir}/prometheus:/prometheus"
    ];
    cmd = [
      "--config.file=/etc/prometheus/prometheus.yml"
      "--storage.tsdb.path=/prometheus"
      "--web.listen-address=127.0.0.1:${toString prometheusPort}"
      "--web.enable-remote-write-receiver"
    ];
  };

  virtualisation.oci-containers.containers.leios-loki = {
    image = lokiImage;
    user = "${toString uid}:${toString gid}";
    extraOptions = hostNet;
    volumes = [
      "${lokiConfig}:/etc/loki/loki.yaml:ro"
      "${workingDir}/loki:/loki"
    ];
    cmd = [ "-config.file=/etc/loki/loki.yaml" ];
  };

  virtualisation.oci-containers.containers.leios-grafana = {
    image = grafanaImage;
    user = "${toString uid}:${toString gid}";
    extraOptions = hostNet;
    environment = {
      GF_SERVER_HTTP_ADDR = "0.0.0.0";
      GF_SERVER_HTTP_PORT = toString grafanaPort;
      GF_AUTH_ANONYMOUS_ENABLED = "true";
      GF_AUTH_ANONYMOUS_ORG_ROLE = "Admin";
      GF_SECURITY_ADMIN_USER = "admin";
      GF_SECURITY_ADMIN_PASSWORD = "admin";
      GF_ANALYTICS_REPORTING_ENABLED = "false";
      GF_ANALYTICS_CHECK_FOR_UPDATES = "false";
      GF_ANALYTICS_CHECK_FOR_PLUGIN_UPDATES = "false";
      GF_PATHS_DATA = "/var/lib/grafana";
      GF_PATHS_LOGS = "/var/lib/grafana/logs";
      GF_PATHS_PLUGINS = "/var/lib/grafana/plugins";
    };
    volumes = [
      "${grafanaDatasources}:/etc/grafana/provisioning/datasources/datasources.yaml:ro"
      "${grafanaDashboardsProvider}:/etc/grafana/provisioning/dashboards/leios.yaml:ro"
      "${./grafana-throughput.json}:/etc/grafana/dashboards/throughput.json:ro"
      "${workingDir}/grafana:/var/lib/grafana"
    ];
    dependsOn = [
      "leios-prometheus"
      "leios-loki"
    ];
  };

  virtualisation.oci-containers.containers.leios-alloy = {
    image = alloyImage;
    user = "${toString uid}:${toString gid}";
    extraOptions = hostNet;
    volumes = [
      "${alloyConfig}:/etc/alloy/config.alloy:ro"
      "${workingDir}:/data:ro"
      "${workingDir}/alloy:/var/lib/alloy"
    ];
    cmd = [
      "run"
      "--storage.path=/var/lib/alloy"
      "/etc/alloy/config.alloy"
    ];
    dependsOn = [
      "leios-prometheus"
      "leios-loki"
    ];
  };
}
