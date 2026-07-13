const grpc = require('@grpc/grpc-js');
const protoLoader = require('@grpc/proto-loader');
const path = require('path');

const PROTO_PATH = path.join(__dirname, '..', 'users.proto');

const packageDefinition = protoLoader.loadSync(PROTO_PATH, {
  keepCase: true,
  longs: String,
  enums: String,
  defaults: true,
  oneofs: true
});

const usersProto = grpc.loadPackageDefinition(packageDefinition).users;

async function runBenchmark() {
  const client = new usersProto.UserService('localhost:9090', grpc.credentials.createInsecure(), {
    'grpc.max_receive_message_length': -1,
    'grpc.max_send_message_length': -1
  });

  const TOTAL_REQUESTS = 50000;
  const CONCURRENCY = 200;

  console.log(`Starting gRPC Benchmark with ${TOTAL_REQUESTS} total requests (${CONCURRENCY} concurrent)...`);

  const startTime = Date.now();
  let completed = 0;
  let failed = 0;
  const latencies = [];

  const runRequest = async () => {
    return new Promise((resolve) => {
      const reqStart = Date.now();
      client.GetUser({ id: 42 }, (err, response) => {
        const duration = Date.now() - reqStart;
        latencies.push(duration);
        if (err) {
          failed++;
        }
        completed++;
        resolve();
      });
    });
  };

  const workers = [];
  const queue = Array.from({ length: TOTAL_REQUESTS });

  const worker = async () => {
    while (queue.length > 0) {
      queue.pop();
      await runRequest();
    }
  };

  for (let i = 0; i < CONCURRENCY; i++) {
    workers.push(worker());
  }

  await Promise.all(workers);

  const endTime = Date.now();
  const totalDurationSec = (endTime - startTime) / 1000;
  const qps = completed / totalDurationSec;

  latencies.sort((a, b) => a - b);
  const avgLatency = latencies.reduce((sum, val) => sum + val, 0) / latencies.length;
  const p99Index = Math.min(Math.floor(latencies.length * 0.99), latencies.length - 1);
  const p99Latency = latencies[p99Index];

  console.log('\n--- Benchmark Results ---');
  console.log(`Total Requests: ${completed}`);
  console.log(`Failed Requests: ${failed}`);
  console.log(`Duration: ${totalDurationSec.toFixed(2)}s`);
  console.log(`QPS (Throughput): ${qps.toFixed(2)} req/sec`);
  console.log(`Average Latency: ${avgLatency.toFixed(2)}ms`);
  console.log(`p99 Latency: ${p99Latency}ms`);
  console.log('-------------------------\n');
}

runBenchmark();
