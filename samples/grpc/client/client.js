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

function main() {
  const client = new usersProto.UserService('localhost:9090', grpc.credentials.createInsecure());

  console.log('Sending GetUser request for ID 42...');
  client.GetUser({ id: 42 }, (err, response) => {
    if (err) {
      console.error('gRPC Error:', err);
      process.exit(1);
    }
    console.log('gRPC Response received successfully!');
    console.log('User ID:', response.id);
    console.log('User Name:', response.name);
    console.log('User Email:', response.email);
  });
}

main();
