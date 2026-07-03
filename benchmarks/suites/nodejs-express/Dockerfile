FROM node:20-alpine

WORKDIR /app

COPY benchmarks/suites/nodejs-express/package.json ./
RUN npm install --production

COPY benchmarks/suites/nodejs-express/server.js ./

EXPOSE 9090

ENTRYPOINT ["node", "server.js"]
