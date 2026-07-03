const express = require('express');
const app = express();

app.get('/ping', (req, res) => {
  res.setHeader('Content-Type', 'text/plain');
  res.send('pong');
});

console.log('Servidor Node.js/Express rodando na porta 9090...');
app.listen(9090);
