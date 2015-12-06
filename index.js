var parinfer = require("parinfer");
var http = require("http");

var handler = function(req, res) {

    var body = "";
    req.on('data', function(data) {
        body += data;
    });

    req.on('end', function() {
        if (req.url == "/indent-mode") {
            res.end(parinfer.indentMode(body).text + "\n");
        }
        else if (req.url == "/paren-mode") {
            res.end(parinfer.indentMode(body).text + "\n");
        }
        else if (req.url == "/indent-mode-changed") {
            res.end(parinfer.indentMode(body).text + "\n");
        }
    });
};

module.exports = function() {
    var server = http.createServer(handler);
    server.listen(8088, function() {
        console.log("Server Listening");
    });
};
