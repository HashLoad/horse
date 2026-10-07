"""Real HTTP assertions shared by Apache, CGI, FastCGI and ISAPI hosts."""
import sys
import urllib.error
import urllib.request


def request(base, path, expected, status=200, data=None, headers=None, method=None):
    req = urllib.request.Request(base + path, data=data, headers=headers or {}, method=method)
    try:
        response = urllib.request.urlopen(req, timeout=10)
    except urllib.error.HTTPError as error:
        response = error
    with response:
        body = response.read().decode("utf-8")
        assert response.status == status, (path, response.status, body)
        if expected is not None:
            assert body == expected, (path, body, expected)
        if path == "/ping":
            assert response.headers.get("X-Horse-Hosted") == "ok", response.headers


def main():
    base = sys.argv[1].rstrip("/")
    request(base, "/ping", "pong")
    request(base, "/query?v=a%2B%2541", "a+%41")
    request(base, "/query?v=Jo+da+Silva", "Jo da Silva")
    payload = '{"text":"ação 中文 100%"}'.encode("utf-8")
    request(base, "/body", payload.decode("utf-8"), data=payload,
            headers={"Content-Type": "application/json; charset=utf-8"}, method="POST")
    request(base, "/form", "a+%41", data=b"v=a%2B%2541",
            headers={"Content-Type": "application/x-www-form-urlencoded"}, method="PUT")
    request(base, "/missing", None, status=404)
    print("PASS: ping/header, query decoding, UTF-8 JSON body, form decoding, 404:", base)


if __name__ == "__main__":
    main()
