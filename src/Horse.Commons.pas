unit Horse.Commons;

interface

type
  /// <summary>
  ///   HTTP Status Codes
  ///   httpstatuses.com is an easy to reference database of HTTP Status Codes
  ///   with their definitions and helpful code references all in one place.
  ///   Visit an individual status code via httpstatuses.com/code or browse the
  ///   list below.
  /// </summary>
  HTTPStatus = record
  const
    /// <summary>
    ///   The initial part of a request has been received and has not yet been
    ///   rejected by the server. The server intends to send a final response
    ///   after the request has been fully received and acted upon.
    /// </summary>
    Continue = 100;
    /// <summary>
    ///   The server understands and is willing to comply with the client's
    ///   request, via the Upgrade header field, for a change in the
    ///   application protocol being used on this connection.
    /// </summary>
    SwitchingProtocols = 101;
    /// <summary>
    ///   An interim response used to inform the client that the server has
    ///   accepted the complete request, but has not yet completed it.
    /// </summary>
    Processing = 102;
    /// <summary>
    ///   The request has succeeded.
    /// </summary>
    OK = 200;
    /// <summary>
    ///   The request has been fulfilled and has resulted in one or more new
    ///   resources being created.
    /// </summary>
    Created = 201;
    /// <summary>
    ///   The request has been accepted for processing, but the processing has
    ///   not been completed. The request might or might not eventually be acted
    ///   upon, as it might be disallowed when processing actually takes place.
    /// </summary>
    Accepted = 202;
    /// <summary>
    ///   The request was successful but the enclosed payload has been modified
    ///   from that of the origin server's 200 OK response by a transforming proxy.
    /// </summary>
    NonAuthoritativeInformation = 203;
    /// <summary>
    ///   The server has successfully fulfilled the request and that there is no
    ///   additional content to send in the response payload body.
    /// </summary>
    NoContent = 204;
    /// <summary>
    ///   The server has fulfilled the request and desires that the user agent
    ///   reset the "document view", which caused the request to be sent, to its
    ///   original state as received from the origin server.
    /// </summary>
    ResetContent = 205;
    /// <summary>
    ///   The server is successfully fulfilling a range request for the target
    ///   resource by transferring one or more parts of the selected representation
    ///   that correspond to the satisfiable ranges found in the request's Range
    ///   header field.
    /// </summary>
    PartialContent = 206;
    /// <summary>
    ///   A Multi-Status response conveys information about multiple resources
    ///   in situations where multiple status codes might be appropriate.
    /// </summary>
    MultiStatus = 207;
    /// <summary>
    ///   Used inside a DAV: propstat response element to avoid enumerating the
    ///   internal members of multiple bindings to the same collection repeatedly.
    /// </summary>
    AlreadyReported = 208;
    /// <summary>
    ///   The server has fulfilled a GET request for the resource, and the
    ///   response is a representation of the result of one or more
    ///   instance-manipulations applied to the current instance.
    /// </summary>
    IMUsed = 226;
    /// <summary>
    ///   The target resource has more than one representation, each with its
    ///   own more specific identifier, and information about the alternatives
    ///   is being provided so that the user (or user agent) can select a
    ///   preferred representation by redirecting its request to one or more
    ///   of those identifiers.
    /// </summary>
    MultipleChoices = 300;
    /// <summary>
    ///   The target resource has been assigned a new permanent URI and any
    ///   future references to this resource ought to use one of the enclosed URIs.
    /// </summary>
    MovedPermanently = 301;
    /// <summary>
    ///   The target resource resides temporarily under a different URI.
    ///   Since the redirection might be altered on occasion, the client ought
    ///   to continue to use the effective request URI for future requests.
    /// </summary>
    Found = 302;
    /// <summary>
    ///   The server is redirecting the user agent to a different resource, as
    ///   indicated by a URI in the Location header field, which is intended to
    ///   provide an indirect response to the original request.
    /// </summary>
    SeeOther = 303;
    /// <summary>
    ///   A conditional GET or HEAD request has been received and would have
    ///   resulted in a 200 OK response if it were not for the fact that the
    ///   condition evaluated to false.
    /// </summary>
    NotModified = 304;
    /// <summary>
    ///   Defined in a previous version of this specification and is now
    ///   deprecated, due to security concerns regarding in-band configuration
    ///   of a proxy.
    /// </summary>
    UseProxy = 305;
    /// <summary>
    ///   The target resource resides temporarily under a different URI and the
    ///   user agent MUST NOT change the request method if it performs an
    ///   automatic redirection to that URI.
    /// </summary>
    TemporaryRedirect = 307;
    /// <summary>
    ///   The target resource has been assigned a new permanent URI and any
    ///   future references to this resource ought to use one of the enclosed URIs.
    /// </summary>
    PermanentRedirect = 308;
    /// <summary>
    ///   The server cannot or will not process the request due to something that
    ///   is perceived to be a client error (e.g., malformed request syntax,
    ///   invalid request message framing, or deceptive request routing).
    /// </summary>
    BadRequest = 400;
    /// <summary>
    ///   The request has not been applied because it lacks valid authentication
    ///   credentials for the target resource.
    /// </summary>
    Unauthorized = 401;
    /// <summary>
    ///   Reserved for future use.
    /// </summary>
    PaymentRequired = 402;
    /// <summary>
    ///   The server understood the request but refuses to authorize it.
    /// </summary>
    Forbidden = 403;
    /// <summary>
    ///   The origin server did not find a current representation for the target
    ///   resource or is not willing to disclose that one exists.
    /// </summary>
    NotFound = 404;
    /// <summary>
    ///   The method received in the request-line is known by the origin server
    ///   but not supported by the target resource.
    /// </summary>
    MethodNotAllowed = 405;
    /// <summary>
    ///   The target resource does not have a current representation that would
    ///   be acceptable to the user agent, according to the proactive negotiation
    ///   header fields received in the request, and the server is unwilling to
    ///   supply a default representation.
    /// </summary>
    NotAcceptable = 406;
    /// <summary>
    ///   Similar to 401 Unauthorized, but it indicates that the client needs to
    ///   authenticate itself in order to use a proxy.
    /// </summary>
    ProxyAuthenticationRequired = 407;
    /// <summary>
    ///   The server did not receive a complete request message within the time
    ///   that it was prepared to wait.
    /// </summary>
    RequestTimeout = 408;
    /// <summary>
    ///   The request could not be completed due to a conflict with the current
    ///   state of the target resource. This code is used in situations where
    ///   the user might be able to resolve the conflict and resubmit the request.
    /// </summary>
    Conflict = 409;
    /// <summary>
    ///   The target resource is no longer available at the origin server and
    ///   that this condition is likely to be permanent.
    /// </summary>
    Gone = 410;
    /// <summary>
    ///   The server refuses to accept the request without a defined Content-Length.
    /// </summary>
    LengthRequired = 411;
    /// <summary>
    ///   One or more conditions given in the request header fields evaluated to
    ///   false when tested on the server.
    /// </summary>
    PreconditionFailed = 412;
    /// <summary>
    ///   The server is refusing to process a request because the request payload
    ///   is larger than the server is willing or able to process.
    /// </summary>
    PayloadTooLarge = 413;
    /// <summary>
    ///   The server is refusing to service the request because the request-target
    ///   is longer than the server is willing to interpret.
    /// </summary>
    RequestURITooLong = 414;
    /// <summary>
    ///   The origin server is refusing to service the request because the payload
    ///   is in a format not supported by this method on the target resource.
    /// </summary>
    UnsupportedMediaType = 415;
    /// <summary>
    ///   None of the ranges in the request's Range header field overlap the
    ///   current extent of the selected resource or that the set of ranges
    ///   requested has been rejected due to invalid ranges or an excessive
    ///   request of small or overlapping ranges.
    /// </summary>
    RequestedRangeNotSatisfiable = 416;
    /// <summary>
    ///   The expectation given in the request's Expect header field could not
    ///   be met by at least one of the inbound servers.
    /// </summary>
    ExpectationFailed = 417;
    /// <summary>
    ///   Any attempt to brew coffee with a teapot should result in the error
    ///   code "418 I'm a teapot". The resulting entity body MAY be short and stout.
    /// </summary>
    Imateapot = 418;
    /// <summary>
    ///   The request was directed at a server that is not able to produce a
    ///   response. This can be sent by a server that is not configured to produce
    ///   responses for the combination of scheme and authority that are included
    ///   in the request URI.
    /// </summary>
    MisdirectedRequest = 421;
    /// <summary>
    ///   The server understands the content type of the request entity
    ///   (hence a 415 Unsupported Media Type status code is inappropriate),
    ///   and the syntax of the request entity is correct (thus a 400 Bad Request
    ///   status code is inappropriate) but was unable to process the contained
    ///   instructions.
    /// </summary>
    UnprocessableEntity = 422;
    /// <summary>
    ///   The source or destination resource of a method is locked.
    /// </summary>
    Locked = 423;
    /// <summary>
    ///   The method could not be performed on the resource because the requested
    ///   action depended on another action and that action failed.
    /// </summary>
    FailedDependency = 424;
    /// <summary>
    ///   The server refuses to perform the request using the current protocol
    ///   but might be willing to do so after the client upgrades to a different
    ///   protocol.
    /// </summary>
    UpgradeRequired = 426;
    /// <summary>
    ///   The origin server requires the request to be conditional.
    /// </summary>
    PreconditionRequired = 428;
    /// <summary>
    ///   The user has sent too many requests in a given amount of time
    ///   ("rate limiting").
    /// </summary>
    TooManyRequests = 429;
    /// <summary>
    ///   The server is unwilling to process the request because its header
    ///   fields are too large. The request MAY be resubmitted after reducing
    ///   the size of the request header fields.
    /// </summary>
    RequestHeaderFieldsTooLarge = 431;
    /// <summary>
    ///   A non-standard status code used to instruct nginx to close the
    ///   connection without sending a response to the client, most commonly
    ///   used to deny malicious or malformed requests.
    /// </summary>
    ConnectionClosedWithoutResponse = 444;
    /// <summary>
    ///   The server is denying access to the resource as a consequence of a
    ///   legal demand.
    /// </summary>
    UnavailableForLegalReasons = 451;
    /// <summary>
    ///   A non-standard status code introduced by nginx for the case when a
    ///   client closes the connection while nginx is processing the request.
    /// </summary>
    ClientClosedRequest = 499;
    /// <summary>
    ///   The server encountered an unexpected condition that prevented it from
    ///   fulfilling the request.
    /// </summary>
    InternalServerError = 500;
    /// <summary>
    ///   The server does not support the functionality required to fulfill
    ///   the request.
    /// </summary>
    NotImplemented = 501;
    /// <summary>
    ///   The server, while acting as a gateway or proxy, received an invalid
    ///   response from an inbound server it accessed while attempting to
    ///   fulfill the request.
    /// </summary>
    BadGateway = 502;
    /// <summary>
    ///   The server is currently unable to handle the request due to a
    ///   temporary overload or scheduled maintenance, which will likely be
    ///   alleviated after some delay.
    /// </summary>
    ServiceUnavailable = 503;
    /// <summary>
    ///   The server, while acting as a gateway or proxy, did not receive a
    ///   timely response from an upstream server it needed to access in order
    ///   to complete the request.
    /// </summary>
    GatewayTimeout = 504;
    /// <summary>
    ///   The server does not support, or refuses to support, the major version
    ///   of HTTP that was used in the request message.
    /// </summary>
    HTTPVersionNotSupported = 505;
    /// <summary>
    ///   The server has an internal configuration error: the chosen variant
    ///   resource is configured to engage in transparent content negotiation
    ////  itself, and is therefore not a proper end point in the negotiation process.
    /// </summary>
    VariantAlsoNegotiates = 506;
    /// <summary>
    ///   The method could not be performed on the resource because the server
    ///   is unable to store the representation needed to successfully complete
    ///   the request.
    /// </summary>
    InsufficientStorage = 507;
    /// <summary>
    ///   The server terminated an operation because it encountered an infinite
    ///   loop while processing a request with "Depth: infinity". This status
    ///   indicates that the entire operation failed.
    /// </summary>
    LoopDetected = 508;
    /// <summary>
    ///   The policy for accessing the resource has not been met in the request.
    ///   The server should send back all the information necessary for the client
    ///   to issue an extended request.
    /// </summary>
    NotExtended = 510;
    /// <summary>
    ///   The client needs to authenticate to gain network access.
    /// </summary>
    NetworkAuthenticationRequired = 511;
    /// <summary>
    ///   This status code is not specified in any RFCs, but is used by some
    ///   HTTP proxies to signal a network connect timeout behind the proxy to
    ///   a client in front of the proxy.
    /// </summary>
    NetworkConnectTimeoutError = 599;
  end;

implementation

end.
