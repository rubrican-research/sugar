unit sugar.http;

{$mode ObjFPC}{$H+}
{$modeswitch typehelpers}

interface
uses
    Classes, SysUtils;

type

  THTTPResponses = (
      httpUndefined=0,
      httpContinue=100,
      httpSwitchProtocols=101,
      httpProcessing=102,
      httpEarlyHints=103,

      httpOK=200,
      httpCreated=201,
      httpAccepted=202,
      httpNonAuthoritativeInformation=203,
      httpNoContent=204,
      httpResetContent=205,
      httpPartialContent=206,
      httpMultiStatus=207,
      httpAlreadyReported=208,
      httpIMUsed=226,

      httpMultipleChoice=300,
      httpMovedPermanently=301,
      httpFound=302,
      httpSeeOther=303,
      httpNotModified=304,
      httpTemporaryRedirect=307,
      httpPermanentRedirect=308,

      httpBadRequest=400,
      httpUnauthorized=401,
      httpPaymentRequired=402,
      httpForbidden=403,
      httpNotFound=404,
      httpMethodNotAllowed=405,
      httpNotAcceptable=406,
      httpProxyAuthenticationRequired=407,
      httpRequestTimeout=408,
      httpConflict=409,
      httpGone=410,
      httpLengthRequired=411,
      httpPreConditionFailed=412,
      httpPayLoadTooLarge=413,
      httpUriTooLong=414,
      httpUnsupportedMediaType=415,
      httpRequestRangeNotSatisfiable=416,
      httpExpectationFailed=417,
      httpImaTeaPot=418,
      httpMisdirectedRequest=421,
      httpUnprocessableEntity=422,
      httpLocked=423,
      httpFailedDependency=424,
      httpTooEarly=425,
      httpUpgradeRequired=426,
      httpPreconditionRequired=428,
      httpTooManyRequest=429,
      httpHeaderFieldsTooLarge=431,
      httpUnavailableForLegalReasons=451,

      httpInternalServerError=500,
      httpNotImplemented=501,
      httpBadGateWay=502,
      httpServiceUnavailable=503,
      httpGatewayTimeOut=504,
      httpVersionNotSupported=505,
      httpVariantAlsoNegotiates=506,
      httpInsufficientStorage=507,
      httpLoopDetected=508,
      httpNotExtended=510,
      httpNetworkAuthenticationRequired=511
  );

  { THTTPResponsesHelper }
  THTTPResponsesHelper = type helper for THTTPResponses
    function code: word;
    function from(const _code: word) : THTTPResponses;
    function text: string;
   end;

implementation


{ THTTPResponsesHelper }

function THTTPResponsesHelper.code: word;
begin
    Result:= ord(Self);
end;

function THTTPResponsesHelper.from(const _code: word): THTTPResponses;
begin
  Self := THTTPResponses(_code);
  Result := Self;
end;

function THTTPResponsesHelper.text: string;
begin
    case self of
        httpContinue:
          Result := 'HTTP: Continue';
        httpSwitchProtocols:
          Result := 'HTTP: Switch Protocols';
        httpProcessing:
          Result := 'HTTP: Processing';
        httpEarlyHints:
          Result := 'HTTP: Early Hints';
        httpOK:
          Result := 'HTTP: OK';
        httpCreated:
          Result := 'HTTP: Created';
        httpAccepted:
          Result := 'HTTP: Accepted';
        httpNonAuthoritativeInformation:
          Result := 'HTTP: Non Authoritative Information';
        httpNoContent:
          Result := 'HTTP: No Content';
        httpResetContent:
          Result := 'HTTP: Reset Content';
        httpPartialContent:
          Result := 'HTTP: Partial Content';
        httpMultiStatus:
          Result := 'HTTP: Multi Status';
        httpAlreadyReported:
          Result := 'HTTP: Already Reported';
        httpIMUsed:
          Result := 'HTTP: IM Used';
        httpMultipleChoice:
          Result := 'HTTP: Multiple Choice';
        httpMovedPermanently:
          Result := 'HTTP: Moved Permanently';
        httpFound:
          Result := 'HTTP: Not Found';
        httpSeeOther:
          Result := 'HTTP: See Other';
        httpNotModified:
          Result := 'HTTP: Not Modified';
        httpTemporaryRedirect:
          Result := 'HTTP: Temporary Redirect';
        httpPermanentRedirect:
          Result := 'HTTP: Permanent Redirect';
        httpBadRequest:
          Result := 'HTTP: Bad Request';
        httpUnauthorized:
          Result := 'HTTP: Unauthorized';
        httpPaymentRequired:
          Result := 'HTTP: Payment Required';
        httpForbidden:
          Result := 'HTTP: Forbidden';
        httpNotFound:
          Result := 'HTTP: Not Found';
        httpMethodNotAllowed:
          Result := 'HTTP: Method not allowed';
        httpNotAcceptable:
          Result := 'HTTP: Not Acceptable';
        httpProxyAuthenticationRequired:
          Result := 'HTTP: Proxy Authentication Required';
        httpRequestTimeout:
          Result := 'HTTP: Request Timeout';
        httpConflict:
          Result := 'HTTP: Conflict';
        httpGone:
          Result := 'HTTP: Gone';
        httpLengthRequired:
          Result := 'HTTP: Length Required';
        httpPreConditionFailed:
          Result := 'HTTP: Precondition Failed';
        httpPayLoadTooLarge:
          Result := 'HTTP: Payload Too Large';
        httpUriTooLong:
          Result := 'HTTP: Uri Too Long';
        httpUnsupportedMediaType:
          Result := 'HTTP: Unsupported Media Type';
        httpRequestRangeNotSatisfiable:
          Result := 'HTTP: Request Range not satisfiable';
        httpExpectationFailed:
          Result := 'HTTP: Expectation failed';
        httpImaTeaPot:
          Result := 'HTTP: I am a teapot!!';
        httpMisdirectedRequest:
          Result := 'HTTP: Misdirected request';
        httpUnprocessableEntity:
          Result := 'HTTP: Unprocessable entity';
        httpLocked:
          Result := 'HTTP: Locked';
        httpFailedDependency:
          Result := 'HTTP: Failed dependency';
        httpTooEarly:
          Result := 'HTTP: Too early';
        httpUpgradeRequired:
          Result := 'HTTP: Upgrade required';
        httpPreconditionRequired:
          Result := 'HTTP: Precondition required';
        httpTooManyRequest:
          Result := 'HTTP: Too many requests';
        httpHeaderFieldsTooLarge:
          Result := 'HTTP: Header fields too large';
        httpUnavailableForLegalReasons:
          Result := 'HTTP: Unavailable for legal reasons';
        httpInternalServerError:
          Result := 'HTTP: Internal Server Error';
        httpNotImplemented:
          Result := 'HTTP: Not implemented';
        httpBadGateWay:
          Result := 'HTTP: Bad gateway';
        httpServiceUnavailable:
          Result := 'HTTP: Service unavailable';
        httpGatewayTimeOut:
          Result := 'HTTP: Gateway timeout';
        httpVersionNotSupported:
          Result := 'HTTP: Version not supported';
        httpVariantAlsoNegotiates:
          Result := 'HTTP: Variant also negotiates';
        httpInsufficientStorage:
          Result := 'HTTP: Insufficient storage';
        httpLoopDetected:
          Result := 'HTTP: Loop detected';
        httpNotExtended:
          Result := 'HTTP: Not extended';
        httpNetworkAuthenticationRequired:
          Result := 'HTTP: Network authentication required';
    end;
end;

end.

