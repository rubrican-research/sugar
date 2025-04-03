unit git.api;

{$mode ObjFPC}{$H+}
interface

uses
  Process, SysUtils, fphttpclient, fpjson, jsonparser;

function  repoOwner: string;
procedure repoOwner(const _owner: string);

function gitLocalCommitSHA(const _localRepo: string): string;
function gitCheckUpdates(const _remoteRepo: string): string;

function gitCmd(_repo: string; _command: TStringArray): string;

implementation
const
    GITHUB_API_URL = 'https://api.github.com/repos/';

var
  REPO_OWNER: string = 'your_username';   // Replace with the repository owner's username
  REPO_NAME: string  = 'your_repository'; // Replace with the repository name



function gitCmd(_repo: string; _command: TStringArray): string;
var
    gitexe: TProcessString = 'git';
    {out} outputstring: string;
    {out} exitStatus: integer;
    i : integer;
begin

    i := RunCommandIndir(_repo, gitexe, _command, outputstring, exitStatus, [], swoHIDE);
    if i = 0 then
        Result := outPutString
    else
        Result := Format('Failed: returned %s (%d). Return value: %d', [ outputstring, exitStatus, i]);
end;

function repoOwner: string;
begin
    Result := REPO_OWNER;
end;

procedure repoOwner(const _owner: string);
begin
    REPO_OWNER := _owner;
end;

function gitLocalCommitSHA(const _localRepo: string): string;
const
  cmd: TStringArray = ('rev-parse', 'HEAD');
begin
    result := gitCmd(_localRepo, cmd);
end;

function gitCheckUpdates(const _remoteRepo: string): string;
var
  HttpClient: TFPHTTPClient;
  Response: String;
  JsonResponse: TJSONData;
  LocalCommitSha: String;
  RemoteCommitSha: String;

begin
  HttpClient := TFPHTTPClient.Create(nil);
  try
    // Set user agent to avoid GitHub API rejection
    HttpClient.AddHeader('User-Agent', 'Mozilla/5.0');

    // Fetch the latest commit information from GitHub
    Response := HttpClient.Get(GITHUB_API_URL + REPO_OWNER + '/' + REPO_NAME + '/commits/main'); // Change 'main' if your default branch is different

    // Parse the JSON response
    JsonResponse := GetJSON(Response);
    try
      // Get the SHA of the latest commit from the response
      RemoteCommitSha := JsonResponse.FindPath('sha').Value;

      // Here, you should set LocalCommitSha to the SHA of your local repository's latest commit.
      // This is typically done using a git command or by reading it from a local file.
      // For demonstration purposes, let's assume it's hardcoded.
      LocalCommitSha := 'your_local_commit_sha'; // Replace with your local commit SHA

      // Compare local and remote commit SHAs
      if LocalCommitSha <> RemoteCommitSha then
        Writeln('There are newer changes available. You need to pull updates.')
      else
        Writeln('Your local repository is up-to-date.');
    finally
      JsonResponse.Free;
    end;

  except
    on E: Exception do
      Writeln('An error occurred: ', E.Message);
  end;

  HttpClient.Free;
end;




end.

