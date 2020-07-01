import qs from "qs";

type UUIDResponse = { UUID: string };

type LobbyState = "InLobby" | "InGame";

type LobbyResponse = {
  settings: { size: number; timeInSeconds: number };
  joinCode: string;
  lobbyState: { tag: LobbyState };
  players: string[];
  host: string;
  //previousRound?:
};
type LobbyInfo = {
  settings: { size: number; timeInSeconds: number };
  joinCode: string;
  lobbyState: LobbyState;
  players: string[];
  host: string;
  //previousRound?:
};

const API = "http://hoggle.meme.ninja/api";
const LOBBY_NEW_ENDPOINT = API + "/lobby/new";
const LOBBY_JOIN_ENDPOINT = API + "/lobby/join";
const LOBBY_INFO_ENDPOINT = API + "/lobby/info";
const LOBBY_SETTINGS_ENDPOINT = API + "/lobby/settings";
const LOBBY_STARTGAME_ENDPOINT = API + "/lobby/startgame";
const LOBBY_LEAVE_ENDPOINT = API + "/lobby/leave";
const GAME_SENDWORD_ENDPOINT = API + "/game/sendword";
const GAME_DELETEWORD_ENDPOINT = API + "/game/deleteword";

const newLobby = async (nickname: string) => {
  const data = "name=" + nickname;
  return fetchUUID(LOBBY_NEW_ENDPOINT, data);
};

const join = async (nickname: string, lobbyCode: string) => {
  const data = "name=" + nickname + "&code=" + lobbyCode;
  return fetchUUID(LOBBY_JOIN_ENDPOINT, data);
};

const fetchUUID = async (
  endpoint: string,
  data: any
): Promise<{ success: boolean; data: string }> => {
  const requestOptions = {
    method: "POST",
    headers: { "Content-Type": "application/x-www-form-urlencoded" },
    body: data,
  };
  alert(data)
  return fetch(endpoint, requestOptions)
    .then(function (response) {
      if (!response.ok) {
        throw new Error(response.statusText);
      }
      return response.json();
    })
    .then(function (json: UUIDResponse) {
      return { success: true, data: json.UUID };
    })
    .catch(function (err) {
      if (err instanceof Error) {
        return { success: false, data: err.message };
      }
      throw err;
    });
};

function info(uuid: string) {
  const json: LobbyResponse = {
    settings: { size: 5, timeInSeconds: 180 },
    joinCode: "LOBY",
    lobbyState: { tag: "InLobby" },
    players: ["Steve", "Jim"],
    host: "Steve",
    // previousRoundScores: null,
  };
  return { success: true, lobby: toLobbyInfo(json) };
}

// const info = async (props: any) => {
//   const requestOptions = {
//     method: "POST",
//     headers: { "Content-Type": "application/json" },
//     body: JSON.stringify({ UUID: props.uuid }),
//   };
//   const response = await fetch(LOBBY_INFO_ENDPOINT, requestOptions);
//   if (!response.ok) {
//     return { success: false, data: response.status.toString() };
//   } else {
//     return { success: true, data: await response.json() };
//   }
// };

const toLobbyInfo = (json: LobbyResponse): LobbyInfo => {
  return {
    settings: json.settings,
    joinCode: json.joinCode,
    lobbyState: json.lobbyState.tag,
    players: json.players,
    host: json.host,
  };
};

export { newLobby, join };
