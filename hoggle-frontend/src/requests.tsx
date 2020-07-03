import qs from "qs";
import { LobbyResponse, LobbyInfo } from "./types";

const API = "http://hoggle.meme.ninja/api";
const LOBBY_NEW_ENDPOINT = API + "/lobby/new";
const LOBBY_JOIN_ENDPOINT = API + "/lobby/join";
const LOBBY_INFO_ENDPOINT = API + "/lobby/info";
const LOBBY_SETTINGS_ENDPOINT = API + "/lobby/settings";
const LOBBY_STARTGAME_ENDPOINT = API + "/lobby/startgame";
const LOBBY_LEAVE_ENDPOINT = API + "/lobby/leave";
const GAME_SENDWORD_ENDPOINT = API + "/game/sendword";
const GAME_DELETEWORD_ENDPOINT = API + "/game/deleteword";

const post = async (endpoint: string, data: any): Promise<Response> => {
  const requestOptions = {
    method: "POST",
    headers: { "Content-Type": "application/x-www-form-urlencoded" },
    body: data,
  };
  return fetch(endpoint, requestOptions);
};

const newLobby = async (nickname: string) => {
  const data = qs.stringify({ name: nickname });
  return fetchUUID(LOBBY_NEW_ENDPOINT, data);
};

const join = async (nickname: string, lobbyCode: string) => {
  const data = qs.stringify({ name: nickname, code: lobbyCode });
  return fetchUUID(LOBBY_JOIN_ENDPOINT, data);
};

const fetchUUID = async (
  endpoint: string,
  data: any
): Promise<{ success: boolean; data: string }> => {
  return post(endpoint, data)
    .then((response) => {
      if (!response.ok) {
        throw new Error(response.status.toString() + response.statusText);
      }
      console.log(response);
      return response.json();
    })
    .then((uuid: string) => {
      console.log(uuid);
      return { success: true, data: uuid };
    })
    .catch((e) => {
      if (e instanceof Error) {
        console.error(e.message);
        return { success: false, data: e.message };
      }
      throw e;
    });
};

const info = async (uuid: string) => {
  const data = qs.stringify({ uuid: uuid });
  return post(LOBBY_INFO_ENDPOINT, data)
    .then((response) => {
      if (!response.ok) {
        throw new Error(response.statusText);
      }
      return response.json();
    })
    .then((json) => {
      console.log(json);
      return toLobbyInfo(json);
    })
    .catch((e) => {
      if (e instanceof Error) {
        console.error(e.message);
      }
      throw e;
    });
};

const toLobbyInfo = (json: LobbyResponse): LobbyInfo => {
  let time;
  let board;
  let words;

  if (json.state.tag === "StartingGame") {
    time = Date.parse(json.state.contents);
  } else if (json.state.tag === "InGame") {
    time = Date.parse(json.state.contents[0]);
    board = [];
    for (var i = 0; i < 25; i++) {
      board.push(json.state.contents[1][i][1]);
    }
    words = json.state.contents[2];
  }

  return {
    currentSettings: json.currentSettings,
    hostName: json.hostName,
    lastRoundScores: json.lastRoundScores,
    lobbyCode: json.lobbyCode,
    playerNames: json.playerNames,
    state: json.state.tag,
    startTime: time,
    board: board,
    words: words,
  };
};

const settings = async (uuid: string, size: number, seconds: number) => {
  const data = qs.stringify({ uuid: uuid, size: size, timeInSeconds: seconds });
  return await getSuccess(LOBBY_SETTINGS_ENDPOINT, data);
};

const start = async (uuid: string) => {
  const data = qs.stringify({ uuid: uuid });
  return await getSuccess(LOBBY_STARTGAME_ENDPOINT, data);
};

const leave = async (uuid: string) => {
  const data = qs.stringify({ uuid: uuid });
  return await getSuccess(LOBBY_LEAVE_ENDPOINT, data);
};

const sendWord = async (uuid: string, word: string) => {
  const data = qs.stringify({ uuid: uuid, word: word });
  return await getSuccess(GAME_SENDWORD_ENDPOINT, data);
};

const deleteWord = async (uuid: string, word: string) => {
  const data = qs.stringify({ uuid: uuid, word: word });
  return await getSuccess(GAME_DELETEWORD_ENDPOINT, data);
};

const getSuccess = async (endpoint: string, data: string) => {
  const response = await post(endpoint, data);
  if (!response.ok) {
    const errorMessage = await response.text();
    alert(errorMessage);
    return false;
  } else {
    return true;
  }
};

export { newLobby, join, info, settings, start, leave, sendWord, deleteWord };
