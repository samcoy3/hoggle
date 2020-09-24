import qs from "qs";
import { LobbyResponse, LobbyInfo } from "./types";

const API = "https://hoggle.meme.ninja/api";
const LOBBY_NEW_ENDPOINT = API + "/lobby/new";
const LOBBY_JOIN_ENDPOINT = API + "/lobby/join";
const LOBBY_INFO_ENDPOINT = API + "/lobby/info";
const LOBBY_SETTINGS_ENDPOINT = API + "/lobby/settings";
const LOBBY_STARTGAME_ENDPOINT = API + "/lobby/startgame";
const LOBBY_LEAVE_ENDPOINT = API + "/lobby/leave";
const GAME_SENDWORD_ENDPOINT = API + "/game/sendword";
const GAME_REMOVEWORD_ENDPOINT = API + "/game/removeword";
const GAME_REROLL_ENDPOINT = API + "/game/reroll";

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
): Promise<false | string> => {
  const response = await post(endpoint, data);
  if (!response.ok) {
    const content = await response.text();
    alert(response.status + " " + content);
    return false;
  } else {
    const content = await response.json();
    return content;
  }
};

const info = async (uuid: string): Promise<false | LobbyInfo> => {
  const data = qs.stringify({ uuid: uuid });
  const response = await post(LOBBY_INFO_ENDPOINT, data);
  if (!response.ok) {
    const content = await response.text();
    alert(response.status + " " + content);
    return false;
  } else {
    const json = await response.json();
    return toLobbyInfo(json);
  }
};

const toLobbyInfo = (json: LobbyResponse): LobbyInfo => {
  let time;
  let board;
  let words;
  let lastRoundScores;

  if (json.state.tag === "StartingGame") {
    time = Date.parse(json.state.contents);
  } else if (json.state.tag === "InGame") {
    time = Date.parse(json.state.contents[0]);
    board = [];
    for (var i = 0; i < json.state.contents[1].length; i++) {
      board.push(json.state.contents[1][i][1]);
    }
    words = json.state.contents[2];
  }

  if (json.lastRoundScores !== null) {
    lastRoundScores = {
      lastRoundWords: json.lastRoundScores[0],
      lastRoundPoints: json.lastRoundScores[1],
    };
  }

  if (time && board && words) {
    return {
      currentSettings: json.currentSettings,
      hostName: json.hostName,
      lastRoundScores: lastRoundScores,
      lobbyCode: json.lobbyCode,
      playerNames: json.playerNames,
      state: "InGame",
      endTime: time,
      board: board,
      words: words,
    };
  } else if (time) {
    return {
      currentSettings: json.currentSettings,
      hostName: json.hostName,
      lastRoundScores: lastRoundScores,
      lobbyCode: json.lobbyCode,
      playerNames: json.playerNames,
      state: "StartingGame",
      startTime: time,
    };
  } else {
    return {
      currentSettings: json.currentSettings,
      hostName: json.hostName,
      lastRoundScores: lastRoundScores,
      lobbyCode: json.lobbyCode,
      playerNames: json.playerNames,
      state: "InLobby",
    };
  }
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
  alert(data);
  return await getSuccess(LOBBY_LEAVE_ENDPOINT, data);
};

const sendWord = async (uuid: string, word: string) => {
  const data = qs.stringify({ uuid: uuid, word: word });
  return await getSuccess(GAME_SENDWORD_ENDPOINT, data);
};

const removeWord = async (uuid: string, word: string) => {
  const data = qs.stringify({ uuid: uuid, word: word });
  return await getSuccess(GAME_REMOVEWORD_ENDPOINT, data);
};

const reroll = async (uuid: string) => {
  const data = qs.stringify({ uuid: uuid });
  return await getSuccess(GAME_REROLL_ENDPOINT, data);
};

const getSuccess = async (endpoint: string, data: string) => {
  const response = await post(endpoint, data);
  if (!response.ok) {
    const content = await response.text();
    alert(response.status + " " + content);
    return false;
  } else {
    return true;
  }
};

export {
  newLobby,
  join,
  info,
  settings,
  start,
  leave,
  sendWord,
  removeWord,
  reroll,
};