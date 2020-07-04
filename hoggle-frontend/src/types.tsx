import { FormEvent, ChangeEvent, MouseEvent } from "react";

export type ChangeEventFunction = (
  event: ChangeEvent<HTMLInputElement>
) => void;
export type SubmitEventFunction = (event: FormEvent<HTMLFormElement>) => void;
export type ClickEventFunction = (event: MouseEvent<HTMLButtonElement>) => void;

export type LobbyState = "InLobby" | "StartingGame" | "InGame";

export type LobbyResponse = {
  currentSettings: { size: number; timeInSeconds: number };
  hostName: string;
  lastRoundScores: null;
  lobbyCode: string;
  playerNames: string[];
  state: {
    tag: "InLobby";
  };
} | {
  currentSettings: { size: number; timeInSeconds: number };
  hostName: string;
  lastRoundScores: null;
  lobbyCode: string;
  playerNames: string[];
  state: {
    tag: "StartingGame";
    contents: string;
  };
} | {
  currentSettings: { size: number; timeInSeconds: number };
  hostName: string;
  lastRoundScores: null;
  lobbyCode: string;
  playerNames: string[];
  state: {
    tag: "InGame";
    contents: [string, [[any], string], string[]];
  };
};

export type LobbyInfo = {
  currentSettings: { size: number; timeInSeconds: number };
  hostName: string;
  lastRoundScores: null;
  lobbyCode: string;
  playerNames: string[];
  state: "InLobby";
} | {
  currentSettings: { size: number; timeInSeconds: number };
  hostName: string;
  lastRoundScores: null;
  lobbyCode: string;
  playerNames: string[];
  state: "StartingGame";
  startTime: number;
} | {
  currentSettings: { size: number; timeInSeconds: number };
  hostName: string;
  lastRoundScores: null;
  lobbyCode: string;
  playerNames: string[];
  state: "InGame";
  startTime: number;
  board: string[];
  words: string[];
};
