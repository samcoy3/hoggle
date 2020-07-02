import { FormEvent, ChangeEvent, MouseEvent } from "react";

export type ChangeEventFunction = (
  event: ChangeEvent<HTMLInputElement>
) => void;
export type SubmitEventFunction = (event: FormEvent<HTMLFormElement>) => void;
export type ClickEventFunction = (event: MouseEvent<HTMLButtonElement>) => void;

export enum GameState {
  InLanding,
  InLobby,
  InGame,
  JoiningLobby,
}

export type LobbyState = "InLobby" | "StartingGame" | "InGame";

export type LobbyResponse = {
  currentSettings: { size: number; timeInSeconds: number };
  hostName: string;
  lastRoundScores: null;
  lobbyCode: string;
  playerNames: string[];
  state: {
    tag: LobbyState;
    contents: string | [string, [[any], string]];
  };
};
export type LobbyInfo = {
  currentSettings: { size: number; timeInSeconds: number };
  hostName: string;
  lastRoundScores: null;
  lobbyCode: string;
  playerNames: string[];
  state: LobbyState;
  time?: number;
  board?: string[];
};
