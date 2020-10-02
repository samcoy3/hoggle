import { FormEvent, ChangeEvent, MouseEvent } from "react";

export type ChangeEventFunction = (
  event: ChangeEvent<HTMLInputElement>
) => void;
export type SubmitEventFunction = (event: FormEvent<HTMLFormElement>) => void;
export type ButtonClickEventFunction = (event: MouseEvent<HTMLButtonElement>) => void;
export type ParagraphClickEventFunction = (event: MouseEvent<HTMLParagraphElement>) => void;

// Structure of server response with lobby info
export type LobbyResponse =
  | {
      state: {
        tag: "InLobby";
      };
      lobbyCode: string;
      hostName: string;
      currentSettings: CurrentSettings;
      playerNames: string[];
      lastRoundScores:
        | [{ [name: string]: string[] }, { [word: string]: number }]
        | null;
    }
  | {
      state: {
        tag: "StartingGame";
        contents: string;
      };
      lobbyCode: string;
      hostName: string;
      currentSettings: { size: number; timeInSeconds: number };
      playerNames: string[];
      lastRoundScores:
        | [{ [name: string]: string[] }, { [word: string]: number }]
        | null;
    }
  | {
      state: {
        tag: "InGame";
        contents: [string, [[any], string], string[]];
      };
      lobbyCode: string;
      hostName: string;
      currentSettings: { size: number; timeInSeconds: number };
      playerNames: string[];
      lastRoundScores:
        | [{ [name: string]: string[] }, { [word: string]: number }]
        | null;
    };

// Type for lobby info passed to App from server response 
export type LobbyInfo =
  | {
      state: "InLobby";
      lobbyCode: string;
      hostName: string;
      currentSettings: CurrentSettings;
      playerNames: string[];
      lastRoundScores?: LastRound;
    }
  | {
      state: "StartingGame";
      lobbyCode: string;
      hostName: string;
      currentSettings: CurrentSettings;
      changeTime: number;
    }
  | {
      state: "InGame";
      lobbyCode: string;
      hostName: string;
      currentSettings: CurrentSettings;
      changeTime: number;
      board: string[];
      words: string[];
    };

export type GameData = {
  board: string[];
  word: string;
  words: Set<string>;
};

export type LastRound = {
  lastRoundWords: { [name: string]: string[] };
  lastRoundPoints: { [word: string]: number };
};

type CurrentSettings = {
  size: number;
  timeInSeconds: number;
};

export type NewSettings = {
  size: string;
  timeInSeconds: string;
};

export type AdminSettings = {
  currentSettings: CurrentSettings;
  newSettings: NewSettings;
};
