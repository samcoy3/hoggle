import React from "react";
import {
  ClickEventFunction,
  LastRound,
  SubmitEventFunction,
  ChangeEventFunction,
} from "./types";
import { AdminPanel } from "./AdminPanel";
import { LastRoundScores } from "./LastRoundScores";

type LobbyProps = {
  nickname: string;
  hostName: string;
  playerNames: string[];
  lastRound?: LastRound;
  newSettings?: { size: string; timeInSeconds: string };
  handleChangeFunction: ChangeEventFunction;
  handleSubmitFunction: SubmitEventFunction;
  startGameFunction: ClickEventFunction;
};

const Lobby = (props: LobbyProps) => {
  return (
    <div id="lobby">
      {props.nickname === props.hostName && props.newSettings ? (
        <AdminPanel
          location="Lobby"
          newSettings={props.newSettings}
          handleChangeFunction={props.handleChangeFunction}
          handleSubmitFunction={props.handleSubmitFunction}
          gameFunction={props.startGameFunction}
        />
      ) : null}
      <PlayerList players={props.playerNames} />
      {props.lastRound ? (
        <LastRoundScores
          playerNames={props.playerNames}
          lastRound={props.lastRound}
        />
      ) : null}
    </div>
  );
};

type PlayerListProps = {
  players: string[];
};

const PlayerList = (props: PlayerListProps) => {
  const listPlayers = props.players.map((player, i) => (
    <h4 className="player-name" key={i}>
      {player}
    </h4>
  ));
  return (
    <div id="lobby-players">
      <h2 id="lobby-players-title">Players In Lobby:</h2>
      <div id="player-list">{listPlayers}</div>
    </div>
  );
};

export default Lobby;
