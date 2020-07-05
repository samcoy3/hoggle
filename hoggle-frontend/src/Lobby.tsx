import React from "react";
import "./main.css";
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
    <div className="player-name" key={i}>
      {player}
    </div>
  ));
  return (
    <div className="player-list">
      <h2>Players In Lobby:</h2>
      {listPlayers}
    </div>
  );
};

export default Lobby;
