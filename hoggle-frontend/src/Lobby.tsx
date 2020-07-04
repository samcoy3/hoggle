import React from "react";
import "./main.css";
import { LobbyInfo, ClickEventFunction } from "./types";

type LobbyProps = {
  nickname: string;
  hostName: string;
  playerNames: string[];
  startGameFunction: ClickEventFunction;
};

//For testing board css
// type LobbyProps = {
//   lobbyInfo: { hostName: string; playerNames: string[] };
//   nickname: string;
//   startGameFunction: ClickEventFunction;
// };

const Lobby = (props: LobbyProps) => {
  return (
    <div className="Lobby">
      {props.nickname === props.hostName ? (
        <button onClick={props.startGameFunction}>Start Game</button>
      ) : null}
      <PlayerList players={props.playerNames} />
    </div>
  );
};

type PlayerListProps = {
  players: string[];
};

const PlayerList = (props: PlayerListProps) => {
  const listPlayers = props.players.map((player, i) => (
    <li key={i}>{player}</li>
  ));
  return <ul>{listPlayers}</ul>;
};

export default Lobby;
