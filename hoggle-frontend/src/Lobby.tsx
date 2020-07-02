import React from "react";
import "./main.css";
import { LobbyInfo, ClickEventFunction } from "./types";

type LobbyProps = {
  lobbyInfo: LobbyInfo;
  nickname: string;
  startGameFunction: ClickEventFunction;
};

const Lobby = (props: LobbyProps) => {
  return (
    <div className="Lobby">
      {props.nickname === props.lobbyInfo.hostName ? (
        <button onClick={props.startGameFunction}>Start Game</button>
      ) : null}
      <PlayerList players={props.lobbyInfo.playerNames} />
      {props.lobbyInfo.board ? (
        <Board
          letters={props.lobbyInfo.board}
          size={props.lobbyInfo.currentSettings.size}
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
    <li key={i}>{player}</li>
  ));
  return <ul>{listPlayers}</ul>;
};

type BoardProps = {
  letters: string[];
  size: number;
};

const Board = (props: BoardProps) => {
  const getRows = () => {
    const letters = props.letters;
    const size = props.size;
    var rows = [];
    for (var i = 0; i < size; i++) {
      const start = i * size;
      const end = i * size + size;
      rows.push(<Row key={i} letters={props.letters.slice(start, end)} />);
    }
    return rows;
  };
  return <section>{getRows()}</section>;
};

type RowProps = {
  letters: string[];
};

const Row = (props: RowProps) => {
  const getLetters = props.letters.map((letter, i) => <h1 key={i}>{letter}</h1>);
  return <section className="board-row">{getLetters}</section>;
};

export default Lobby;
