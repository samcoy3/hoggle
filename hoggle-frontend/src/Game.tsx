import React from "react";
import "./main.css";

import { LobbyInfo } from "./types";

type GameProps = {
  lobbyInfo: LobbyInfo;
  nickname: string;
};

//For testing board css
// type GameProps = {
//   lobbyInfo: { lobbyCode: string; board: string[]; hostName: string;};
//   nickname: string;
// };

const Game = (props: GameProps) => {
  const letters = props.lobbyInfo.board;
  return (
    <div className="Game">
      {letters ? (
        <Board letters={letters} size={Math.sqrt(letters.length)} />
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
  return (
    <div className="board">
      <div className="board-contents">{getRows()}</div>
    </div>
  );
};

type RowProps = {
  letters: string[];
};

const Row = (props: RowProps) => {
  const getLetters = props.letters.map((letter, i) => <Tile letter={letter} />);
  return <div className="board-row">{getLetters}</div>;
};

type TileProps = {
  letter: string;
};

const Tile = (props: TileProps) => {
  return <div className="tile">{props.letter}</div>;
};

export default Game;
