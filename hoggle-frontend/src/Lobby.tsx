import React from "react";
import "./main.css";
import { ClickEventFunction, LastRound } from "./types";

type LobbyProps = {
  nickname: string;
  hostName: string;
  playerNames: string[];
  lastRoundScores?: LastRound;
  changeSettingsFunction: ClickEventFunction;
  startGameFunction: ClickEventFunction;
};

const Lobby = (props: LobbyProps) => {
  return (
    <div className="Lobby">
      {props.nickname === props.hostName ? (
        <div>
          <button onClick={props.changeSettingsFunction}>
            Change Settings
          </button>
          <button onClick={props.startGameFunction}>Start Game</button>
        </div>
      ) : null}
      <PlayerList players={props.playerNames} />
      {props.lastRoundScores ? (
        <LastRoundScores
          playerNames={props.playerNames}
          lastRoundScores={props.lastRoundScores}
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

type LastRoundScoresProps = {
  playerNames: string[];
  lastRoundScores: LastRound;
};

const LastRoundScores = (props: LastRoundScoresProps) => {
  const listPlayers = props.playerNames.map((player, i) => (
    <li key={i}>
      <h1>{player}</h1>
      <PlayerWords words={props.lastRoundScores.lastRoundWords[player]} />
    </li>
  ));
  return <ul>{listPlayers}</ul>;
};

type PlayerWordsProps = {
  words: string[];
};

const PlayerWords = (props: PlayerWordsProps) => {
  const listWords = props.words.map((word, i) => <li key={i}>{word}</li>);
  return <ul>{listWords}</ul>;
};

export default Lobby;
