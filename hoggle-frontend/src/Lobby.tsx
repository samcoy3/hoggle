import React from "react";
import "./main.css";
import { ClickEventFunction, LastRound } from "./types";

type LobbyProps = {
  nickname: string;
  hostName: string;
  playerNames: string[];
  lastRound?: LastRound;
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
    <li key={i}>{player}</li>
  ));
  return <ul>{listPlayers}</ul>;
};

type LastRoundScoresProps = {
  playerNames: string[];
  lastRound: LastRound;
};

const LastRoundScores = (props: LastRoundScoresProps) => {
  const listPlayers = props.playerNames.map((player, i) => (
    <div className="player-score-card">
      <h1>{player}</h1>
      <PlayerWords
        words={props.lastRound.lastRoundWords[player]}
        points={props.lastRound.lastRoundPoints}
      />
    </div>
  ));
  return <ul>{listPlayers}</ul>;
};

type PlayerWordsProps = {
  words: string[];
  points: { [word: string]: number };
};

const PlayerWords = (props: PlayerWordsProps) => {
  const listWords = props.words.map((word, i) => (
    <section className="player-word-list">
      <div className="player-word">{word}</div>
      <div className="player-word-score">{props.points[word]}</div>
    </section>
  ));

  return <ul>{listWords}</ul>;
};

export default Lobby;
