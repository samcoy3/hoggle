import React from "react";
import "./main.css";
import {
  ClickEventFunction,
  LastRound,
  SubmitEventFunction,
  ChangeEventFunction,
} from "./types";
import { TextInputForm } from "./InputComponents";

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
    <div className="Lobby">
      {props.nickname === props.hostName ? (
        <div>
          {props.newSettings ? (
            <TextInputForm
              formName={"settings"}
              inputs={[
                {
                  name: "size",
                  value: props.newSettings.size,
                  label: "Board Size:",
                },
                {
                  name: "time",
                  value: props.newSettings.timeInSeconds,
                  label: "Game time (seconds):",
                },
              ]}
              handleChangeFunction={props.handleChangeFunction}
              handleSubmitFunction={props.handleSubmitFunction}
            />
          ) : null}
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
  const getScore = (player: string) => {
    let total = 0;
    const words = props.lastRound.lastRoundWords[player];
    const points = props.lastRound.lastRoundPoints;
    if (words) {
      for (var i = 0; i < words.length; i++) {
        total += points[words[i]];
      }
    }    
    return total;
  }
  const listPlayers = props.playerNames.map((player) => {
    if (props.lastRound.lastRoundWords[player]) {
      return (<div className="player-score-card">
      <h1>{player}</h1>
      <h2>{getScore(player)}</h2>
      <PlayerWords
        words={props.lastRound.lastRoundWords[player]}
        points={props.lastRound.lastRoundPoints}
      />
    </div>)
    }
  });
  return <ul>{listPlayers}</ul>;
};

type PlayerWordsProps = {
  words: string[];
  points: { [word: string]: number };
};

const PlayerWords = (props: PlayerWordsProps) => {
  const listWords = props.words ? props.words.map((word, i) => (
    <section className="player-word-list">
      <div className="player-word">{word}</div>
      <div className="player-word-score">{props.points[word]}</div>
    </section>
  )): null;

  return <ul>{listWords}</ul>;
};

export default Lobby;
