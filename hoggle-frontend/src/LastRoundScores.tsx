import React from "react";
import "./main.css";
import { LastRound } from "./types";
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
  };
  const getPlayerCards = props.playerNames.map((player, i) => {
    if (props.lastRound.lastRoundWords[player]) {
      return (
        <div className="score-card" key={i}>
          <div className="score-card-title">
            {player + " :: " + getScore(player)}
          </div>
          <PlayerWords
            words={props.lastRound.lastRoundWords[player]}
            points={props.lastRound.lastRoundPoints}
          />
        </div>
      );
    } else {
      return null;
    }
  });
  return (
    <div id="last-round">
      <div id="last-round-title">Last Round Scores</div>
      <div id="score-card-container">{getPlayerCards}</div>
    </div>
  );
};

type PlayerWordsProps = {
  words: string[];
  points: { [word: string]: number };
};

const PlayerWords = (props: PlayerWordsProps) => {
  const listWords = props.words
    ? props.words.map((word, i) => {
        const points = props.points[word];
        return (
          <div
            key={i}
            className={`score-card-word ${
              points > 0 ? "right" : points === 0 ? "shared" : "wrong"
            }`}
          >
            {word + " :: " + points}
          </div>
        );
      })
    : null;
  return <div className="score-card-word-list">{listWords}</div>;
};

export { LastRoundScores };
