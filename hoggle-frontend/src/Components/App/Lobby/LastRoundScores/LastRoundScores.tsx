import React from "react";
import PlayerWords from "./PlayerWords/PlayerWords";
import { LastRound } from "../../../../types";

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
          <h3 className="score-card-title">
            {player + " :: " + getScore(player)}
          </h3>
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
      <h2 id="last-round-title">Last Round Scores</h2>
      <div id="score-card-container">{getPlayerCards}</div>
    </div>
  );
};

export default LastRoundScores;
