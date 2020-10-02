import React from "react";
import PlayerWords from "./PlayerWords/PlayerWords";
import { LastRound } from "../../../../types";
import Board from "../../Board/Board";
import "./LastRoundScores.css"

type LastRoundScoresProps = {
  playerNames: string[];
  lastRound: LastRound;
};

const LastRoundScores = (props: LastRoundScoresProps) => {
  const getScore = (player: string) => {
    let total = 0;
    const words = props.lastRound.playerWords[player];
    const points = props.lastRound.wordPoints;
    if (words) {
      for (var i = 0; i < words.length; i++) {
        total += points[words[i]];
      }
    }
    return total;
  };
  const getPlayerCards = props.playerNames.map((player, i) => {
    if (props.lastRound.playerWords[player]) {
      return (
        <div className="score-card" key={i}>
          <h3 className="score-card-title">
            {player + " :: " + getScore(player)}
          </h3>
          <PlayerWords
            words={props.lastRound.playerWords[player]}
            wordPoints={props.lastRound.wordPoints}
            notInGrid={props.lastRound.notInGrid}
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
      <Board letters={props.lastRound.board}/>
    </div>
  );
};

export default LastRoundScores;
