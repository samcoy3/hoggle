import React from "react";
import "./PlayerWords.css"

type PlayerWordsProps = {
  words: string[];
  wordPoints: { [word: string]: number };
  notInGrid: string[];
};

const PlayerWords = (props: PlayerWordsProps) => {
  const listWords = props.words
    ? props.words.map((word, i) => {
        const points = props.wordPoints[word];
        return (
          <div
            key={i}
            className={`score-card-word ${
              points > 0
                ? "right"
                : props.notInGrid.includes(word)
                ? "not-in-grid"
                : points === 0
                ? "shared"
                : "wrong"
            }`}
          >
            {word + " :: " + points}
          </div>
        );
      })
    : null;
  return <div className="score-card-word-list">{listWords}</div>;
};

export default PlayerWords;
