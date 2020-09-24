import React from "react";

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

export default PlayerWords;
