import React from "react";

type WordListProps = {
  words?: Set<String>;
};

const WordList = (props: WordListProps) => {
  // If no words return an empty word list
  if (!props.words) return null;
  // Sort words alphabetically
  const sorted = Array.from(props.words).sort();
  const listWords = sorted.map((word, i) => (
    <div className="word" key={i}>
      {word}
    </div>
  ));
  return <div id="words-list">{listWords}</div>;
};

export default WordList;
