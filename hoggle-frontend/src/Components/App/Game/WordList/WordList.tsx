import React from "react";
import { ParagraphClickEventFunction } from "../../../../types";
import "./WordList.css";

type WordListProps = {
  words?: Set<string>;
  handleRemoveWordFunction: ParagraphClickEventFunction;
};

const WordList = (props: WordListProps) => {
  // If no words return an empty word list
  if (!props.words) return null;
  // Sort words alphabetically
  const sorted = Array.from(props.words).sort();
  const listWords = sorted.map((word, i) => (
    <p onClick={props.handleRemoveWordFunction} className="word" key={i}>
      {word}
    </p>
  ));
  return <div id="words-list">{listWords}</div>;
};

export default WordList;
