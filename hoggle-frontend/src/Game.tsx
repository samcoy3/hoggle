import React from "react";
import "./main.css";

import {
  ChangeEventFunction,
  SubmitEventFunction,
  ClickEventFunction,
} from "./types";
import { TextInputForm } from "./InputComponents";
import { hostname } from "os";

type GameProps = {
  board: string[];
  hostName: string;
  nickname: string;
  word: string;
  words: Set<String>;
  rerollFunction: ClickEventFunction;
  wordChangeFunction: ChangeEventFunction;
  wordSubmitFunction: SubmitEventFunction;
  counter?: number;
};

const Game = (props: GameProps) => {
  return (
    <div>
      {props.hostName === props.nickname ? (
        <button id="reroll-button" onClick={props.rerollFunction}>
          Reroll Board
        </button>
      ) : null}
      <div className="timer">
        {props.counter ? <h1>{props.counter}</h1> : <h1>--</h1>}
      </div>
      <div className="game">
        <Board letters={props.board} size={Math.sqrt(props.board.length)} />
        <div className="words">
          <TextInputForm
            formName={"word"}
            inputs={[{ name: "word", value: props.word }]}
            handleChangeFunction={props.wordChangeFunction}
            handleSubmitFunction={props.wordSubmitFunction}
          />
          <div className="words-list">
            <WordList words={props.words} />
          </div>
        </div>
      </div>
    </div>
  );
};

type WordListProps = {
  words: Set<String>;
};

const WordList = (props: WordListProps) => {
  const sorted = Array.from(props.words).sort();
  const listWords = sorted.map((word, i) => <li key={i}>{word}</li>);
  return <ul>{listWords}</ul>;
};

type BoardProps = {
  letters: string[];
  size: number;
};

const Board = (props: BoardProps) => {
  const getRows = () => {
    const size = props.size;
    var rows = [];
    for (var i = 0; i < size; i++) {
      const start = i * size;
      const end = i * size + size;
      rows.push(<Row key={i} letters={props.letters.slice(start, end)} />);
    }
    return rows;
  };
  return <div className="board-contents">{getRows()}</div>;
};

type RowProps = {
  letters: string[];
};

const Row = (props: RowProps) => {
  const getLetters = props.letters.map((letter, i) => <Tile letter={letter} />);
  return <div className="board-row">{getLetters}</div>;
};

type TileProps = {
  letter: string;
};

const Tile = (props: TileProps) => {
  return <div className="tile">{props.letter}</div>;
};

export default Game;
