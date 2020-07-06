import React from "react";
import {
  ChangeEventFunction,
  SubmitEventFunction,
  ClickEventFunction,
} from "./types";
import { TextInputForm } from "./InputComponents";
import { AdminPanel } from "./AdminPanel";

type GameProps = {
  board: string[];
  hostName: string;
  nickname: string;
  newSettings?: { size: string; timeInSeconds: string };
  word: string;
  words: Set<String>;
  rerollGameFunction: ClickEventFunction;
  handleChangeFunction: ChangeEventFunction;
  handleSubmitFunction: SubmitEventFunction;
  counter?: number;
};

const Game = (props: GameProps) => {
  return (
    <div id="game">
      {props.nickname === props.hostName && props.newSettings ? (
        <AdminPanel
          location="Game"
          newSettings={props.newSettings}
          handleChangeFunction={props.handleChangeFunction}
          handleSubmitFunction={props.handleSubmitFunction}
          gameFunction={props.rerollGameFunction}
        />
      ) : null}
      <div id="timer">
        {props.counter ? <h1>{props.counter}</h1> : <h1>--</h1>}
      </div>
      <div id="play-area">
        <Board letters={props.board} size={Math.sqrt(props.board.length)} />
        <div id="words">
          <div id="word-input">
            <TextInputForm
              formName={"word"}
              inputs={[{ name: "word", value: props.word }]}
              handleChangeFunction={props.handleChangeFunction}
              handleSubmitFunction={props.handleSubmitFunction}
            />
          </div>
          <WordList words={props.words} />
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
  const listWords = sorted.map((word, i) => (
    <div className="word" key={i}>
      {word}
    </div>
  ));
  return <div id="words-list">{listWords}</div>;
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
  return <div id="board">{getRows()}</div>;
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
