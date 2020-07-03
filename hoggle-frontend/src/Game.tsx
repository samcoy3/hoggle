import React from "react";
import "./main.css";

import { ChangeEventFunction, SubmitEventFunction } from "./types";

type GameProps = {
  lobbyCode: string;
  board: string[];
  hostName: string;
  startTime: number;
  nickname: string;
  word: string;
  words: Set<String>;
  wordChangeFunction: ChangeEventFunction;
  wordSubmitFunction: SubmitEventFunction;
};

const Game = (props: GameProps) => {
  const startTime = props.startTime;
  const [counter, setCounter] = React.useState(
    startTime > Date.now()
      ? Math.ceil((startTime - Date.now()) / 1000)
      : Math.ceil((startTime + 180 * 1000 - Date.now()) / 1000)
  );

  React.useEffect(() => {
    startTime > Date.now() &&
      setTimeout(
        () => setCounter(Math.ceil((startTime - Date.now()) / 1000)),
        1000
      );
    startTime <= Date.now() &&
      setTimeout(
        () =>
          setCounter(Math.ceil((startTime + 180 * 1000 - Date.now()) / 1000)),
        1000
      );
  }, [counter]);

  return (
    <div>
      <div className="timer">
        <h1>{counter}</h1>
      </div>
      <div className="game">
        <Board letters={props.board} size={Math.sqrt(props.board.length)} />
        <div className="words">
          <SingleInputForm
            name={"word"}
            inputValue={props.word}
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

type PlayerListProps = {
  players: string[];
};

const PlayerList = (props: PlayerListProps) => {
  const listPlayers = props.players.map((player, i) => (
    <li key={i}>{player}</li>
  ));
  return <ul>{listPlayers}</ul>;
};

type WordListProps = {
  words: Set<String>;
};

const WordList = (props: WordListProps) => {
  const sorted = Array.from(props.words).sort();
  const listWords = sorted.map((word, i) => (
    <li key={i}>{word}</li>
  ))
  return <ul>{listWords}</ul>
};

type BoardProps = {
  letters: string[];
  size: number;
};

const Board = (props: BoardProps) => {
  const getRows = () => {
    const letters = props.letters;
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

type SingleInputFormProps = {
  name: string;
  inputLabel?: string;
  inputValue: string;
  handleChangeFunction: ChangeEventFunction;
  handleSubmitFunction: SubmitEventFunction;
};

const SingleInputForm = (props: SingleInputFormProps) => (
  <div className="form-wrapper">
    <form name={props.name} onSubmit={props.handleSubmitFunction} noValidate>
      <TextInput
        name={"word"}
        label={props.inputLabel}
        value={props.inputValue}
        handleChangeFunction={props.handleChangeFunction}
      />
      <input type="submit" value="Sumbit" />
    </form>
  </div>
);

type TextInputProps = {
  label?: string;
  name: string;
  value: string;
  valid?: boolean;
  handleChangeFunction: ChangeEventFunction;
  info?: string;
};

const TextInput = (props: TextInputProps) => (
  <div className="text-input-wrapper">
    {props.label ? <label htmlFor={props.name}>{props.label}</label> : null}
    <input
      className={`text-input ${props.valid === false ? "invalid" : ""}`}
      type="text"
      name={props.name}
      value={props.value}
      onChange={props.handleChangeFunction}
    />
    {props.info ? (
      <div className="info">
        <small>{props.info}</small>
      </div>
    ) : null}
  </div>
);

export default Game;
