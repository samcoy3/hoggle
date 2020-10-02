import React from "react";
import {
  ChangeEventFunction,
  SubmitEventFunction,
  ButtonClickEventFunction,
  ParagraphClickEventFunction,
  NewSettings,
  GameData,
} from "../../../types";
import TextInputForm from "../TextInputForm/TextInputForm";
import AdminPanel from "../AdminPanel/AdminPanel";
import Board from "./Board/Board";
import WordList from "./WordList/WordList";

type GameProps = {
  gameData?: GameData;
  newSettings?: NewSettings;
  counter?: number;
  handleRerollGameFunction: ButtonClickEventFunction;
  handleChangeInputFunction: ChangeEventFunction;
  handleSubmitWordFunction: SubmitEventFunction;
  handleRemoveWordFunction: ParagraphClickEventFunction;
};

const Game = (props: GameProps) => {
  return (
    <div id="game">
      {props.newSettings ? (
        <AdminPanel
          location="Game"
          newSettings={props.newSettings}
          handleChangeFunction={props.handleChangeInputFunction}
          handleSubmitFunction={props.handleSubmitWordFunction}
          gameFunction={props.handleRerollGameFunction}
        />
      ) : null}
      <div id="timer">
        {props.counter ? <h1>{props.counter}</h1> : <h1>--</h1>}
      </div>
      <div id="play-area">
        <Board letters={props.gameData?.board} />
        <div id="words">
          <div id="word-input">
            <TextInputForm
              formName={"word"}
              inputs={[
                {
                  name: "word",
                  value: props.gameData ? props.gameData.word : "",
                },
              ]}
              autocompleteOff={true}
              handleChangeFunction={props.handleChangeInputFunction}
              handleSubmitFunction={props.handleSubmitWordFunction}
            />
          </div>
          <WordList
            words={props.gameData ? props.gameData.words : undefined}
            handleRemoveWordFunction={props.handleRemoveWordFunction}
          />
        </div>
      </div>
    </div>
  );
};

export default Game;
