import React, { Component, FormEvent, ChangeEvent } from "react";
import "./App.css";

import { join, newLobby, info, start, sendWord, deleteWord } from "./requests";
import { GameState, LobbyInfo } from "./types";

import Landing from "./Landing";
import Lobby from "./Lobby";
import Game from "./Game";

type AppProps = {};

type AppState = {
  gameState: GameState;
  nickname: string;
  lobbyCode: string;
  valid: { nickname?: boolean; lobbyCode?: boolean; server?: boolean };
  errors: { nickname: string[]; lobbyCode: string[]; server: string[] };
  uuid: string;
  lobby?: LobbyInfo;
  word: string;
  words: Set<String>;
};

class App extends Component<AppProps, AppState> {
  fetchInterval?: NodeJS.Timeout;
  constructor(props: AppProps) {
    super(props);
    this.state = {
      gameState: GameState.InLanding,
      nickname: "",
      lobbyCode: "",
      valid: {},
      errors: { nickname: [], lobbyCode: [], server: [] },
      uuid: "",
      word: "",
      words: new Set<String>(),
    };
    this.handleTextChange = this.handleTextChange.bind(this);
    this.handleFormSubmit = this.handleFormSubmit.bind(this);
    this.createLobby = this.createLobby.bind(this);
    this.startGame = this.startGame.bind(this);
  }

  validateNickname(): boolean {
    const maxLength = 16;
    const regex = new RegExp(/^[a-zA-Z0-9,.?!\-_ ]+$/);

    const nickname = this.state.nickname;
    let valid = this.state.valid;
    let errors = this.state.errors;
    errors.nickname = [];

    if (!nickname) {
      errors.nickname.push("Please enter a nickname");
    } else {
      if (nickname.length > maxLength) {
        errors.nickname.push("Nickname is too long");
      }
      if (!regex.test(nickname)) {
        errors.nickname.push("Nickname uses invalid characters");
      }
    }

    if (errors.nickname.length > 0) {
      valid.nickname = false;
    } else {
      valid.nickname = true;
    }

    this.setState({ valid: valid, errors: errors });
    return valid.nickname;
  }

  validateLobbyCode(): boolean {
    const regex = new RegExp(/^[a-zA-Z]+$/);

    const lobbyCode = this.state.lobbyCode;
    let valid = this.state.valid;
    let errors = this.state.errors;
    errors.lobbyCode = [];

    if (!lobbyCode) {
      errors.lobbyCode.push("Please enter a lobby code");
    } else {
      if (lobbyCode.length !== 4) {
        errors.lobbyCode.push("Lobby code must be 4 characters long");
      }
      if (!regex.test(lobbyCode)) {
        errors.lobbyCode.push("Lobby code must only contain letters");
      }
    }

    if (errors.lobbyCode.length > 0) {
      valid.lobbyCode = false;
    } else {
      valid.lobbyCode = true;
    }

    this.setState({ valid: valid, errors: errors });
    return valid.lobbyCode;
  }

  handleTextChange(event: ChangeEvent<HTMLInputElement>): void {
    const { name, value } = event.target;
    switch (name) {
      case "nickname":
        this.setState({ nickname: value }, this.validateNickname);
        break;
      case "lobbyCode":
        this.setState({ lobbyCode: value.toUpperCase() });
        break;
      case "word":
        this.setState({ word: value });
        break;
      default:
        alert("Unknown text field name: " + name);
        break;
    }
  }

  handleFormSubmit(event: FormEvent<HTMLFormElement>) {
    event.preventDefault();
    const formName = event.currentTarget.name;
    if (formName === "lobby") {
      if (this.validateNickname() && this.validateLobbyCode()) {
        join(this.state.nickname, this.state.lobbyCode).then((UUIDReturn) => {
          this.handleUUIDReturn(UUIDReturn);
        });
      }
    } else if (formName === "word") {
      if (this.state.word) {
        let words = this.state.words;
        const first = this.state.word[0];
        if (first === "." || first === "-" || first === "!") {
          words.delete(this.state.word.slice(1));
          deleteWord(this.state.uuid,this.state.word.slice(1))
        } else {
          words.add(this.state.word);
          sendWord(this.state.uuid,this.state.word)
        }
        this.setState({ words: words, word: "" });
        return;
      }
    }
  }

  createLobby() {
    if (this.validateNickname()) {
      newLobby(this.state.nickname).then((UUIDReturn) => {
        this.handleUUIDReturn(UUIDReturn);
      });
    }
  }

  handleUUIDReturn(response: { success: boolean; data: string }) {
    let valid = this.state.valid;
    let errors = this.state.errors;
    errors.server = [];
    if (response.success) {
      valid.server = true;
      this.setState(
        {
          gameState: GameState.InLobby,
          uuid: response.data,
          errors: errors,
          valid: valid,
        },
        this.startFetchLobbyLoop
      );
    } else {
      errors.server.push(response.data);
      valid.server = false;
      this.setState({ errors: errors, valid: valid });
    }
  }

  startFetchLobbyLoop() {
    this.getLobby();
    this.fetchInterval = setInterval(this.getLobby.bind(this), 1000);
  }

  async getLobby() {
    const lobbyInfo: LobbyInfo = await info(this.state.uuid);
    if (lobbyInfo.state === "StartingGame" || lobbyInfo.state === "InGame") {
      this.setState({ gameState: GameState.InGame, lobby: lobbyInfo });
    } else {
      this.setState({ gameState: GameState.InLobby, lobby: lobbyInfo });
    }
  }

  startGame() {
    start(this.state.uuid);
  }

  render = () => (
    <div className="App">
      <div className="App-header">
        {/* <img src={logo} className="App-logo" alt="logo" /> */}
        {(() => {
          switch (this.state.gameState) {
            case GameState.InLanding:
              return (
                <div>
                  <h1>Hoggle</h1>
                  <h2>An Online Multiplayer Boggle Game</h2>
                </div>
              );
            case GameState.InLobby:
            case GameState.InGame:
              return (
                <section className="lobby-header">
                  <h1>{this.state.lobby?.lobbyCode}</h1>
                  <h1>{this.state.nickname}</h1>
                </section>
              );
            default:
              return null;
          }
        })()}
      </div>
      <div className="App-body">
        {(() => {
          switch (this.state.gameState) {
            case GameState.InLanding:
              return (
                <Landing
                  nickname={this.state.nickname}
                  lobbyCode={this.state.lobbyCode}
                  valid={this.state.valid}
                  errors={this.state.errors}
                  handleChangeFunction={this.handleTextChange}
                  joinLobbyFunction={this.handleFormSubmit}
                  createLobbyFunction={this.createLobby}
                />
              );
            case GameState.InLobby:
              if (!this.state.lobby) {
                return <h1>Loading</h1>;
              } else {
                return (
                  <Lobby
                    lobbyInfo={this.state.lobby}
                    nickname={this.state.nickname}
                    startGameFunction={this.startGame}
                  />
                );
              }
            // For testing layout/css
            // return (
            //   <Lobby
            //     lobbyInfo={{
            //       hostName: "steve",
            //       playerNames: [],
            //     }}
            //     nickname={"steve"}
            //     startGameFunction={this.startGame}
            //   />
            // );
            case GameState.InGame:
              if (!this.state.lobby?.startTime) {
                return <h1>Loading</h1>;
              } else {
                return (
                  <Game
                    lobbyCode={this.state.lobbyCode}
                    board={this.state.lobby.board ? this.state.lobby.board : []}
                    hostName={this.state.lobby.hostName}
                    startTime={this.state.lobby.startTime}
                    nickname={this.state.nickname}
                    word={this.state.word}
                    words={this.state.words}
                    wordChangeFunction={this.handleTextChange}
                    wordSubmitFunction={this.handleFormSubmit}
                  />
                );
              }
            // For testing layout/css
            // return <Game lobbyInfo={{
            //   state: "InGame",
            //   lobbyCode: "ABCD",
            //   board: [
            //     "A",
            //     "B",
            //     "C",
            //     "D",
            //     "E",
            //     "F",
            //     "G",
            //     "H",
            //     "I",
            //     "J",
            //     "K",
            //     "L",
            //     "M",
            //     "N",
            //     "O",
            //     "P",
            //     "QU",
            //     "R",
            //     "S",
            //     "T",
            //     "U",
            //     "V",
            //     "W",
            //     "X",
            //     "Y",
            //   ],
            //   hostName: "steve",
            //   startTime: Date.now() + 10 * 1000,
            // }}
            // nickname={"steve"}/>;
            default:
              return null;
          }
        })()}
      </div>
    </div>
  );
}

export default App;
