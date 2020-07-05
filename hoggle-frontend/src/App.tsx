import React, { Component, FormEvent, ChangeEvent } from "react";
import "./App.css";

import {
  join,
  newLobby,
  info,
  settings,
  start,
  sendWord,
  removeWord,
  leave,
} from "./requests";
import { LobbyInfo, LastRound } from "./types";

import Landing from "./Landing";
import Lobby from "./Lobby";
import Game from "./Game";

type AppProps = {};

type AppState = {
  nickname: string;
  lobbyCode: string;
  valid: { nickname?: boolean; lobbyCode?: boolean; server?: boolean };
  errors: { nickname: string[]; lobbyCode: string[]; server: string[] };
  uuid: string;
  currentSettings?: {
    size: number;
    timeInSeconds: number;
  };
  newSettings?: {
    size: string;
    timeInSeconds: string;
  };
  hostName?: string;
  lastRound?: LastRound;
  playerNames?: string[];
  startTime?: number;
  endTime?: number;
  board: string[];
  word: string;
  words: Set<string>;
  timerRunning: boolean;
  counter?: number;
  interval?: NodeJS.Timeout;
};

class App extends Component<AppProps, AppState> {
  fetchInterval?: NodeJS.Timeout;
  constructor(props: AppProps) {
    super(props);
    this.state = {
      nickname: "",
      lobbyCode: "",
      valid: {},
      errors: { nickname: [], lobbyCode: [], server: [] },
      uuid: "",
      board: new Array<string>(),
      word: "",
      words: new Set<string>(),
      timerRunning: false,
    };
    this.handleTextChange = this.handleTextChange.bind(this);
    this.handleFormSubmit = this.handleFormSubmit.bind(this);
    this.createLobby = this.createLobby.bind(this);
    this.startGame = this.startGame.bind(this);
  }

  componentDidMount() {
    window.addEventListener("beforeunload", this.beforeunload);
  }

  // componentWillUnmount() {
  //   window.addEventListener("beforeunload", this.unload);
  // }

  startTimer() {
    const interval = setInterval(() => {
      let counter = undefined;
      if (this.state.startTime && this.state.startTime > Date.now()) {
        counter = Math.ceil((this.state.startTime - Date.now()) / 1000);
      } else if (this.state.endTime && this.state.endTime > Date.now()) {
        counter = Math.ceil((this.state.endTime - Date.now()) / 1000);
      }
      this.setState({ counter: counter });
    }, 1000);
    this.setState({ interval: interval, timerRunning: true });
  }

  cancelTimer() {
    if (this.state.interval) {
      clearInterval(this.state.interval);
    }
    this.setState({timerRunning: false})
  }

  beforeunload = (e: Event) => {
    e.preventDefault();
    if (this.fetchInterval) {
      clearInterval(this.fetchInterval);
    }
    if (this.state.uuid) {
      leave(this.state.uuid);
    }
    this.cancelTimer();
    this.setState({ uuid: "" });
  };

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
      case "size":
        if (this.state.newSettings) {
          const newSettings = this.state.newSettings;
          newSettings.size = value;
          this.setState({ newSettings: newSettings });
        }
        break;
      case "time": {
        if (this.state.newSettings) {
          const newSettings = this.state.newSettings;
          newSettings.timeInSeconds = value;
          this.setState({ newSettings: newSettings });
        }
        break;
      }
      default:
        alert("Unknown text field name: " + name);
        break;
    }
  }

  async handleFormSubmit(event: FormEvent<HTMLFormElement>) {
    event.preventDefault();
    const formName = event.currentTarget.name;
    switch (formName) {
      case "lobby":
        if (this.validateNickname() && this.validateLobbyCode()) {
          join(this.state.nickname, this.state.lobbyCode).then((UUIDReturn) => {
            this.handleUUIDReturn(UUIDReturn);
          });
        }
        break;
      case "word":
        if (this.state.word) {
          let words = this.state.words;
          const first = this.state.word[0];
          let success = false;
          if (first === "." || first === "-" || first === "!") {
            words.delete(this.state.word.slice(1));
            success = await removeWord(
              this.state.uuid,
              this.state.word.slice(1)
            );
          } else {
            words.add(this.state.word);
            success = await sendWord(this.state.uuid, this.state.word);
          }

          if (success) {
            this.setState({ words: words, word: "" });
          }
          return;
        }
        break;
      case "settings":
        // TODO: reword app state so new/current settings either both present or both absent
        if (this.state.newSettings) {
          const sizeInt = parseInt(this.state.newSettings.size);
          const timeInt = parseInt(this.state.newSettings.timeInSeconds);
          if (this.state.currentSettings) {
            if (isNaN(sizeInt) && isNaN(timeInt)) {
              this.setState({
                newSettings: {
                  size: this.state.currentSettings.size.toString(),
                  timeInSeconds: this.state.currentSettings.timeInSeconds.toString(),
                },
              });
              alert("Invalid board size and game time");
              return;
            } else if (isNaN(sizeInt)) {
              const newSettings = this.state.newSettings;
              newSettings.size = this.state.currentSettings.size.toString();
              this.setState({ newSettings: newSettings });
              alert("Invalid board size");
              return;
            } else if (isNaN(timeInt)) {
              const newSettings = this.state.newSettings;
              newSettings.timeInSeconds = this.state.currentSettings.timeInSeconds.toString();
              this.setState({ newSettings: newSettings });
              alert("Invalid game time");
              return;
            }
            if (sizeInt > 9) {
              alert("Please select grid size less than 10");
              return;
            }
            const success = await settings(this.state.uuid, sizeInt, timeInt);
            if (success) {
              this.setState({
                newSettings: {
                  size: sizeInt.toString(),
                  timeInSeconds: timeInt.toString(),
                },
              });
              alert("Changes successful!");
            }
          }
        }
        break;
      default:
        console.log("Unknown form " + formName);
        break;
    }
  }

  createLobby() {
    if (this.validateNickname()) {
      newLobby(this.state.nickname).then((UUIDReturn) => {
        this.handleUUIDReturn(UUIDReturn);
      });
    }
  }

  handleUUIDReturn(response: false | string) {
    if (response !== false) {
      this.setState(
        {
          uuid: response,
        },
        this.startFetchLobbyLoop
      );
    }
  }

  startFetchLobbyLoop() {
    this.getLobby();
    this.fetchInterval = setInterval(this.getLobby.bind(this), 1000);
  }

  async getLobby() {
    const lobbyInfo: LobbyInfo = await info(this.state.uuid);
    if (!this.state.lobbyCode) {
      this.setState({ lobbyCode: lobbyInfo.lobbyCode });
    }
    if (!this.state.newSettings) {
      const newSettings = {
        size: lobbyInfo.currentSettings.size.toString(),
        timeInSeconds: lobbyInfo.currentSettings.timeInSeconds.toString(),
      };
      this.setState({
        currentSettings: lobbyInfo.currentSettings,
        newSettings: newSettings,
      });
    }
    if (lobbyInfo.state === "InLobby") {
      if (lobbyInfo.lastRoundScores === null) {
        this.setState(
          {
            currentSettings: lobbyInfo.currentSettings,
            hostName: lobbyInfo.hostName,
            lastRound: undefined,
            lobbyCode: lobbyInfo.lobbyCode,
            playerNames: lobbyInfo.playerNames,
            startTime: undefined,
            endTime: undefined,
          },
          () => this.resetWords(lobbyInfo)
        );
      } else {
        this.setState(
          {
            currentSettings: lobbyInfo.currentSettings,
            hostName: lobbyInfo.hostName,
            lastRound: lobbyInfo.lastRoundScores,
            lobbyCode: lobbyInfo.lobbyCode,
            playerNames: lobbyInfo.playerNames,
            startTime: undefined,
            endTime: undefined,
          },
          () => this.resetWords(lobbyInfo)
        );
      }
    } else if (lobbyInfo.state === "StartingGame") {
      this.setState(
        {
          currentSettings: lobbyInfo.currentSettings,
          hostName: lobbyInfo.hostName,
          lobbyCode: lobbyInfo.lobbyCode,
          startTime: lobbyInfo.startTime,
          endTime: undefined,
          board: [],
        },
        () => this.resetWords(lobbyInfo)
      );
    } else if (lobbyInfo.state === "InGame") {
      this.setState(
        {
          currentSettings: lobbyInfo.currentSettings,
          hostName: lobbyInfo.hostName,
          lobbyCode: lobbyInfo.lobbyCode,
          endTime: lobbyInfo.endTime,
          board: lobbyInfo.board,
        },
        () => this.resetWords(lobbyInfo)
      );
    }
  }

  resetWords(lobbyInfo: LobbyInfo) {
    if (
      (lobbyInfo.state === "StartingGame" || lobbyInfo.state === "InGame") &&
      !this.state.timerRunning
    ) {
      this.startTimer();
    } else if (lobbyInfo.state === "InLobby" && this.state.timerRunning) {
      this.cancelTimer();
    }

    if (
      (this.state.word || this.state.words.size > 0) &&
      (lobbyInfo.state === "InLobby" ||
        lobbyInfo.state === "StartingGame" ||
        (lobbyInfo.state === "InGame" &&
          lobbyInfo.endTime !== this.state.endTime))
    ) {
      if (lobbyInfo.state === "InGame") {
      }
      this.setState({ word: "", words: new Set<string>() });
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
          if (!this.state.uuid) {
            return (
              <div>
                <h1>Hoggle</h1>
                <h2>An Online Multiplayer Boggle Game</h2>
              </div>
            );
          } else {
            return (
              <section className="lobby-header">
                <h1>{this.state.lobbyCode}</h1>
                <h1>{this.state.nickname}</h1>
              </section>
            );
          }
        })()}
      </div>
      <div className="App-body">
        {(() => {
          if (
            this.state.hostName &&
            (this.state.startTime || this.state.endTime)
          ) {
            return (
              <Game
                board={this.state.board}
                hostName={this.state.hostName}
                nickname={this.state.nickname}
                word={this.state.word}
                words={this.state.words}
                wordChangeFunction={this.handleTextChange}
                wordSubmitFunction={this.handleFormSubmit}
                counter={this.state.counter}
              />
            );
          } else if (this.state.hostName && this.state.playerNames) {
            return (
              <Lobby
                nickname={this.state.nickname}
                hostName={this.state.hostName}
                playerNames={this.state.playerNames}
                lastRound={this.state.lastRound}
                newSettings={this.state.newSettings}
                startGameFunction={this.startGame}
                handleChangeFunction={this.handleTextChange}
                handleSubmitFunction={this.handleFormSubmit}
              />
            );
          } else {
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
          }
        })()}
      </div>
    </div>
  );
}

export default App;
