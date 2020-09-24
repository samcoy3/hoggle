import React, { Component, FormEvent, ChangeEvent } from "react";
import {
  join,
  newLobby,
  info,
  settings,
  start,
  leave,
  sendWord,
  removeWord,
  reroll,
} from "../../api-requests";
import { GameData, LastRound, AdminSettings } from "../../types";
import Landing from "../App/Landing/Landing";
import Lobby from "../App/Lobby/Lobby";
import Game from "../App/Game/Game";

type AppProps = {};

type AppState = {
  status: "Landing" | "InLobby" | "StartingGame" | "InGame";

  // Data about user
  nickname: string;
  uuid?: string;

  // Data about landing validation
  nicknameErrors?: string[];
  lobbyCodeErrors?: string[];

  // Data about lobby
  lobbyCode: string;
  playerNames?: string[];
  lastRound?: LastRound;

  // Data about game
  gameData?: GameData;

  // Data about timer/countdown
  changeTime?: number;
  counter?: number;
  counterInterval?: NodeJS.Timeout;

  // Data for admin
  gameSettings?: AdminSettings;
};

class App extends Component<AppProps, AppState> {
  fetchInterval?: NodeJS.Timeout;
  constructor(props: AppProps) {
    super(props);
    // Grab lobby code from URL params if present
    const lobbyCode = this.get_url_param("lobby");
    this.state = {
      status: "Landing",
      nickname: "",
      lobbyCode: lobbyCode != null ? lobbyCode : "",
    };
    this.handleTextChange = this.handleTextChange.bind(this);
    this.handleFormSubmit = this.handleFormSubmit.bind(this);
    this.createLobby = this.createLobby.bind(this);
    this.startGame = this.startGame.bind(this);
    this.rerollGame = this.rerollGame.bind(this);
    this.copyJoinUrl = this.copyJoinUrl.bind(this);
  }

  componentDidMount() {
    window.addEventListener("beforeunload", this.beforeunload);
  }

  startTimer() {
    const interval = setInterval(() => {
      let counter = undefined;
      const endTime = this.state.changeTime;
      if (endTime && endTime > Date.now()) {
        counter = Math.ceil((endTime - Date.now()) / 1000);
      } else {
        counter = 0;
      }
      this.setState({ counter: counter });
    }, 1000);
    this.setState({ counterInterval: interval });
  }

  cancelTimer() {
    if (this.state.counterInterval) {
      clearInterval(this.state.counterInterval);
    }
  }

  beforeunload = (e: Event) => {
    // (Hopefully) runs of user quitting
    // TODO: work out how to make this nicer (while still consistent)
    e.preventDefault();
    if (this.state.uuid) {
      // Send API request for user leaving
      // Allows for reassigning host if needed
      // Also removes user from lobby list
      leave(this.state.uuid);
    }
    if (this.fetchInterval) {
      // Clear the fetch data interval
      clearInterval(this.fetchInterval);
    }
    // Cancel timer
    this.cancelTimer();
    // Remove uuid
    this.setState({ uuid: "" });
  };

  get_url_param(param_name: string): string | null {
    // Get specified parameter from query string
    const queryString = window.location.search;
    const urlParams = new URLSearchParams(queryString);
    return urlParams.get(param_name);
  }

  validateNickname(): boolean {
    // Allow nicknames of up to 16 chars
    const maxLength = 16;
    // Allow alphanumeric chars + some special
    const regex = new RegExp(/^[a-zA-Z0-9,.?!\-_ ]+$/);

    const nickname = this.state.nickname;
    let errors = [];

    // Test nickname is present
    if (!nickname) {
      errors.push("Please enter a nickname");
    } else {
      // Test nickname length
      if (nickname.length > maxLength) {
        errors.push("Nickname is too long");
      }
      // Test nickname characters against regex
      if (!regex.test(nickname)) {
        errors.push("Nickname uses invalid characters");
      }
    }

    // If no errors, nickname is valid
    let valid = undefined;
    if (errors.length > 0) {
      valid = false;
      this.setState({ nicknameErrors: errors });
    } else {
      valid = true;
      this.setState({ nicknameErrors: undefined });
    }

    // Set state and return bool
    return valid;
  }

  validateLobbyCode(): boolean {
    // Lobby codes only use letter
    const regex = new RegExp(/^[a-zA-Z]+$/);

    const lobbyCode = this.state.lobbyCode;
    let errors = [];

    // Test lobby code is present
    if (!lobbyCode) {
      errors.push("Please enter a lobby code");
    } else {
      // Test lobby code is exactly 4 characters
      if (lobbyCode.length !== 4) {
        errors.push("Lobby code must be 4 characters long");
      }
      // Test lobby code against regex
      if (!regex.test(lobbyCode)) {
        errors.push("Lobby code must only contain letters");
      }
    }

    // If no errors, lobby code is valid
    let valid = undefined;
    if (errors.length > 0) {
      valid = false;
      this.setState({ lobbyCodeErrors: errors });
    } else {
      valid = true;
      this.setState({ lobbyCodeErrors: undefined });
    }

    // Return bool
    return valid;
  }

  handleTextChange(event: ChangeEvent<HTMLInputElement>): void {
    // Handle text changes in input boxes
    // Usually this just involves setting the app state to the input
    const { name, value } = event.target;
    switch (name) {
      case "nickname":
        // For nickname validate the input each time it changes
        // This allows instant feedback if name is too long or contains disallowed characters
        this.setState({ nickname: value }, this.validateNickname);
        break;
      case "lobbyCode":
        this.setState({ lobbyCode: value.toUpperCase() });
        break;
      case "word":
        if (this.state.gameData) {
          let gameData = this.state.gameData;
          gameData.word = value;
          this.setState({ gameData: gameData });
        }
        break;
      case "size":
        if (this.state.gameSettings) {
          let settings = this.state.gameSettings;
          settings.newSettings.size = value;
          this.setState({ gameSettings: settings });
        }
        break;
      case "time": {
        if (this.state.gameSettings) {
          let settings = this.state.gameSettings;
          settings.newSettings.timeInSeconds = value;
          this.setState({ gameSettings: settings });
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
      case "lobbyCode":
        if (this.validateNickname() && this.validateLobbyCode()) {
          join(this.state.nickname, this.state.lobbyCode).then((UUIDReturn) => {
            this.handleUUIDReturn(UUIDReturn);
          });
        }
        break;
      case "word":
        // Can't input word without uuid
        if (!this.state.uuid) return;
        // Can't input word if not in game
        if (!this.state.gameData) return;

        let gameData = this.state.gameData;
        // Get first character of input word
        const first = gameData.word[0];
        let success = false;
        if ([".", "-", "!"].includes(first)) {
          // If first char of word is '.' '-' or '!' remove word from list

          // Get word without removal indicator
          const word = gameData.word.slice(1);
          // Send remove word API request
          removeWord(this.state.uuid, word);

          // Reset input box
          gameData.word = "";
          // Setting state now helps for slow connections
          // Waiting until success causes input to clear at incovenient times
          this.setState({ gameData: gameData });
        } else {
          const word = gameData.word;
          // Send add word API request
          sendWord(this.state.uuid, word);

          // Reset input box
          gameData.word = "";
          // Setting state now helps for slow connections
          // Waiting until success causes input to clear at incovenient times
          this.setState({ gameData: gameData });
        }
        break;
      case "settings":
        if (this.state.gameSettings) {
          // Can't change settings without uuid
          if (!this.state.uuid) return;

          let gameSettings = this.state.gameSettings;
          const sizeInt = parseInt(gameSettings.newSettings.size);
          const timeInt = parseInt(gameSettings.newSettings.timeInSeconds);

          if (isNaN(sizeInt) && isNaN(timeInt)) {
            // If invalid, reset values shown in input boxes to actual setting values
            gameSettings.newSettings.size = gameSettings.currentSettings.size.toString();
            gameSettings.newSettings.timeInSeconds = gameSettings.currentSettings.timeInSeconds.toString();
            this.setState({ gameSettings: gameSettings });
            alert("Invalid board size and game time");
            return;
          }

          if (isNaN(sizeInt)) {
            // If size is not a number
            // Reset value shown in size input box to actual size value
            gameSettings.newSettings.size = gameSettings.currentSettings.size.toString();
            this.setState({ gameSettings: gameSettings });
            alert("Invalid board size");
            return;
          }

          if (isNaN(timeInt)) {
            // If time is not a number
            // Reset value shown in time input box to actual time value
            gameSettings.newSettings.timeInSeconds = gameSettings.currentSettings.timeInSeconds.toString();
            this.setState({ gameSettings: gameSettings });
            alert("Invalid game time");
            return;
          }

          if (sizeInt > 9) {
            // Grid size is constrained (to <10) by backend
            gameSettings.newSettings.size = gameSettings.currentSettings.size.toString();
            this.setState({ gameSettings: gameSettings });
            alert("Please select grid size less than 10");
            return;
          }

          // Send API request with uuid and new settings
          const success = await settings(this.state.uuid, sizeInt, timeInt);
          if (success) {
            // If request succeeds, update state and alert user
            gameSettings.newSettings.size = sizeInt.toString();
            gameSettings.newSettings.timeInSeconds = timeInt.toString();
            this.setState({ gameSettings: gameSettings });
            alert("Changes successful!");
          }
        }
        break;
      default:
        console.log("Unknown form " + formName);
        break;
    }
  }

  createLobby() {
    // If nickname validates, send new lobby API request and handle response
    if (this.validateNickname()) {
      // Clear anything from lobby code
      this.setState({ lobbyCode: ""})
      newLobby(this.state.nickname).then((UUIDReturn) => {
        this.handleUUIDReturn(UUIDReturn);
      });
    }
  }

  handleUUIDReturn(response: false | string) {
    // Handle UUID | false response from server
    if (response !== false) {
      // If request succeeds, set uuid and begin fetch loop
      this.setState({ uuid: response }, this.startFetchLobbyLoop);
    }
  }

  startFetchLobbyLoop() {
    // Get lobby info
    this.getLobby();
    // Set interval to fetch new info every second
    this.fetchInterval = setInterval(this.getLobby.bind(this), 1000);
  }

  async getLobby() {
    // Can't be in lobby without uuid
    if (!this.state.uuid) return;

    const response = await info(this.state.uuid);

    if (response) {
      if (!this.state.lobbyCode) {
        // Store lobby code if not yet set
        this.setState({ lobbyCode: response.lobbyCode });
      }

      if (this.state.nickname === response.hostName) {
        // Only store settings is player is host
        if (!this.state.gameSettings) {
          // If state doesn't yet have game settings
          // Use response data for both current and new
          const currentSettings = response.currentSettings;
          const newSettings = {
            size: response.currentSettings.size.toString(),
            timeInSeconds: response.currentSettings.timeInSeconds.toString(),
          };
          this.setState({
            gameSettings: {
              currentSettings: currentSettings,
              newSettings: newSettings,
            },
          });
        } else {
          // If state has existing game settings
          // Only update current settings with response data
          let gameSettings = this.state.gameSettings;
          gameSettings.currentSettings = response.currentSettings;
          this.setState({ gameSettings: gameSettings });
        }
      }

      if (response.state === "InLobby") {
        // Game lobby is waiting in lobby
        if (this.state.status !== "InLobby") {
          // Not previously in lobby
          // Remove old time/counter/gameData
          this.setState({
            status: "InLobby",
            lastRound: response.lastRoundScores,
            playerNames: response.playerNames,
            changeTime: undefined,
            counter: undefined,
            gameData: undefined,
          });
          // Stop the timer running
          this.cancelTimer();
        } else {
          // Previously in lobby
          // Old data should have been removed previously
          this.setState({
            lastRound: response.lastRoundScores,
            playerNames: response.playerNames,
          });
        }
      } else if (response.state === "StartingGame") {
        // Game lobby is starting new game

        if (
          this.state.status !== "StartingGame" ||
          this.state.changeTime !== response.changeTime
        ) {
          // Not previously starting this game
          // Need to set time and unset old data
          this.setState(
            {
              status: "StartingGame",
              changeTime: response.changeTime,
              playerNames: undefined,
              lastRound: undefined,
              gameData: undefined,
            },
            // Start timer
            () => this.startTimer()
          );
        }
      } else if (response.state === "InGame") {
        // Game lobby is in game
        if (
          !(
            this.state.status === "InGame" &&
            this.state.changeTime === response.changeTime &&
            JSON.stringify(this.state.gameData?.board) ===
              JSON.stringify(response.board)
          )
        ) {
          // Not previously in this game
          // Need to set time/gameData and unset old data
          this.setState(
            {
              status: "InGame",
              gameData: {
                board: response.board,
                word: "",
                words: new Set<string>(),
              },
              changeTime: response.changeTime,
              playerNames: undefined,
              lastRound: undefined,
            },
            () => this.startTimer()
          );
        } else if (this.state.gameData) {
          // Update words
          const words = new Set(response.words);
          this.setState({
            gameData: {
              board: this.state.gameData.board,
              word: this.state.gameData.word,
              words: words,
            },
          });
        }
      }
    }
  }

  startGame() {
    // Can't start game without uuid
    if (this.state.uuid) start(this.state.uuid);
  }

  rerollGame() {
    // Can't reroll game without uuid
    if (this.state.uuid) reroll(this.state.uuid);
  }

  copyJoinUrl(event: React.MouseEvent<HTMLHeadingElement, MouseEvent>): void {
    let joinUrl =
      window.location.origin + "/?lobby=" + event.currentTarget.innerText;
    navigator.clipboard.writeText(joinUrl);
  }

  render = () => (
    <div id="App">
      <div id="App-header">
        {/* <img src={logo} className="App-logo" alt="logo" /> */}
        {(() => {
          if (!this.state.uuid) {
            return (
              <div id="landing-header">
                <h1>Hoggle</h1>
                <h2>An Online Multiplayer Boggle Game</h2>
              </div>
            );
          } else {
            return (
              <div id="lobby-game-header">
                <h1 onClick={this.copyJoinUrl}>{this.state.lobbyCode}</h1>
                <h1>{this.state.nickname}</h1>
              </div>
            );
          }
        })()}
      </div>
      <div id="App-body">
        {(() => {
          if (this.state.changeTime) {
            // If change time is set if in game or starting game
            return (
              <Game
                gameData={this.state.gameData}
                counter={this.state.counter}
                newSettings={this.state.gameSettings?.newSettings}
                rerollGameFunction={this.rerollGame}
                handleChangeFunction={this.handleTextChange}
                handleSubmitFunction={this.handleFormSubmit}
              />
            );
          } else if (this.state.playerNames) {
            // If playernames is set we're in lobby
            return (
              <Lobby
                playerNames={this.state.playerNames}
                lastRound={this.state.lastRound}
                newSettings={this.state.gameSettings?.newSettings}
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
                nicknameErrors={this.state.nicknameErrors}
                lobbyCodeErrors={this.state.lobbyCodeErrors}
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
