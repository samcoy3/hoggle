import React from "react";
import Row from "./Row/Row";

type BoardProps = {
  letters?: string[];
};

const Board = (props: BoardProps) => {
  const getRows = () => {
    // If no letters return an empty board
    if (!props.letters) return null;
    // Letters provided as flat array, find size of (square) board
    const size = Math.sqrt(props.letters.length);
    var rows = [];
    for (var i = 0; i < size; i++) {
      // Make rows out of slices of letter array
      const start = i * size;
      const end = i * size + size;
      rows.push(<Row key={i} letters={props.letters.slice(start, end)} />);
    }
    return rows;
  };
  return <div id="board">{getRows()}</div>;
};

export default Board;
