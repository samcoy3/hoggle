import React from "react";
import Tile from "./Tile/Tile";

type RowProps = {
  letters: string[];
};

const Row = (props: RowProps) => {
  const getLetters = props.letters.map((letter, i) => <Tile letter={letter} key={i} />);
  return <div className="board-row">{getLetters}</div>;
};

export default Row;
