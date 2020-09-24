import React from "react";
import { ChangeEventFunction } from "../../../types";

type TextInputProps = {
  name: string;
  value: string;
  label?: string;
  info?: string;
  valid?: boolean;
  handleChangeFunction: ChangeEventFunction;
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

export default TextInput;
