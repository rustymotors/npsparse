import { error } from "node:console";

export function asciiToHexValues(asciiArray: Buffer): Buffer {
  // Split the buffer into sets of 2 bytes
  const hexValues = Buffer.alloc(asciiArray.length / 2);
  let count = 0;

  for (let i = 0; i < asciiArray.length / 2; i += 2) {
    const byte = Buffer.from([asciiArray[i], asciiArray[i + 1]]);
    if (byte.length !== 2) {
      throw new Error(`Invalid byte length: ${byte.length}`);
    }
    const hexValue = `0x${byte.toString("utf8")}`;

    hexValues[count] = parseInt(hexValue, 16);
    count++;
  }

  // Join the hex values into a single buffer
  const hexBuffer = hexValues;

  return hexBuffer;
}

  function isBuffer(value: any): ParseFailure | null {
    return Buffer.isBuffer(value) ? null : new ParseFailure("Not a Buffer", 0, 0, value);
  }

  function isBufferEmpty(value: any): ParseFailure | null {
    return value.length === 0 ? new ParseFailure("Buffer is empty", 0, 0, value) : null;
  }
  function isBufferLengthEven(value: any): ParseFailure | null {
    return value.length % 2 !== 0
      ? new ParseFailure("Buffer length is not even", 0, 0, value)
      : null;
  }

  export function validateBuffer(buffer: Buffer): ParseFailure | null {
    return isBuffer(buffer) ??
      isBufferEmpty(buffer) ??
      isBufferLengthEven(buffer) 
  }

export function parseLittleEndianWord(buffer: Buffer, index: number): ParseTreeNode | ParseFailure {
  if (index < 0 || index >= buffer.length - 1) {
    return new ParseFailure("Index out of bounds", index, 0, buffer.subarray(index));
  }
  const word = buffer.readUInt16LE(index);
  return new ParseTreeNode("word", word);
}
export function parseBigEndianWord(buffer: Buffer, index: number): ParseTreeNode | ParseFailure {
  if (index < 0 || index >= buffer.length - 1) {
    return new ParseFailure("Index out of bounds", index, 0, buffer.subarray(index));
  }
  const word = buffer.readUInt16BE(index);
  return new ParseTreeNode("word", word);
}

export class ParseFailure {
  private _type: string;
  private _message: string;
  private _lineNumber: number;
  private _columnNumber: number;
  private _remainingBuffer: Buffer | null;
  constructor(
    message: string,
    lineNumber: number,
    columnNumber: number,
    remainingBuffer: Buffer
  ) {
    this._type = "ParseFailure";
    this._message = message;
    this._lineNumber = lineNumber;
    this._columnNumber = columnNumber;
    this._remainingBuffer = remainingBuffer;
  }
  get type(): string {
    return this._type;
  }
  get message(): string {
    return this._message;
  }
  get lineNumber(): number {
    return this._lineNumber;
  }
  get columnNumber(): number {
    return this._columnNumber;
  }

  get remainingBuffer(): Buffer | null {
    return this._remainingBuffer;
  }
}

export class ParseTreeNode {
  private _type: string;
  private _value: any;
  private _children: (ParseTreeNode | ParseFailure)[] = [];
  constructor(type: string, value: any) {
    this._type = type;
    this._value = value;
  }
  get type(): string {
    return this._type;
  }
  get value(): any {
    return this._value;
  }
  
  addNode(node: ParseTreeNode | ParseFailure): void {
    this._children.push(node);
  }
  getNode(index: number): ParseTreeNode | ParseFailure | null {
    if (index < 0 || index >= this._children.length) {
      return null;
    }
    return this._children[index];
  }

  hasError(): ParseFailure | null {
    for (const child of this._children) {
      if (child instanceof ParseFailure) {
        return child;
      }
    }
    return null;
  }
}

export class Parser {
  private parseTree = new ParseTreeNode("root", null);
  private currentIndex: number = 0;
  private endian: string = "little";

  constructor() {}

  setEndian(endian: string): void {
    if (endian !== "little" && endian !== "big") {
      throw new Error("Invalid endian type. Use 'little' or 'big'.");
    }
    this.endian = endian;
  }

  parseWord(buffer: Buffer, index: number): ParseTreeNode | ParseFailure {
    if (index < 0 || index >= buffer.length - 1) {
      return new ParseFailure("Index out of bounds", index, 0, buffer.subarray(index));
    }
    return this.endian === "little" ? parseLittleEndianWord(buffer, index) : parseBigEndianWord(buffer, index);
  }
    

  parse(buffer: Buffer): void {
    this.currentIndex = 0;

    const validationError = validateBuffer(buffer);
    if (validationError !== null) {
      console.error("Validation error:", validationError, buffer);
      this.parseTree.addNode(validationError);
      return;
    }

    const byteArray = asciiToHexValues(buffer);
    while (this.currentIndex < byteArray.length) {
      this.parseTree.addNode(this.parseWord(byteArray, this.currentIndex));
      this.currentIndex++;
    }
  }

  getParseTree(): ParseTreeNode | null {
    if (this.parseTree.hasError()) {
      return null
    } else {
      return this.parseTree;
    } 
  }

  getError(): ParseFailure | null {
    return this.parseTree.hasError();
  }
}
