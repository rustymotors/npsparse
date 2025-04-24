class ParserError extends Error {
  constructor(message: string) {
    super(message);
    this.name = "ParserError";
  }
}

class ParserSuccess {
  private data: any;
  private remaining: string[];
  constructor(data: any, remaining: string[]) {
    this.data = data;
    this.remaining = remaining;
  }

  toString() {
    return JSON.stringify({
      data: this.data,
      remaining: this.remaining,
    });
  }
}

type ParserResult = ParserError | ParserSuccess;

const clone = <T>(obj: T): T => {
  return JSON.parse(JSON.stringify(obj));
};

const makeAtIndex = (index: number) => {
  return Object.freeze((input: string): string =>{
    if (index < 0 || index >= input.length) {
      throw new Error("Index out of bounds");
    }
    return input[index];
  });
};

const first = makeAtIndex(0);
const second = makeAtIndex(1);


const returnValueIfTrue = (condition: boolean, value: any): any => {
  if (condition) {
    return value;
  }
  throw new Error("Condition is false");
}

const getTwoCharacters = (input: string): string => {
  if (input.length < 2) {
    throw new Error("Input string is too short");
  }
  return clone(input.substring(0, 2));
};

const hexByteToAlpha = (input: string): number => {
  // (input.length) !== 2 ? throw new Error("Input string must be exactly 2 characters long");
  const word = getTwoCharacters(input);
  const highByte = parseInt(first(word), 16);
  const lowByte = parseInt(second(word), 16);
  return (highByte << 4) | lowByte;
};

const combineString = (first: string, second: string): string => {
  return `${first}${second}`;
};

const parseWord = (input: string) => {
  console.log("Parsing input:", input);
  if (input.length % 2 !== 0) {
    return new ParserError("Input length must be even");
  }
  const highByte = input.charAt(0);
  const lowByte = input.charAt(1);
  let remaining = input.substring(2);
  return new ParserSuccess(
    parseInt(`0x${highByte}${lowByte}`, 16),
    Array.from([remaining])
  );
};

const parser = () => {
  const parse = (input: string) => {
    // Parsing logic here
    console.log("Parsing input:", input);
    if (input.length % 2 !== 0) {
      return {
        error: "Input length must be even",
      };
    }

    const parsedData: (ParserResult | ParserError)[] = [];
    for (let i = 0; i < input.length; i += 2) {
      const highByte = input.charAt(i);
      const lowByte = input.charAt(i + 1);
      let remaining = input.substring(i + 1);
      const result = new ParserSuccess(
        parseInt(`0x${highByte}${lowByte}`, 16),
        Array.from([remaining])
      );
      parsedData.push(parseWord(input.substring(i)));
    }
    return parsedData;
  };

  return {
    parse,
  };
};

const r = parser().parse("1101");

console.log("Parsing");

if (r instanceof Array) {
  r.map((i) => {
    console.log(i.toString());
  });
} else {
  console.log(r.error);
}

console.log("Result:", hexByteToAlpha("af"));
