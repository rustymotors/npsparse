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
