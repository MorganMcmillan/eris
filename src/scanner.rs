pub struct Scanner<'a> {
    /// The input to the scanner.
    input: &'a str,
    /// The current position of the scanner, as the index of a single valid unicode character
    position: usize,
    /// The last saved position, used to backtrack when a token from a sequence fails
    last_position: usize,
}

impl<'a> Scanner<'a> {
    pub fn new(input: &'a str) -> Self {
        Self {
            input,
            position: 0,
            last_position: 0,
        }
    }

    pub fn peek(&self) -> Option<char> {
        let end = self.input.ceil_char_boundary(self.position + 1);
        return self.input.get(self.position..end).and_then(|c| c.chars().next());
    }

    pub fn next(&mut self) -> Option<char> {
        let start = self.position;
        let end = self.input.ceil_char_boundary(start + 1);
        // println!("Start: {start}, End: {end}");
        self.position = end;
        return self.input.get(start..end).and_then(|c| c.chars().next());
    }

    pub fn save_position(&mut self) {
        self.last_position = self.position;
    }

    pub fn lexeme(&self) -> &'a str {
        &self.input[self.last_position..self.position]
    }

    pub fn rest(&self) -> &'a str {
        &self.input[self.position..]
    }
    
    pub fn take_while(&mut self, predicate: &impl Fn(char) -> bool) -> &'a str {
        let start = self.position;
        while self.peek().map(predicate).unwrap_or(false) {
            self.next();
        }
        return &self.input[start..self.position];
    }
}