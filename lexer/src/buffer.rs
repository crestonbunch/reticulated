use std::io::Read;
use std::str;

const BUFFER_LEN: usize = 4096;

enum ScanDirection {
    Forward,
    Backward,
}

pub struct TokenBuffer<R: Read> {
    buf: Box<[u8; 2 * BUFFER_LEN]>,
    inner: R,
    begin: usize,
    forward: usize,
    end: usize,
}

/// A `TokenBuffer` is useful for buffering inputs to the lexer because it
/// it is more efficient for the operating system to read blocks of bytes at a
/// time than it is to read one byte at a time.
///
/// The `TokenBuffer` maintains two buffers: a left buffer and a right buffer.
/// These buffers are contiguous chunks of memory so we can use a pointer to
/// refer to bytes in both buffers.
///
/// A begin pointer references the start of the token in the buffers
/// and a forward pointer references the current character being scanned by
/// the lexer DFA. When the forward pointer goes beyond the left buffer, we
/// will read the next block of input into the right buffer and the forward
/// pointer will continue scanning the right buffer.
///
/// When a token is found, the bytes between the begin pointer and forward
/// pointer are token characters. Once a token is found, the begin pointer
/// is set to the forward pointer and the next token can be analyzed. When
/// the forward pointer scans beyond the right buffer, we copy the right
/// buffer into the left buffer and move the begin and forward pointers into
/// the left buffer so the scan can continue.
///
/// If the begin pointer is already in the left buffer that
/// means the token we are scanning is larger than a buffered block size and
/// we return an error.
///
/// ```
/// use lexer::buffer::TokenBuffer;
///
/// let input = "Hello, World!".as_bytes();
/// let mut buf = TokenBuffer::new(input);
/// assert_eq!(buf.scan_forward().unwrap(), 'H');
/// assert_eq!(buf.scan_forward().unwrap(), 'e');
/// assert_eq!(buf.pop(), "He");
/// ```
impl<R: Read> TokenBuffer<R> {
    pub fn new(mut read: R) -> TokenBuffer<R> {
        let mut buf: [u8; 2 * BUFFER_LEN] = [0; 2 * BUFFER_LEN];
        let end = read.read(&mut buf).unwrap();
        TokenBuffer {
            buf: Box::new(buf),
            inner: read,
            begin: 0,
            forward: 0,
            end,
        }
    }

    fn load_left(&mut self) {
        // if we're reloading the left buffer but our begin pointer is still
        // in the left half, that means we've read the entire right buffer
        // without finding a token
        if self.begin < BUFFER_LEN {
            panic!("Parsing token longer than {} bytes", BUFFER_LEN);
        }
        // copy the right buffer to the left
        self.buf.rotate_right(BUFFER_LEN);
        // move pointers to new positions
        self.begin = self.begin - BUFFER_LEN;
    }

    fn load_right(&mut self) {
        let result = self.inner.read(&mut self.buf[BUFFER_LEN..]);
        self.end = BUFFER_LEN + result.unwrap();
    }

    // Scroll the forward pointer in the given direction and return whether
    // or not we've reached EOF
    fn scroll(&mut self, direction: &ScanDirection) -> bool {
        match direction {
            ScanDirection::Forward => {
                let mut eof = false;
                self.forward = {
                    if self.forward == self.end {
                        // this is the end of the input so we can't move the
                        // forward pointer any more
                        eof = true;
                        self.forward
                    } else if self.forward < 2 * BUFFER_LEN - 1 {
                        self.forward + 1
                    } else {
                        // reached end of right buffer, so copy the right buffer
                        // into the left buffer and start again
                        self.load_left();
                        self.load_right();
                        BUFFER_LEN
                    }
                };
                eof
            }
            ScanDirection::Backward => {
                self.forward = {
                    if self.forward - 1 < self.begin {
                        panic!(
                            "Cannot move backward beyond the start character"
                        );
                    }
                    self.forward - 1
                };
                false
            }
        }
    }

    // Move the forward pointer and return the current buffered token slice
    // or `None` if the input reader has reached EOF and there is nothing
    // left to be read.
    fn scan(&mut self, direction: &ScanDirection) -> Option<String> {
        match self.scroll(direction) {
            true => Option::None,
            false => {
                let mut curr_bytes = &self.buf[self.begin..self.forward];
                let mut result = str::from_utf8(curr_bytes);
                // Scan until the next valid UTF-8 character is added
                while result.is_err() {
                    self.scroll(direction);
                    curr_bytes = &self.buf[self.begin..self.forward];
                    result = str::from_utf8(curr_bytes);
                }
                // Return the UTF-8 encoded character string
                Option::Some(String::from(result.unwrap()))
            }
        }
    }

    /// Scan until the next character in the buffer and return the character
    /// that was found or `None` if we have reached the end of the input.
    pub fn scan_forward(&mut self) -> Option<char> {
        let token = self.scan(&ScanDirection::Forward);
        token.map(|t| t.chars().next_back().unwrap())
    }

    /// Return the current character and move the forward pointer to the
    /// preceeding character. Panics if you try to go back before the begin
    /// pointer.
    pub fn scan_back(&mut self) -> char {
        let curr_bytes = &self.buf[self.begin..self.forward];
        let result = str::from_utf8(curr_bytes).unwrap();
        let c = result.chars().next_back().unwrap();
        self.scan(&ScanDirection::Backward);
        c
    }

    /// Return the current token from the buffer and advance the begin pointer.
    pub fn pop(&mut self) -> String {
        let curr_bytes = &self.buf[self.begin..self.forward];
        let result = str::from_utf8(curr_bytes);
        self.begin = self.forward;
        result.unwrap().to_owned()
    }
}

#[cfg(test)]
mod tests {
    use super::*;
    use std::iter::repeat;

    #[test]
    fn it_reads_one_token() {
        let input = "Hello, World!".as_bytes();
        let mut buf = TokenBuffer::new(input);
        assert_eq!(buf.scan_forward().unwrap(), 'H');
        assert_eq!(buf.pop(), "H");
    }

    #[test]
    fn it_handles_empty_input() {
        let input = "".as_bytes();
        let mut buf = TokenBuffer::new(input);
        assert_eq!(buf.scan_forward(), None);
        assert_eq!(buf.pop(), "");
    }

    #[test]
    fn it_reads_to_eof() {
        let input = "Hello".as_bytes();
        let mut buf = TokenBuffer::new(input);
        assert_eq!(buf.scan_forward().unwrap(), 'H');
        assert_eq!(buf.scan_forward().unwrap(), 'e');
        assert_eq!(buf.scan_forward().unwrap(), 'l');
        assert_eq!(buf.scan_forward().unwrap(), 'l');
        assert_eq!(buf.scan_forward().unwrap(), 'o');
        assert_eq!(buf.scan_forward(), Option::None);
        assert_eq!(buf.scan_forward(), Option::None);
    }

    #[test]
    fn it_scans_backward() {
        let input = "Hello".as_bytes();
        let mut buf = TokenBuffer::new(input);
        assert_eq!(buf.scan_forward().unwrap(), 'H');
        assert_eq!(buf.scan_forward().unwrap(), 'e');
        assert_eq!(buf.scan_forward().unwrap(), 'l');
        assert_eq!(buf.scan_back(), 'l');
        assert_eq!(buf.scan_back(), 'e');
        assert_eq!(buf.scan_back(), 'H');
    }

    #[test]
    #[should_panic]
    fn it_cannot_scan_too_far_backwards() {
        let input = "Hello".as_bytes();
        let mut buf = TokenBuffer::new(input);
        assert_eq!(buf.scan_forward().unwrap(), 'H');
        assert_eq!(buf.scan_forward().unwrap(), 'e');
        assert_eq!(buf.scan_forward().unwrap(), 'l');
        buf.pop();
        assert_eq!(buf.scan_forward().unwrap(), 'l');
        buf.scan_back();
        buf.scan_back();
    }

    #[test]
    fn it_returns_popped_values() {
        let input: String = repeat("Hello, World! ").take(1000).collect();
        let mut buf = TokenBuffer::new(input.as_bytes());
        assert_eq!(buf.scan_forward().unwrap(), 'H');
        assert_eq!(buf.scan_forward().unwrap(), 'e');
        assert_eq!(buf.scan_forward().unwrap(), 'l');
        let first = buf.pop();
        assert_eq!(buf.scan_forward().unwrap(), 'l');
        assert_eq!(buf.scan_forward().unwrap(), 'o');
        let second = buf.pop();
        let third = buf.pop();
        // scan forward to overwrite the buffer
        for _ in 0..8000 {
            buf.scan_forward();
        }
        assert_eq!(first, "Hel");
        assert_eq!(second, "lo");
        assert_eq!(third, "");
    }

    #[test]
    fn it_scans_large_input() {
        let input: String = repeat("Hello, World! ").take(1000).collect();
        let mut buf = TokenBuffer::new(input.as_bytes());
        for _ in 0..4200 {
            buf.scan_forward();
        }
        // Take the scanned token off so we don't go past the buffer limit
        buf.pop();
        for _ in 0..4200 {
            buf.scan_forward();
        }
        assert_eq!(buf.scan_forward().unwrap(), 'H');
        assert_eq!(buf.scan_forward().unwrap(), 'e');
        assert_eq!(buf.scan_forward().unwrap(), 'l');
    }

    #[test]
    #[should_panic]
    fn it_cannot_scan_beyond_buffer() {
        let input: String = repeat("Hello, World! ").take(1000).collect();
        let mut buf = TokenBuffer::new(input.as_bytes());
        for _ in 0..8400 {
            buf.scan_forward();
        }
    }

    #[test]
    fn it_can_handle_multibyte_unicode() {
        let input: String = repeat("❤️").take(1000).collect();
        let mut buf = TokenBuffer::new(input.as_bytes());
        for _ in 0..1998 {
            buf.scan_forward();
        }
        buf.pop();
        buf.scan_forward();
        buf.scan_forward();
        assert_eq!(buf.pop(), "❤️");
        assert_eq!(buf.scan_forward(), None);
    }

    #[test]
    fn it_can_handle_surrogates() {
        let input: String = repeat("𐐷").take(3).collect();
        let mut buf = TokenBuffer::new(input.as_bytes());
        buf.scan_forward();
        buf.scan_forward();
        buf.scan_forward();
        assert_eq!(buf.pop(), "𐐷𐐷𐐷");
        assert_eq!(buf.scan_forward(), None);
    }

    #[test]
    fn it_can_scan_surrogates_backward() {
        let input = "𐐷Z𐐷𐐷AAA";
        let mut buf = TokenBuffer::new(input.as_bytes());
        buf.scan_forward();
        buf.scan_forward();
        buf.scan_forward();
        assert_eq!(buf.scan_forward().unwrap(), '𐐷');
        assert_eq!(buf.scan_forward().unwrap(), 'A');
        assert_eq!(buf.scan_back(), 'A');
        assert_eq!(buf.scan_back(), '𐐷');
        assert_eq!(buf.scan_back(), '𐐷');
        assert_eq!(buf.scan_back(), 'Z');
    }

}
