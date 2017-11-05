package com.hierynomus.rpcgen;

class IDLLetterOrDigitMatcher extends IDLCharacterMatcher {
    public IDLLetterOrDigitMatcher() {
        super("LetterOrDigit");
    }

    @Override
    protected boolean acceptChar(char c) {
        return ('0' <= c && c <= '9') || ('a' <= c && c <= 'z') || ('A' <= c && c <= 'Z') || c == '_';
    }
}
