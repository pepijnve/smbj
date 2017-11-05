package com.hierynomus.rpcgen;

class IDLLetterMatcher extends IDLCharacterMatcher {
    public IDLLetterMatcher() {
        super("Letter");
    }

    @Override
    protected boolean acceptChar(char c) {
        return ('a' <= c && c <= 'z') || ('A' <= c && c <= 'Z') || c == '_';
    }
}
