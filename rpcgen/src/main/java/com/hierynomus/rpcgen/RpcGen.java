package com.hierynomus.rpcgen;

import org.parboiled.Parboiled;
import org.parboiled.errors.ParseError;
import org.parboiled.parserunners.ParseRunner;
import org.parboiled.parserunners.ReportingParseRunner;
import org.parboiled.parserunners.TracingParseRunner;
import org.parboiled.support.ParsingResult;

import java.io.IOException;
import java.nio.charset.StandardCharsets;
import java.nio.file.Files;
import java.nio.file.Path;
import java.nio.file.Paths;
import java.util.List;

public class RpcGen {
    public RpcGen() {
    }

    public static void main(String[] args) throws IOException {
        String idlFile = readFile(Paths.get("/Users/pepijn/Projects/smbj/rpcgen/src/main/resources/ms-srvs.idl"));

        IDLParser parser = Parboiled.createParser(IDLParser.class);
        ParseRunner runner = new TracingParseRunner(parser.Interface());
        ParsingResult result = runner.run(idlFile);
        if (result.hasErrors()) {
            List<ParseError> parseErrors = result.parseErrors;
            for (ParseError parseError : parseErrors) {
                System.out.println(parseError.getStartIndex() + "-" + parseError.getEndIndex() + ": " + parseError.getErrorMessage());
            }
        }
    }

    private static String readFile(Path f) throws IOException {
        return new String(Files.readAllBytes(f), StandardCharsets.UTF_8);
    }
}
