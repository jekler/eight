package net.yeeyaa.eight.common.util;

import org.antlr.runtime.*;


public class LogicLexer extends Lexer {
    public static final int T__22=22;
    public static final int ESC=8;
    public static final int T__21=21;
    public static final int T__20=20;
    public static final int INT=6;
    public static final int Tokens=23;
    public static final int EOF=-1;
    public static final int T__19=19;
    public static final int WS=10;
    public static final int T__16=16;
    public static final int T__15=15;
    public static final int T__18=18;
    public static final int T__17=17;
    public static final int SPECIAL=4;
    public static final int T__12=12;
    public static final int T__11=11;
    public static final int T__14=14;
    public static final int T__13=13;
    public static final int DOUBLE=7;
    public static final int ATOMID=5;
    public static final int STRING=9;

    // delegates
    public Logic_BaseLexer gBaseLexer;
    // delegators

    public LogicLexer() {;} 
    public LogicLexer(CharStream input) {
        this(input, new RecognizerSharedState());
    }
    public LogicLexer(CharStream input, RecognizerSharedState state) {
        super(input,state);
        gBaseLexer = new Logic_BaseLexer(input, state, this);
    }
    public String getGrammarFileName() { return "Logic__.g"; }

    // $ANTLR start "T__11"
    public final void mT__11() throws RecognitionException {
        try {
            int _type = T__11;
            int _channel = DEFAULT_TOKEN_CHANNEL;
            // Logic__.g:5:7: ( '==' )
            // Logic__.g:5:9: '=='
            {
            match("=="); 


            }

            state.type = _type;
            state.channel = _channel;
        }
        finally {
        }
    }
    // $ANTLR end "T__11"

    // $ANTLR start "T__12"
    public final void mT__12() throws RecognitionException {
        try {
            int _type = T__12;
            int _channel = DEFAULT_TOKEN_CHANNEL;
            // Logic__.g:6:7: ( '!=' )
            // Logic__.g:6:9: '!='
            {
            match("!="); 


            }

            state.type = _type;
            state.channel = _channel;
        }
        finally {
        }
    }
    // $ANTLR end "T__12"

    // $ANTLR start "T__13"
    public final void mT__13() throws RecognitionException {
        try {
            int _type = T__13;
            int _channel = DEFAULT_TOKEN_CHANNEL;
            // Logic__.g:7:7: ( '>=' )
            // Logic__.g:7:9: '>='
            {
            match(">="); 


            }

            state.type = _type;
            state.channel = _channel;
        }
        finally {
        }
    }
    // $ANTLR end "T__13"

    // $ANTLR start "T__14"
    public final void mT__14() throws RecognitionException {
        try {
            int _type = T__14;
            int _channel = DEFAULT_TOKEN_CHANNEL;
            // Logic__.g:8:7: ( '<=' )
            // Logic__.g:8:9: '<='
            {
            match("<="); 


            }

            state.type = _type;
            state.channel = _channel;
        }
        finally {
        }
    }
    // $ANTLR end "T__14"

    // $ANTLR start "T__15"
    public final void mT__15() throws RecognitionException {
        try {
            int _type = T__15;
            int _channel = DEFAULT_TOKEN_CHANNEL;
            // Logic__.g:9:7: ( '>' )
            // Logic__.g:9:9: '>'
            {
            match('>'); 

            }

            state.type = _type;
            state.channel = _channel;
        }
        finally {
        }
    }
    // $ANTLR end "T__15"

    // $ANTLR start "T__16"
    public final void mT__16() throws RecognitionException {
        try {
            int _type = T__16;
            int _channel = DEFAULT_TOKEN_CHANNEL;
            // Logic__.g:10:7: ( '<' )
            // Logic__.g:10:9: '<'
            {
            match('<'); 

            }

            state.type = _type;
            state.channel = _channel;
        }
        finally {
        }
    }
    // $ANTLR end "T__16"

    // $ANTLR start "T__17"
    public final void mT__17() throws RecognitionException {
        try {
            int _type = T__17;
            int _channel = DEFAULT_TOKEN_CHANNEL;
            // Logic__.g:11:7: ( '&' )
            // Logic__.g:11:9: '&'
            {
            match('&'); 

            }

            state.type = _type;
            state.channel = _channel;
        }
        finally {
        }
    }
    // $ANTLR end "T__17"

    // $ANTLR start "T__18"
    public final void mT__18() throws RecognitionException {
        try {
            int _type = T__18;
            int _channel = DEFAULT_TOKEN_CHANNEL;
            // Logic__.g:12:7: ( '|' )
            // Logic__.g:12:9: '|'
            {
            match('|'); 

            }

            state.type = _type;
            state.channel = _channel;
        }
        finally {
        }
    }
    // $ANTLR end "T__18"

    // $ANTLR start "T__19"
    public final void mT__19() throws RecognitionException {
        try {
            int _type = T__19;
            int _channel = DEFAULT_TOKEN_CHANNEL;
            // Logic__.g:13:7: ( '!' )
            // Logic__.g:13:9: '!'
            {
            match('!'); 

            }

            state.type = _type;
            state.channel = _channel;
        }
        finally {
        }
    }
    // $ANTLR end "T__19"

    // $ANTLR start "T__20"
    public final void mT__20() throws RecognitionException {
        try {
            int _type = T__20;
            int _channel = DEFAULT_TOKEN_CHANNEL;
            // Logic__.g:14:7: ( '(' )
            // Logic__.g:14:9: '('
            {
            match('('); 

            }

            state.type = _type;
            state.channel = _channel;
        }
        finally {
        }
    }
    // $ANTLR end "T__20"

    // $ANTLR start "T__21"
    public final void mT__21() throws RecognitionException {
        try {
            int _type = T__21;
            int _channel = DEFAULT_TOKEN_CHANNEL;
            // Logic__.g:15:7: ( ')' )
            // Logic__.g:15:9: ')'
            {
            match(')'); 

            }

            state.type = _type;
            state.channel = _channel;
        }
        finally {
        }
    }
    // $ANTLR end "T__21"

    // $ANTLR start "T__22"
    public final void mT__22() throws RecognitionException {
        try {
            int _type = T__22;
            int _channel = DEFAULT_TOKEN_CHANNEL;
            // Logic__.g:16:7: ( '.' )
            // Logic__.g:16:9: '.'
            {
            match('.'); 

            }

            state.type = _type;
            state.channel = _channel;
        }
        finally {
        }
    }
    // $ANTLR end "T__22"

    public void mTokens() throws RecognitionException {
        // Logic__.g:1:8: ( T__11 | T__12 | T__13 | T__14 | T__15 | T__16 | T__17 | T__18 | T__19 | T__20 | T__21 | T__22 | BaseLexer. Tokens )
        int alt1=13;
        alt1 = dfa1.predict(input);
        switch (alt1) {
            case 1 :
                // Logic__.g:1:10: T__11
                {
                mT__11(); 

                }
                break;
            case 2 :
                // Logic__.g:1:16: T__12
                {
                mT__12(); 

                }
                break;
            case 3 :
                // Logic__.g:1:22: T__13
                {
                mT__13(); 

                }
                break;
            case 4 :
                // Logic__.g:1:28: T__14
                {
                mT__14(); 

                }
                break;
            case 5 :
                // Logic__.g:1:34: T__15
                {
                mT__15(); 

                }
                break;
            case 6 :
                // Logic__.g:1:40: T__16
                {
                mT__16(); 

                }
                break;
            case 7 :
                // Logic__.g:1:46: T__17
                {
                mT__17(); 

                }
                break;
            case 8 :
                // Logic__.g:1:52: T__18
                {
                mT__18(); 

                }
                break;
            case 9 :
                // Logic__.g:1:58: T__19
                {
                mT__19(); 

                }
                break;
            case 10 :
                // Logic__.g:1:64: T__20
                {
                mT__20(); 

                }
                break;
            case 11 :
                // Logic__.g:1:70: T__21
                {
                mT__21(); 

                }
                break;
            case 12 :
                // Logic__.g:1:76: T__22
                {
                mT__22(); 

                }
                break;
            case 13 :
                // Logic__.g:1:82: BaseLexer. Tokens
                {
                gBaseLexer.mTokens(); 

                }
                break;

        }

    }


    protected DFA1 dfa1 = new DFA1(this);
    static final String DFA1_eotS =
        "\2\uffff\1\14\1\16\1\20\4\uffff\1\21\10\uffff";
    static final String DFA1_eofS =
        "\22\uffff";
    static final String DFA1_minS =
        "\1\11\1\uffff\3\75\4\uffff\1\60\10\uffff";
    static final String DFA1_maxS =
        "\1\174\1\uffff\3\75\4\uffff\1\71\10\uffff";
    static final String DFA1_acceptS =
        "\1\uffff\1\1\3\uffff\1\7\1\10\1\12\1\13\1\uffff\1\15\1\2\1\11\1"+
        "\3\1\5\1\4\1\6\1\14";
    static final String DFA1_specialS =
        "\22\uffff}>";
    static final String[] DFA1_transitionS = {
            "\2\12\2\uffff\1\12\22\uffff\1\12\1\2\1\12\3\uffff\1\5\1\uffff"+
            "\1\7\1\10\4\uffff\1\11\1\uffff\12\12\2\uffff\1\4\1\1\1\3\2\uffff"+
            "\32\12\4\uffff\1\12\1\uffff\32\12\1\uffff\1\6",
            "",
            "\1\13",
            "\1\15",
            "\1\17",
            "",
            "",
            "",
            "",
            "\12\12",
            "",
            "",
            "",
            "",
            "",
            "",
            "",
            ""
    };

    static final short[] DFA1_eot = DFA.unpackEncodedString(DFA1_eotS);
    static final short[] DFA1_eof = DFA.unpackEncodedString(DFA1_eofS);
    static final char[] DFA1_min = DFA.unpackEncodedStringToUnsignedChars(DFA1_minS);
    static final char[] DFA1_max = DFA.unpackEncodedStringToUnsignedChars(DFA1_maxS);
    static final short[] DFA1_accept = DFA.unpackEncodedString(DFA1_acceptS);
    static final short[] DFA1_special = DFA.unpackEncodedString(DFA1_specialS);
    static final short[][] DFA1_transition;

    static {
        int numStates = DFA1_transitionS.length;
        DFA1_transition = new short[numStates][];
        for (int i=0; i<numStates; i++) {
            DFA1_transition[i] = DFA.unpackEncodedString(DFA1_transitionS[i]);
        }
    }

    class DFA1 extends DFA {

        public DFA1(BaseRecognizer recognizer) {
            this.recognizer = recognizer;
            this.decisionNumber = 1;
            this.eot = DFA1_eot;
            this.eof = DFA1_eof;
            this.min = DFA1_min;
            this.max = DFA1_max;
            this.accept = DFA1_accept;
            this.special = DFA1_special;
            this.transition = DFA1_transition;
        }
        public String getDescription() {
            return "1:1: Tokens : ( T__11 | T__12 | T__13 | T__14 | T__15 | T__16 | T__17 | T__18 | T__19 | T__20 | T__21 | T__22 | BaseLexer. Tokens );";
        }
    }
}