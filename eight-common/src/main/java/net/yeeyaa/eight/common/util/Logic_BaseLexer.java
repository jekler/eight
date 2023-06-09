package net.yeeyaa.eight.common.util;

import org.antlr.runtime.*;


public class Logic_BaseLexer extends Lexer {
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



    public LogicLexer gLogic;
    public LogicLexer gParent;

    public Logic_BaseLexer() {;} 
    public Logic_BaseLexer(CharStream input, LogicLexer gLogic) {
        this(input, new RecognizerSharedState(), gLogic);
    }
    public Logic_BaseLexer(CharStream input, RecognizerSharedState state, LogicLexer gLogic) {
        super(input,state);

        this.gLogic = gLogic;
        gParent = gLogic;
    }
    public String getGrammarFileName() { return "BaseLexer.g"; }


    public final void mSPECIAL() throws RecognitionException {
        try {
            int _type = SPECIAL;
            int _channel = DEFAULT_TOKEN_CHANNEL;

            int alt1=3;
            switch ( input.LA(1) ) {
            case 'n':
                {
                alt1=1;
                }
                break;
            case 't':
                {
                alt1=2;
                }
                break;
            case 'f':
                {
                alt1=3;
                }
                break;
            default:
                NoViableAltException nvae =
                    new NoViableAltException("", 1, 0, input);

                throw nvae;
            }

            switch (alt1) {
                case 1 :

                    {
                    match("null"); 


                    }
                    break;
                case 2 :

                    {
                    match("true"); 


                    }
                    break;
                case 3 :

                    {
                    match("false"); 


                    }
                    break;

            }
            state.type = _type;
            state.channel = _channel;
        }
        finally {
        }
    }



    public final void mATOMID() throws RecognitionException {
        try {
            int _type = ATOMID;
            int _channel = DEFAULT_TOKEN_CHANNEL;


            {
            if ( (input.LA(1)>='A' && input.LA(1)<='Z')||input.LA(1)=='_'||(input.LA(1)>='a' && input.LA(1)<='z') ) {
                input.consume();

            }
            else {
                MismatchedSetException mse = new MismatchedSetException(null,input);
                recover(mse);
                throw mse;}


            loop2:
            do {
                int alt2=2;
                int LA2_0 = input.LA(1);

                if ( ((LA2_0>='0' && LA2_0<='9')||(LA2_0>='A' && LA2_0<='Z')||LA2_0=='_'||(LA2_0>='a' && LA2_0<='z')) ) {
                    alt2=1;
                }


                switch (alt2) {
            	case 1 :

            	    {
            	    if ( (input.LA(1)>='0' && input.LA(1)<='9')||(input.LA(1)>='A' && input.LA(1)<='Z')||input.LA(1)=='_'||(input.LA(1)>='a' && input.LA(1)<='z') ) {
            	        input.consume();

            	    }
            	    else {
            	        MismatchedSetException mse = new MismatchedSetException(null,input);
            	        recover(mse);
            	        throw mse;}


            	    }
            	    break;

            	default :
            	    break loop2;
                }
            } while (true);


            }

            state.type = _type;
            state.channel = _channel;
        }
        finally {
        }
    }



    public final void mINT() throws RecognitionException {
        try {
            int _type = INT;
            int _channel = DEFAULT_TOKEN_CHANNEL;


            {

            int cnt3=0;
            loop3:
            do {
                int alt3=2;
                int LA3_0 = input.LA(1);

                if ( ((LA3_0>='0' && LA3_0<='9')) ) {
                    alt3=1;
                }


                switch (alt3) {
            	case 1 :

            	    {
            	    matchRange('0','9'); 

            	    }
            	    break;

            	default :
            	    if ( cnt3 >= 1 ) break loop3;
                        EarlyExitException eee =
                            new EarlyExitException(3, input);
                        throw eee;
                }
                cnt3++;
            } while (true);


            }

            state.type = _type;
            state.channel = _channel;
        }
        finally {
        }
    }



    public final void mDOUBLE() throws RecognitionException {
        try {
            int _type = DOUBLE;
            int _channel = DEFAULT_TOKEN_CHANNEL;

            int alt5=2;
            int LA5_0 = input.LA(1);

            if ( ((LA5_0>='0' && LA5_0<='9')) ) {
                alt5=1;
            }
            else if ( (LA5_0=='.') ) {
                alt5=2;
            }
            else {
                NoViableAltException nvae =
                    new NoViableAltException("", 5, 0, input);

                throw nvae;
            }
            switch (alt5) {
                case 1 :

                    {
                    mINT(); 
                    match('.'); 

                    int alt4=2;
                    int LA4_0 = input.LA(1);

                    if ( ((LA4_0>='0' && LA4_0<='9')) ) {
                        alt4=1;
                    }
                    switch (alt4) {
                        case 1 :

                            {
                            mINT(); 

                            }
                            break;

                    }


                    }
                    break;
                case 2 :

                    {
                    match('.'); 
                    mINT(); 

                    }
                    break;

            }
            state.type = _type;
            state.channel = _channel;
        }
        finally {
        }
    }



    public final void mSTRING() throws RecognitionException {
        try {
            int _type = STRING;
            int _channel = DEFAULT_TOKEN_CHANNEL;


            {
            match('\"'); 

            loop6:
            do {
                int alt6=3;
                int LA6_0 = input.LA(1);

                if ( (LA6_0=='\\') ) {
                    alt6=1;
                }
                else if ( ((LA6_0>='\u0000' && LA6_0<='!')||(LA6_0>='#' && LA6_0<='[')||(LA6_0>=']' && LA6_0<='\uFFFF')) ) {
                    alt6=2;
                }


                switch (alt6) {
            	case 1 :

            	    {
            	    mESC(); 

            	    }
            	    break;
            	case 2 :

            	    {
            	    if ( (input.LA(1)>='\u0000' && input.LA(1)<='!')||(input.LA(1)>='#' && input.LA(1)<='[')||(input.LA(1)>=']' && input.LA(1)<='\uFFFF') ) {
            	        input.consume();

            	    }
            	    else {
            	        MismatchedSetException mse = new MismatchedSetException(null,input);
            	        recover(mse);
            	        throw mse;}


            	    }
            	    break;

            	default :
            	    break loop6;
                }
            } while (true);

            match('\"'); 

            }

            state.type = _type;
            state.channel = _channel;
        }
        finally {
        }
    }



    public final void mWS() throws RecognitionException {
        try {
            int _type = WS;
            int _channel = DEFAULT_TOKEN_CHANNEL;


            {

            int cnt7=0;
            loop7:
            do {
                int alt7=2;
                int LA7_0 = input.LA(1);

                if ( ((LA7_0>='\t' && LA7_0<='\n')||LA7_0=='\r'||LA7_0==' ') ) {
                    alt7=1;
                }


                switch (alt7) {
            	case 1 :

            	    {
            	    if ( (input.LA(1)>='\t' && input.LA(1)<='\n')||input.LA(1)=='\r'||input.LA(1)==' ' ) {
            	        input.consume();

            	    }
            	    else {
            	        MismatchedSetException mse = new MismatchedSetException(null,input);
            	        recover(mse);
            	        throw mse;}


            	    }
            	    break;

            	default :
            	    if ( cnt7 >= 1 ) break loop7;
                        EarlyExitException eee =
                            new EarlyExitException(7, input);
                        throw eee;
                }
                cnt7++;
            } while (true);

            _channel=HIDDEN;

            }

            state.type = _type;
            state.channel = _channel;
        }
        finally {
        }
    }



    public final void mESC() throws RecognitionException {
        try {


            {
            match('\\'); 
            if ( input.LA(1)=='\"'||input.LA(1)=='\''||input.LA(1)=='\\'||input.LA(1)=='b'||input.LA(1)=='f'||input.LA(1)=='n'||input.LA(1)=='r'||input.LA(1)=='t' ) {
                input.consume();

            }
            else {
                MismatchedSetException mse = new MismatchedSetException(null,input);
                recover(mse);
                throw mse;}


            }

        }
        finally {
        }
    }


    public void mTokens() throws RecognitionException {

        int alt8=6;
        alt8 = dfa8.predict(input);
        switch (alt8) {
            case 1 :

                {
                mSPECIAL(); 

                }
                break;
            case 2 :

                {
                mATOMID(); 

                }
                break;
            case 3 :

                {
                mINT(); 

                }
                break;
            case 4 :

                {
                mDOUBLE(); 

                }
                break;
            case 5 :

                {
                mSTRING(); 

                }
                break;
            case 6 :

                {
                mWS(); 

                }
                break;

        }

    }


    protected DFA8 dfa8 = new DFA8(this);
    static final String DFA8_eotS =
        "\1\uffff\3\4\1\uffff\1\14\3\uffff\3\4\1\uffff\3\4\2\23\1\4\1\uffff"+
        "\1\23";
    static final String DFA8_eofS =
        "\25\uffff";
    static final String DFA8_minS =
        "\1\11\1\165\1\162\1\141\1\uffff\1\56\3\uffff\1\154\1\165\1\154"+
        "\1\uffff\1\154\1\145\1\163\2\60\1\145\1\uffff\1\60";
    static final String DFA8_maxS =
        "\1\172\1\165\1\162\1\141\1\uffff\1\71\3\uffff\1\154\1\165\1\154"+
        "\1\uffff\1\154\1\145\1\163\2\172\1\145\1\uffff\1\172";
    static final String DFA8_acceptS =
        "\4\uffff\1\2\1\uffff\1\4\1\5\1\6\3\uffff\1\3\6\uffff\1\1\1\uffff";
    static final String DFA8_specialS =
        "\25\uffff}>";
    static final String[] DFA8_transitionS = {
            "\2\10\2\uffff\1\10\22\uffff\1\10\1\uffff\1\7\13\uffff\1\6\1"+
            "\uffff\12\5\7\uffff\32\4\4\uffff\1\4\1\uffff\5\4\1\3\7\4\1\1"+
            "\5\4\1\2\6\4",
            "\1\11",
            "\1\12",
            "\1\13",
            "",
            "\1\6\1\uffff\12\5",
            "",
            "",
            "",
            "\1\15",
            "\1\16",
            "\1\17",
            "",
            "\1\20",
            "\1\21",
            "\1\22",
            "\12\4\7\uffff\32\4\4\uffff\1\4\1\uffff\32\4",
            "\12\4\7\uffff\32\4\4\uffff\1\4\1\uffff\32\4",
            "\1\24",
            "",
            "\12\4\7\uffff\32\4\4\uffff\1\4\1\uffff\32\4"
    };

    static final short[] DFA8_eot = DFA.unpackEncodedString(DFA8_eotS);
    static final short[] DFA8_eof = DFA.unpackEncodedString(DFA8_eofS);
    static final char[] DFA8_min = DFA.unpackEncodedStringToUnsignedChars(DFA8_minS);
    static final char[] DFA8_max = DFA.unpackEncodedStringToUnsignedChars(DFA8_maxS);
    static final short[] DFA8_accept = DFA.unpackEncodedString(DFA8_acceptS);
    static final short[] DFA8_special = DFA.unpackEncodedString(DFA8_specialS);
    static final short[][] DFA8_transition;

    static {
        int numStates = DFA8_transitionS.length;
        DFA8_transition = new short[numStates][];
        for (int i=0; i<numStates; i++) {
            DFA8_transition[i] = DFA.unpackEncodedString(DFA8_transitionS[i]);
        }
    }

    class DFA8 extends DFA {

        public DFA8(BaseRecognizer recognizer) {
            this.recognizer = recognizer;
            this.decisionNumber = 8;
            this.eot = DFA8_eot;
            this.eof = DFA8_eof;
            this.min = DFA8_min;
            this.max = DFA8_max;
            this.accept = DFA8_accept;
            this.special = DFA8_special;
            this.transition = DFA8_transition;
        }
        public String getDescription() {
            return "1:1: Tokens : ( SPECIAL | ATOMID | INT | DOUBLE | STRING | WS );";
        }
    }
}
