package net.yeeyaa.eight.common.util;

import org.antlr.runtime.*;


public class LogicParser extends Parser {
    public static final String[] tokenNames = new String[] {
        "<invalid>", "<EOR>", "<DOWN>", "<UP>", "SPECIAL", "ATOMID", "INT", "DOUBLE", "ESC", "STRING", "WS", "'=='", "'!='", "'>='", "'<='", "'>'", "'<'", "'&'", "'|'", "'!'", "'('", "')'", "'.'"
    };
    public static final int ESC=8;
    public static final int T__22=22;
    public static final int T__21=21;
    public static final int T__20=20;
    public static final int INT=6;
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

    public LogicParser(TokenStream input, ICalculator calc) {
        this(input, new RecognizerSharedState(), calc);
    }
    public LogicParser(TokenStream input, RecognizerSharedState state, ICalculator calc) {
        super(input, state);
        this.calc = calc; 
    }

    private ICalculator calc;




    public final boolean stat() throws RecognitionException {
        boolean value = false;

        boolean complexpr1 = false;


        try {


            {

            loop1:
            do {
                int alt1=2;
                int LA1_0 = input.LA(1);

                if ( (LA1_0==WS) ) {
                    alt1=1;
                }


                switch (alt1) {
            	case 1 :

            	    {
            	    match(input,WS,FOLLOW_WS_in_stat47); 

            	    }
            	    break;

            	default :
            	    break loop1;
                }
            } while (true);

            pushFollow(FOLLOW_complexpr_in_stat51);
            complexpr1=complexpr();

            state._fsp--;


            loop2:
            do {
                int alt2=2;
                int LA2_0 = input.LA(1);

                if ( (LA2_0==WS) ) {
                    alt2=1;
                }


                switch (alt2) {
            	case 1 :

            	    {
            	    match(input,WS,FOLLOW_WS_in_stat54); 

            	    }
            	    break;

            	default :
            	    break loop2;
                }
            } while (true);

            match(input,EOF,FOLLOW_EOF_in_stat58); 
            value = complexpr1;

            }

        }
        catch (RecognitionException re) {
            reportError(re);
            recover(input,re);
        }
        finally {
        }
        return value;
    }





    public final boolean atomexpr() throws RecognitionException {
        boolean value = false;

        Token op=null;
        LogicParser.content_return e1 = null;

        LogicParser.content_return e2 = null;


        try {


            {
            pushFollow(FOLLOW_content_in_atomexpr98);
            e1=content();

            state._fsp--;


            loop3:
            do {
                int alt3=2;
                int LA3_0 = input.LA(1);

                if ( (LA3_0==WS) ) {
                    alt3=1;
                }


                switch (alt3) {
            	case 1 :

            	    {
            	    match(input,WS,FOLLOW_WS_in_atomexpr101); 

            	    }
            	    break;

            	default :
            	    break loop3;
                }
            } while (true);

            op=(Token)input.LT(1);
            if ( (input.LA(1)>=11 && input.LA(1)<=16) ) {
                input.consume();
                state.errorRecovery=false;
            }
            else {
                MismatchedSetException mse = new MismatchedSetException(null,input);
                throw mse;
            }


            loop4:
            do {
                int alt4=2;
                int LA4_0 = input.LA(1);

                if ( (LA4_0==WS) ) {
                    alt4=1;
                }


                switch (alt4) {
            	case 1 :

            	    {
            	    match(input,WS,FOLLOW_WS_in_atomexpr121); 

            	    }
            	    break;

            	default :
            	    break loop4;
                }
            } while (true);

            pushFollow(FOLLOW_content_in_atomexpr127);
            e2=content();

            state._fsp--;

            value = calc.compare((e1!=null?input.toString(e1.start,e1.stop):null), (op!=null?op.getText():null), (e2!=null?input.toString(e2.start,e2.stop):null));

            }

        }
        catch (RecognitionException re) {
            reportError(re);
            recover(input,re);
        }
        finally {
        }
        return value;
    }





    public final boolean complexpr() throws RecognitionException {
        boolean value = false;

        boolean e = false;

        boolean atomexpr2 = false;

        boolean notblank3 = false;

        boolean blank4 = false;


        try {


            {

            int alt5=3;
            switch ( input.LA(1) ) {
            case SPECIAL:
            case ATOMID:
            case INT:
            case DOUBLE:
            case STRING:
                {
                alt5=1;
                }
                break;
            case 19:
                {
                alt5=2;
                }
                break;
            case 20:
                {
                alt5=3;
                }
                break;
            default:
                NoViableAltException nvae =
                    new NoViableAltException("", 5, 0, input);

                throw nvae;
            }

            switch (alt5) {
                case 1 :

                    {
                    pushFollow(FOLLOW_atomexpr_in_complexpr162);
                    atomexpr2=atomexpr();

                    state._fsp--;

                    value = atomexpr2;

                    }
                    break;
                case 2 :

                    {
                    pushFollow(FOLLOW_notblank_in_complexpr167);
                    notblank3=notblank();

                    state._fsp--;

                    value = notblank3;

                    }
                    break;
                case 3 :

                    {
                    pushFollow(FOLLOW_blank_in_complexpr172);
                    blank4=blank();

                    state._fsp--;

                    value = blank4;

                    }
                    break;

            }


            int alt10=3;
            alt10 = dfa10.predict(input);
            switch (alt10) {
                case 1 :

                    {

                    loop6:
                    do {
                        int alt6=2;
                        int LA6_0 = input.LA(1);

                        if ( (LA6_0==WS) ) {
                            alt6=1;
                        }


                        switch (alt6) {
                    	case 1 :

                    	    {
                    	    match(input,WS,FOLLOW_WS_in_complexpr185); 

                    	    }
                    	    break;

                    	default :
                    	    break loop6;
                        }
                    } while (true);

                    match(input,17,FOLLOW_17_in_complexpr188); 

                    loop7:
                    do {
                        int alt7=2;
                        int LA7_0 = input.LA(1);

                        if ( (LA7_0==WS) ) {
                            alt7=1;
                        }


                        switch (alt7) {
                    	case 1 :

                    	    {
                    	    match(input,WS,FOLLOW_WS_in_complexpr190); 

                    	    }
                    	    break;

                    	default :
                    	    break loop7;
                        }
                    } while (true);

                    pushFollow(FOLLOW_complexpr_in_complexpr196);
                    e=complexpr();

                    state._fsp--;

                    value = value && e;

                    }
                    break;
                case 2 :

                    {

                    loop8:
                    do {
                        int alt8=2;
                        int LA8_0 = input.LA(1);

                        if ( (LA8_0==WS) ) {
                            alt8=1;
                        }


                        switch (alt8) {
                    	case 1 :

                    	    {
                    	    match(input,WS,FOLLOW_WS_in_complexpr207); 

                    	    }
                    	    break;

                    	default :
                    	    break loop8;
                        }
                    } while (true);

                    match(input,18,FOLLOW_18_in_complexpr210); 

                    loop9:
                    do {
                        int alt9=2;
                        int LA9_0 = input.LA(1);

                        if ( (LA9_0==WS) ) {
                            alt9=1;
                        }


                        switch (alt9) {
                    	case 1 :

                    	    {
                    	    match(input,WS,FOLLOW_WS_in_complexpr212); 

                    	    }
                    	    break;

                    	default :
                    	    break loop9;
                        }
                    } while (true);

                    pushFollow(FOLLOW_complexpr_in_complexpr218);
                    e=complexpr();

                    state._fsp--;

                    value = value || e;

                    }
                    break;

            }


            }

        }
        catch (RecognitionException re) {
            reportError(re);
            recover(input,re);
        }
        finally {
        }
        return value;
    }





    public final boolean notblank() throws RecognitionException {
        boolean value = false;

        boolean blank5 = false;


        try {


            {
            match(input,19,FOLLOW_19_in_notblank252); 

            loop11:
            do {
                int alt11=2;
                int LA11_0 = input.LA(1);

                if ( (LA11_0==WS) ) {
                    alt11=1;
                }


                switch (alt11) {
            	case 1 :

            	    {
            	    match(input,WS,FOLLOW_WS_in_notblank254); 

            	    }
            	    break;

            	default :
            	    break loop11;
                }
            } while (true);

            pushFollow(FOLLOW_blank_in_notblank258);
            blank5=blank();

            state._fsp--;

            value = !blank5;

            }

        }
        catch (RecognitionException re) {
            reportError(re);
            recover(input,re);
        }
        finally {
        }
        return value;
    }





    public final boolean blank() throws RecognitionException {
        boolean value = false;

        boolean complexpr6 = false;


        try {


            {
            match(input,20,FOLLOW_20_in_blank291); 

            loop12:
            do {
                int alt12=2;
                int LA12_0 = input.LA(1);

                if ( (LA12_0==WS) ) {
                    alt12=1;
                }


                switch (alt12) {
            	case 1 :

            	    {
            	    match(input,WS,FOLLOW_WS_in_blank293); 

            	    }
            	    break;

            	default :
            	    break loop12;
                }
            } while (true);

            pushFollow(FOLLOW_complexpr_in_blank297);
            complexpr6=complexpr();

            state._fsp--;

            value = complexpr6;

            loop13:
            do {
                int alt13=2;
                int LA13_0 = input.LA(1);

                if ( (LA13_0==WS) ) {
                    alt13=1;
                }


                switch (alt13) {
            	case 1 :

            	    {
            	    match(input,WS,FOLLOW_WS_in_blank301); 

            	    }
            	    break;

            	default :
            	    break loop13;
                }
            } while (true);

            match(input,21,FOLLOW_21_in_blank304); 

            }

        }
        catch (RecognitionException re) {
            reportError(re);
            recover(input,re);
        }
        finally {
        }
        return value;
    }


    public static class content_return extends ParserRuleReturnScope {
    };



    public final LogicParser.content_return content() throws RecognitionException {
        LogicParser.content_return retval = new LogicParser.content_return();
        retval.start = input.LT(1);

        try {

            int alt14=5;
            switch ( input.LA(1) ) {
            case INT:
                {
                alt14=1;
                }
                break;
            case DOUBLE:
                {
                alt14=2;
                }
                break;
            case STRING:
                {
                alt14=3;
                }
                break;
            case SPECIAL:
                {
                alt14=4;
                }
                break;
            case ATOMID:
                {
                alt14=5;
                }
                break;
            default:
                NoViableAltException nvae =
                    new NoViableAltException("", 14, 0, input);

                throw nvae;
            }

            switch (alt14) {
                case 1 :

                    {
                    match(input,INT,FOLLOW_INT_in_content314); 

                    }
                    break;
                case 2 :

                    {
                    match(input,DOUBLE,FOLLOW_DOUBLE_in_content318); 

                    }
                    break;
                case 3 :

                    {
                    match(input,STRING,FOLLOW_STRING_in_content322); 

                    }
                    break;
                case 4 :

                    {
                    match(input,SPECIAL,FOLLOW_SPECIAL_in_content326); 

                    }
                    break;
                case 5 :

                    {
                    pushFollow(FOLLOW_id_in_content330);
                    id();

                    state._fsp--;


                    }
                    break;

            }
            retval.stop = input.LT(-1);

        }
        catch (RecognitionException re) {
            reportError(re);
            recover(input,re);
        }
        finally {
        }
        return retval;
    }





    public final void id() throws RecognitionException {
        try {


            {
            match(input,ATOMID,FOLLOW_ATOMID_in_id338); 

            loop15:
            do {
                int alt15=2;
                int LA15_0 = input.LA(1);

                if ( (LA15_0==22) ) {
                    alt15=1;
                }


                switch (alt15) {
            	case 1 :

            	    {
            	    match(input,22,FOLLOW_22_in_id341); 
            	    match(input,ATOMID,FOLLOW_ATOMID_in_id343); 

            	    }
            	    break;

            	default :
            	    break loop15;
                }
            } while (true);


            }

        }
        catch (RecognitionException re) {
            reportError(re);
            recover(input,re);
        }
        finally {
        }
        return ;
    }





    protected DFA10 dfa10 = new DFA10(this);
    static final String DFA10_eotS =
        "\5\uffff";
    static final String DFA10_eofS =
        "\2\4\3\uffff";
    static final String DFA10_minS =
        "\2\12\3\uffff";
    static final String DFA10_maxS =
        "\2\25\3\uffff";
    static final String DFA10_acceptS =
        "\2\uffff\1\1\1\2\1\3";
    static final String DFA10_specialS =
        "\5\uffff}>";
    static final String[] DFA10_transitionS = {
            "\1\1\6\uffff\1\2\1\3\2\uffff\1\4",
            "\1\1\6\uffff\1\2\1\3\2\uffff\1\4",
            "",
            "",
            ""
    };

    static final short[] DFA10_eot = DFA.unpackEncodedString(DFA10_eotS);
    static final short[] DFA10_eof = DFA.unpackEncodedString(DFA10_eofS);
    static final char[] DFA10_min = DFA.unpackEncodedStringToUnsignedChars(DFA10_minS);
    static final char[] DFA10_max = DFA.unpackEncodedStringToUnsignedChars(DFA10_maxS);
    static final short[] DFA10_accept = DFA.unpackEncodedString(DFA10_acceptS);
    static final short[] DFA10_special = DFA.unpackEncodedString(DFA10_specialS);
    static final short[][] DFA10_transition;

    static {
        int numStates = DFA10_transitionS.length;
        DFA10_transition = new short[numStates][];
        for (int i=0; i<numStates; i++) {
            DFA10_transition[i] = DFA.unpackEncodedString(DFA10_transitionS[i]);
        }
    }

    class DFA10 extends DFA {

        public DFA10(BaseRecognizer recognizer) {
            this.recognizer = recognizer;
            this.decisionNumber = 10;
            this.eot = DFA10_eot;
            this.eof = DFA10_eof;
            this.min = DFA10_min;
            this.max = DFA10_max;
            this.accept = DFA10_accept;
            this.special = DFA10_special;
            this.transition = DFA10_transition;
        }
        public String getDescription() {
            return "21:114: ( ( WS )* '&' ( WS )* e= complexpr | ( WS )* '|' ( WS )* e= complexpr )?";
        }
    }
 

    public static final BitSet FOLLOW_WS_in_stat47 = new BitSet(new long[]{0x00000000001806F0L});
    public static final BitSet FOLLOW_complexpr_in_stat51 = new BitSet(new long[]{0x0000000000000400L});
    public static final BitSet FOLLOW_WS_in_stat54 = new BitSet(new long[]{0x0000000000000400L});
    public static final BitSet FOLLOW_EOF_in_stat58 = new BitSet(new long[]{0x0000000000000002L});
    public static final BitSet FOLLOW_content_in_atomexpr98 = new BitSet(new long[]{0x000000000001FC00L});
    public static final BitSet FOLLOW_WS_in_atomexpr101 = new BitSet(new long[]{0x000000000001FC00L});
    public static final BitSet FOLLOW_set_in_atomexpr107 = new BitSet(new long[]{0x00000000000006F0L});
    public static final BitSet FOLLOW_WS_in_atomexpr121 = new BitSet(new long[]{0x00000000000006F0L});
    public static final BitSet FOLLOW_content_in_atomexpr127 = new BitSet(new long[]{0x0000000000000002L});
    public static final BitSet FOLLOW_atomexpr_in_complexpr162 = new BitSet(new long[]{0x0000000000060402L});
    public static final BitSet FOLLOW_notblank_in_complexpr167 = new BitSet(new long[]{0x0000000000060402L});
    public static final BitSet FOLLOW_blank_in_complexpr172 = new BitSet(new long[]{0x0000000000060402L});
    public static final BitSet FOLLOW_WS_in_complexpr185 = new BitSet(new long[]{0x0000000000020400L});
    public static final BitSet FOLLOW_17_in_complexpr188 = new BitSet(new long[]{0x00000000001806F0L});
    public static final BitSet FOLLOW_WS_in_complexpr190 = new BitSet(new long[]{0x00000000001806F0L});
    public static final BitSet FOLLOW_complexpr_in_complexpr196 = new BitSet(new long[]{0x0000000000000002L});
    public static final BitSet FOLLOW_WS_in_complexpr207 = new BitSet(new long[]{0x0000000000040400L});
    public static final BitSet FOLLOW_18_in_complexpr210 = new BitSet(new long[]{0x00000000001806F0L});
    public static final BitSet FOLLOW_WS_in_complexpr212 = new BitSet(new long[]{0x00000000001806F0L});
    public static final BitSet FOLLOW_complexpr_in_complexpr218 = new BitSet(new long[]{0x0000000000000002L});
    public static final BitSet FOLLOW_19_in_notblank252 = new BitSet(new long[]{0x00000000001806F0L});
    public static final BitSet FOLLOW_WS_in_notblank254 = new BitSet(new long[]{0x00000000001806F0L});
    public static final BitSet FOLLOW_blank_in_notblank258 = new BitSet(new long[]{0x0000000000000002L});
    public static final BitSet FOLLOW_20_in_blank291 = new BitSet(new long[]{0x00000000001806F0L});
    public static final BitSet FOLLOW_WS_in_blank293 = new BitSet(new long[]{0x00000000001806F0L});
    public static final BitSet FOLLOW_complexpr_in_blank297 = new BitSet(new long[]{0x0000000000200400L});
    public static final BitSet FOLLOW_WS_in_blank301 = new BitSet(new long[]{0x0000000000200400L});
    public static final BitSet FOLLOW_21_in_blank304 = new BitSet(new long[]{0x0000000000000002L});
    public static final BitSet FOLLOW_INT_in_content314 = new BitSet(new long[]{0x0000000000000002L});
    public static final BitSet FOLLOW_DOUBLE_in_content318 = new BitSet(new long[]{0x0000000000000002L});
    public static final BitSet FOLLOW_STRING_in_content322 = new BitSet(new long[]{0x0000000000000002L});
    public static final BitSet FOLLOW_SPECIAL_in_content326 = new BitSet(new long[]{0x0000000000000002L});
    public static final BitSet FOLLOW_id_in_content330 = new BitSet(new long[]{0x0000000000000002L});
    public static final BitSet FOLLOW_ATOMID_in_id338 = new BitSet(new long[]{0x0000000000400002L});
    public static final BitSet FOLLOW_22_in_id341 = new BitSet(new long[]{0x0000000000000020L});
    public static final BitSet FOLLOW_ATOMID_in_id343 = new BitSet(new long[]{0x0000000000400002L});
}
