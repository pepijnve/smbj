package com.hierynomus.rpcgen;

import org.parboiled.BaseParser;
import org.parboiled.Rule;
import org.parboiled.annotations.*;

@BuildParseTree
class IDLParser extends BaseParser<Object> {

    final Rule AT = Terminal("@");
    final Rule AND = Terminal("&", AnyOf("=&"));
    final Rule ANDAND = Terminal("&&");
    final Rule ANDEQU = Terminal("&=");
    final Rule BANG = Terminal("!", Ch('='));
    final Rule BSR = Terminal(">>>", Ch('='));
    final Rule BSREQU = Terminal(">>>=");
    final Rule COLON = Terminal(":");
    final Rule COMMA = Terminal(",");
    final Rule DEC = Terminal("--");
    final Rule DIV = Terminal("/", Ch('='));
    final Rule DIVEQU = Terminal("/=");
    final Rule DOT = Terminal(".");
    final Rule ELLIPSIS = Terminal("...");
    final Rule EQU = Terminal("=", Ch('='));
    final Rule EQUAL = Terminal("==");
    final Rule GE = Terminal(">=");
    final Rule GT = Terminal(">", AnyOf("=>"));
    final Rule HAT = Terminal("^", Ch('='));
    final Rule HATEQU = Terminal("^=");
    final Rule INC = Terminal("++");
    final Rule LBRACKET = Terminal("[");
    final Rule LE = Terminal("<=");
    final Rule LPAREN = Terminal("(");
    final Rule LPOINT = Terminal("<");
    final Rule LT = Terminal("<", AnyOf("=<"));
    final Rule LBRACE = Terminal("{");
    final Rule MINUS = Terminal("-", AnyOf("=-"));
    final Rule MINUSEQU = Terminal("-=");
    final Rule MOD = Terminal("%", Ch('='));
    final Rule MODEQU = Terminal("%=");
    final Rule NOTEQUAL = Terminal("!=");
    final Rule OR = Terminal("|", AnyOf("=|"));
    final Rule OREQU = Terminal("|=");
    final Rule OROR = Terminal("||");
    final Rule PLUS = Terminal("+", AnyOf("=+"));
    final Rule PLUSEQU = Terminal("+=");
    final Rule QUERY = Terminal("?");
    final Rule RBRACKET = Terminal("]");
    final Rule RPAREN = Terminal(")");
    final Rule RPOINT = Terminal(">");
    final Rule RBRACE = Terminal("}");
    final Rule SEMICOLON = Terminal(";");
    final Rule SL = Terminal("<<", Ch('='));
    final Rule SLEQU = Terminal("<<=");
    final Rule SR = Terminal(">>", AnyOf("=>"));
    final Rule SREQU = Terminal(">>=");
    final Rule STAR = Terminal("*", Ch('='));
    final Rule STAREQU = Terminal("*=");
    final Rule TILDA = Terminal("~");

    //(1) <interface> ::= <interface_header> { <interface_body> }
    public Rule Interface() {
        return Sequence(InterfaceHeader(), LBRACE, InterfaceBody(), RBRACE);
    }

    //(2) <interface_header> ::= <[>  <interface_attributes> <]> interface <Identifier>
    Rule InterfaceHeader() {
        return Sequence(Optional(Sequence(LBRACKET, InterfaceAttributes(), RBRACKET)), Terminal("interface"), Identifier());
    }

    //(3) <interface_attributes> ::= <interface_attribute> [ , <interface_attribute> ] ...
    Rule InterfaceAttributes() {
        return Sequence(InterfaceAttribute(), ZeroOrMore(COMMA, InterfaceAttribute()));
    }

    //(4) <interface_attribute> ::= uuid ( <Uuid_rep> )
    //	| version ( <Integer_literal>[.<Integer_literal>])
    //	| endpoint ( <port_spec> [ , <port_spec> ] ... )
    //	| exceptions ( <excep_name> [ , <excep_name> ] ... )
    //	| local
    //	| pointer_default ( <ptr_attr> )
    Rule InterfaceAttribute() {
        return FirstOf(
            Sequence(Terminal("uuid"), LPAREN, Uuid(), RPAREN),
            Sequence(Terminal("version"), LPAREN, IntegerLiteral(), Optional('.', IntegerLiteral()), RPAREN),
            Sequence(Terminal("endpoint"), LPAREN, PortSpec(), ZeroOrMore(COMMA, PortSpec()), RPAREN),
            Sequence(Terminal("exceptions"), LPAREN, ExcepName(), ZeroOrMore(COMMA, ExcepName()), RPAREN),
            Terminal("local"),
            Terminal("ms_union"),
            Sequence(Terminal("pointer_default"), LPAREN, PtrAttr(), RPAREN)
        );
    }

    //(5) <port_spec> ::=  <Family_string> :  <[> <Port_string> <]>
    Rule PortSpec() {
        return Sequence(FamilyString(), COLON, LBRACKET, PortString(), RBRACKET);
    }

    Rule FamilyString() {
        return String();
    }

    Rule PortString() {
        return String();
    }

    //(5.01) <excep_name> ::= <Identifier>
    Rule ExcepName() {
        return Identifier();
    }

    //(6) <interface_body> ::= [ <import> ... ] <interface_component>
    //	[ <interface_component> ... ]
    Rule InterfaceBody() {
        return Sequence(
            Optional(Import()),
            OneOrMore(InterfaceComponent())
        );
    }

    //(7) <import> ::= import <import_list> ;
    Rule Import() {
        return Sequence(Terminal("import"), ImportList(), SEMICOLON);
    }

    //(8) <interface_component> ::= <export> | <op_declarator> ;
    Rule InterfaceComponent() {
        return FirstOf(
            Export(),
            Sequence(OpDeclarator(), SEMICOLON)
        );
    }

    //(9) <export> ::= <type_declarator> ;
    //	| <const_declarator> ;
    //	| <tagged_declarator> ;
    Rule Export() {
        return FirstOf(
            Sequence(TypeDeclarator(), SEMICOLON),
            Sequence(ConstDeclarator(), SEMICOLON),
            Sequence(TaggedDeclarator(), SEMICOLON)
        );
    }

    //(10) <import_list> ::= <import_name> [ , <import_name> ] ...
    Rule ImportList() {
        return Sequence(
            ImportName(),
            ZeroOrMore(
                Sequence(COMMA, ImportName())
            )
        );
    }

    //(11) <import_name> ::=  "<Import_string>"
    Rule ImportName() {
        return String();
    }

    //(12) <const_declarator> ::= const <const_type_spec> <Identifier> = <const_exp>
    Rule ConstDeclarator() {
        return Sequence(
            Terminal("const"),
            ConstTypeSpec(),
            Identifier(),
            Terminal("="),
            ConstExpression()
        );
    }

    //(13) <const_type_spec> ::= <primitive_integer_type>
    //	| char
    //	| boolean
    //	| void *
    //  | char *
    Rule ConstTypeSpec() {
        return FirstOf(
            PrimitiveIntegerType(),
            Terminal("char"),
            Terminal("boolean"),
            Sequence(Terminal("void"), STAR),
            Sequence(Terminal("char"), STAR)
        );
    }

    //(14) <const_exp> ::= <integer_const_exp>
    //	| <Identifier>
    //	| <string>
    //	| <character_constant>
    //	| NULL
    //	| TRUE
    //	| FALSE
    Rule ConstExpression() {
        return FirstOf(
            IntegerConstExpression(),
            Identifier(),
            String(),
            CharacterConstant(),
            Terminal("NULL"),
            Terminal("TRUE"),
            Terminal("FALSE")
        );
    }

    // (14.01) <integer_const_exp> ::= <conditional_exp>
    Rule IntegerConstExpression() {
        return ConditionalExpression();
    }

    //(14.02) <conditional_exp> ::= <logical_or_exp>
    //	| <logical_or_exp> ? <integer_const_exp> : <conditional_exp>
    Rule ConditionalExpression() {
        return Sequence(
            ConditionalOrExpression(),
            ZeroOrMore(QUERY, IntegerConstExpression(), COLON, ConditionalOrExpression())
        );
    }

    //(14.03) <logical_or_exp> ::= <logical_and_exp>
    //	| <logical_or_exp> <||> <logical_and_exp>
    Rule ConditionalOrExpression() {
        return Sequence(
            ConditionalAndExpression(),
            ZeroOrMore(OROR, ConditionalAndExpression())
        );
    }

    //(14.04) <logical_and_exp> ::= <inclusive_or_exp>
    //	| <logical_and_exp> && <inclusive_or_exp>
    Rule ConditionalAndExpression() {
        return Sequence(
            InclusiveOrExpression(),
            ZeroOrMore(ANDAND, InclusiveOrExpression())
        );
    }

    //(14.05) <inclusive_or_exp> ::= <exclusive_or_exp>
    //	| <inclusive_or_exp> <|> <exclusive_or_exp>
    Rule InclusiveOrExpression() {
        return Sequence(
            ExclusiveOrExpression(),
            ZeroOrMore(OR, ExclusiveOrExpression())
        );
    }

    //(14.06) <exclusive_or_exp> ::= <and_exp>
    //	| <exclusive_or_exp> ^ <and_exp>
    Rule ExclusiveOrExpression() {
        return Sequence(
            AndExpression(),
            ZeroOrMore(HAT, AndExpression())
        );
    }

    //(14.07) <and_exp> ::= <equality_exp>
    //	| <and_exp> & <equality_exp>
    Rule AndExpression() {
        return Sequence(
            EqualityExpression(),
            ZeroOrMore(AND, EqualityExpression())
        );
    }

    //(14.08) <equality_exp> ::= <relational_exp>
    //	| <equality_exp> == <relational_exp>
    //	| <equality_exp> != <relational_exp>
    Rule EqualityExpression() {
        return Sequence(
            RelationalExpression(),
            ZeroOrMore(FirstOf(EQUAL, NOTEQUAL), RelationalExpression())
        );
    }

    //(14.09) <relational_exp> ::= <shift_exp>
    //	| <relational_exp> <<> <shift_exp>
    //	| <relational_exp> <>> <shift_exp>
    //	| <relational_exp> <<=> <shift_exp>
    //	| <relational_exp> <>=> <shift_exp>
    Rule RelationalExpression() {
        return Sequence(
            ShiftExpression(),
            ZeroOrMore(
                Sequence(FirstOf(LE, GE, LT, GT), ShiftExpression())
            )
        );
    }

    //(14.10) <shift_exp> ::= <additive_exp>
    //	| <shift_exp> <<<> <additive_exp>
    //	| <shift_exp> <>>> <additive_exp>
    Rule ShiftExpression() {
        return Sequence(
            AdditiveExpression(),
            ZeroOrMore(FirstOf(SL, SR, BSR), AdditiveExpression())
        );
    }

    //(14.11) <additive_exp> ::= <multiplicative_exp>
    //	| <additive_exp> + <multiplicative_exp>
    //	| <additive_exp> - <multiplicative_exp>
    Rule AdditiveExpression() {
        return Sequence(
            MultiplicativeExpression(),
            ZeroOrMore(FirstOf(PLUS, MINUS), MultiplicativeExpression())
        );
    }

    //(14.12) <multiplicative_exp> ::= <unary_exp>
    //	| <multiplicative_exp> * <unary_exp>
    //	| <multiplicative_exp> / <unary_exp>
    //	| <multiplicative_exp> % <unary_exp>
    Rule MultiplicativeExpression() {
        return Sequence(
            UnaryExpression(),
            ZeroOrMore(FirstOf(STAR, DIV, MOD), UnaryExpression())
        );
    }

    //(14.13) <unary_exp> ::= <primary_exp>
    //	| + <primary_exp>
    //	| - <primary_exp>
    //	| ~ <primary_exp>
    //	| ! <primary_exp>
    Rule UnaryExpression() {
        return Sequence(Optional(PrefixOp()), PrimaryExpression());
    }

    Rule PrefixOp() {
        return FirstOf(BANG, TILDA, PLUS, MINUS);
    }

    //(14.14) <primary_exp> ::= <Integer_literal>
    //	| <Identifier>
    //	| '(' <const_exp> ')'
    Rule PrimaryExpression() {
        return FirstOf(
            IntegerLiteral(),
            Identifier(),
            Sequence(LPAREN, ConstExpression(), RPAREN)
        );
    }

    //(15) <string> ::= "[<Character>] ... "
    Rule String() {
        return Sequence(
            '"',
            ZeroOrMore(
                FirstOf(
                    Escape(),
                    Sequence(TestNot(AnyOf("\r\n\"\\")), ANY)
                )
            ).suppressSubnodes(),
            '"',
            Spacing()
        );
    }

    //(16) <character_constant> ::= '<Character>'
    Rule CharacterConstant() {
        return Sequence(
            '\'',
            FirstOf(Escape(), Sequence(TestNot(AnyOf("'\\")), ANY)).suppressSubnodes(),
            '\''
        );
    }

    Rule Escape() {
        return Sequence('\\', FirstOf(AnyOf("btnfr\"\'\\"), OctalEscape(), UnicodeEscape()));
    }

    Rule OctalEscape() {
        return FirstOf(
            Sequence(CharRange('0', '3'), CharRange('0', '7'), CharRange('0', '7')),
            Sequence(CharRange('0', '7'), CharRange('0', '7')),
            CharRange('0', '7')
        );
    }

    Rule UnicodeEscape() {
        return Sequence(OneOrMore('u'), HexDigit(), HexDigit(), HexDigit(), HexDigit());
    }

    //(17) <type_declarator> ::= typedef [ <type_attribute_list> ] <type_spec>
    //	<declarators>
    Rule TypeDeclarator() {
        return Sequence(
            Terminal("typedef"),
            Optional(TypeAttributeList()),
            TypeSpec(),
            Declarators()
        );
    }

    //(18) <type_attribute_list> ::= <[> <type_attribute>
    //	[ , <type_attribute> ] ... <]>
    Rule TypeAttributeList() {
        return Sequence(
            LBRACKET,
            TypeAttribute(),
            ZeroOrMore(Sequence(COMMA, TypeAttribute())),
            RBRACKET
        );
    }

    //(19) <type_spec> ::= <simple_type_spec>
    //	| <constructed_type_spec>
    Rule TypeSpec() {
        return FirstOf(
            SimpleTypeSpec(),
            ConstructedTypeSpec()
        );
    }

    //(20) <simple_type_spec> ::= <base_type_spec>
    //	| <predefined_type_spec>
    //	| <Identifier>
    Rule SimpleTypeSpec() {
        return FirstOf(
            BaseTypeSpec(),
            PredefinedTypeSpec(),
            Identifier()
        );
    }

    //(21) <declarators> ::= <declarator> [ , <declarator> ] ...
    Rule Declarators() {
        return Sequence(
            Declarator(),
            ZeroOrMore(Sequence(COMMA, Declarator()))
        );
    }

    //(23) <declarator> ::= <pointer_opt> <direct_declarator>
    Rule Declarator() {
        return Sequence(PointerOpt(), DirectDeclarator());
    }

    ;

    //(24) <direct_declarator> ::= <Identifier>
    //	| ( <declarator> )
    //  | <array_declarator>
    //	| <function_declarator>
    Rule DirectDeclarator() {
        return Sequence(
            FirstOf(
                Identifier(),
                Sequence(LPAREN, Declarator(), RPAREN)
            ),
            ZeroOrMore(
                FirstOf(
                    ArrayBoundsDeclarator(),
                    ParamDeclarators()
                )
            )
        );
    }

    //(26) <tagged_declarator> ::= <tagged_struct_declarator>
    //	| <tagged_union_declarator>
    Rule TaggedDeclarator() {
        return FirstOf(
            TaggedStructDeclarator(),
            TaggedUnionDeclarator()
        );
    }

    //(27) <base_type_spec> ::= <floating_pt_type>
    //	| <integer_type>
    //	| <char_type>
    //	| <boolean_type>
    //	| <byte_type>
    //	| <void_type>
    //	| <handle_type>
    Rule BaseTypeSpec() {
        return FirstOf(
            FloatingPointType(),
            IntegerType(),
            CharType(),
            BooleanType(),
            ByteType(),
            VoidType(),
            HandleType()
        );
    }

    //(28) <floating_pt_type> ::= float
    //	| double
    Rule FloatingPointType() {
        return FirstOf(Terminal("float"), Terminal("double"));
    }

    //(29) <integer_type> ::= <primitive_integer_type>
    //	| hyper [unsigned] [int]
    //  | unsigned hyper [int]
    Rule IntegerType() {
        return FirstOf(
            PrimitiveIntegerType(),
            Sequence(Terminal("hyper"), Optional(Terminal("unsigned")), Optional(Terminal("int"))),
            Sequence(Terminal("unsigned"), Terminal("hyper"), Optional(Terminal("int")))
        );
    }

    //(29.1) <primitive_integer_type> ::= <signed_integer>
    //	| <unsigned_integer>
    Rule PrimitiveIntegerType() {
        return FirstOf(
            SignedInteger(),
            UnsignedInteger()
        );
    }

    //(30) <signed_integer> ::= <integer_size> [ int ]
    Rule SignedInteger() {
        return FirstOf(
            Sequence(IntegerSize(), Optional(Terminal("int"))),
            Terminal("int"),
            Terminal("__int8"),
            Terminal("__int16"),
            Terminal("__int32"),
            Terminal("__int64"),
            Terminal("__int3264")
        );
    }

    //(31) <unsigned_integer> ::= <integer_size> unsigned [ int ]
    //  | unsigned <integer_size> [ int ]
    Rule UnsignedInteger() {
        return FirstOf(
            Sequence(IntegerSize(), Terminal("unsigned"), Optional(Terminal("int"))),
            Sequence(
                Terminal("unsigned"),
                FirstOf(
                    Sequence(IntegerSize(), Optional(Terminal("int"))),
                    Terminal("int"),
                    Terminal("__int8"),
                    Terminal("__int16"),
                    Terminal("__int32"),
                    Terminal("__int64"),
                    Terminal("__int3264")
                )
            )
        );
    }

    //(32) <integer_size> ::= long
    //	| short
    //	| small
    Rule IntegerSize() {
        return FirstOf(
            Terminal("long"),
            Terminal("short"),
            Terminal("small")
        );
    }

    //(33) <char_type> ::= [ unsigned ] char
    Rule CharType() {
        return FirstOf(
            Sequence(Optional(Terminal("unsigned")), Terminal("char")),
            Terminal("wchar_t")
        );
    }

    //(34) <boolean_type> ::= boolean
    Rule BooleanType() {
        return Terminal("boolean");
    }

    //(35) <byte_type> ::= byte
    Rule ByteType() {
        return Terminal("byte");
    }

    //(36) <void_type> ::= void
    Rule VoidType() {
        return Terminal("void");
    }

    //(37) <handle_type> ::= handle_t
    Rule HandleType() {
        return Terminal("handle_t");
    }

    //(38) <constructed_type_spec> ::= <struct_type>
    //	| <union_type>
    //	| <enumeration_type>
    //	| <tagged_declarator>
    //	| <pipe_type>
    Rule ConstructedTypeSpec() {
        return FirstOf(
            StructType(),
            UnionType(),
            EnumerationType(),
            TaggedDeclarator(),
            PipeType()
        );
    }

    //(39) <tagged_struct_declarator> ::= struct <tag>
    //	| <tagged_struct>
    Rule TaggedStructDeclarator() {
        return FirstOf(
            TaggedStruct(),
            Sequence(Terminal("struct"), Tag())
        );
    }

    //(40) <struct_type> ::= struct  { <member_list> }
    Rule StructType() {
        return Sequence(
            Terminal("struct"),
            LBRACE,
            MemberList(),
            RBRACE
        );
    }

    //(41) <tagged_struct> ::= struct <tag> { <member_list> }
    Rule TaggedStruct() {
        return Sequence(
            Terminal("struct"),
            Tag(),
            LBRACE,
            MemberList(),
            RBRACE
        );
    }

    //(42) <tag> ::= <Identifier>
    Rule Tag() {
        return Identifier();
    }

    //(43) <member_list> ::= <member> [ <member> ] ...
    Rule MemberList() {
        return OneOrMore(Member());
    }

    //(44) <member> ::= <field_declarator> ;
    Rule Member() {
        return Sequence(FieldDeclarator(), SEMICOLON);
    }

    //(45) <field_declarator> ::= [ <field_attribute_list> ] <type_spec>
    //	<declarators>
    Rule FieldDeclarator() {
        return Sequence(
            Optional(FieldAttributeList()),
            TypeSpec(),
            Declarators()
        );
    }

    //(46) <field_attribute_list> ::= <[> <field_attribute>
    //	[ , <field_attribute> ] ... <]>
    Rule FieldAttributeList() {
        return Sequence(
            LBRACKET,
            FieldAttribute(),
            ZeroOrMore(Sequence(COMMA, FieldAttribute())),
            RBRACKET
        );
    }

    //(47) <tagged_union_declarator> ::= union <tag>
    //	| <tagged_union>
    Rule TaggedUnionDeclarator() {
        return FirstOf(
            TaggedUnion(),
            Sequence(Terminal("union"), Tag())
        );
    }

    //(48) <union_type> ::= union <union_switch> { <union_body> }
    //	| union { <union_body_n_e> }
    Rule UnionType() {
        return Sequence(
            Terminal("union"),
            FirstOf(
                Sequence(UnionSwitch(), LBRACE, UnionBody(), RBRACE),
                Sequence(LBRACE, UnionBodyNE(), RBRACE)
            )
        );
    }

    //(48.1) <union_switch> ::= switch ( <switch_type_spec> <Identifier> )
    //        [ <union_name> ]
    //        (49) <switch_type_spec> ::= <primitive_integer_type>
    //	| <char_type>
    //	| <boolean_type>
    //	| <Identifier>
    Rule UnionSwitch() {
        return Sequence(
            Terminal("switch"),
            LPAREN,
            RPAREN
        );
    }

    //(49) <switch_type_spec> ::= <primitive_integer_type>
    //  | <char_type>
    //  | <boolean_type>
    //  | <Identifier>
    Rule SwitchTypeSpec() {
        return FirstOf(
            PrimitiveIntegerType(),
            CharType(),
            BooleanType(),
            Identifier()
        );
    }

    //(50) <tagged_union> ::= union <tag> <union_switch> { <union_body> }
    //	| union <tag> { <union_body_n_e> }
    Rule TaggedUnion() {
        return Sequence(
            Terminal("union"),
            Tag(),
            FirstOf(
                Sequence(UnionSwitch(), LBRACE, UnionBody(), RBRACE),
                Sequence(LBRACE, UnionBodyNE(), RBRACE)
            )
        );
    }

    //(51) <union_name> ::= <Identifier>
    Rule UnionName() {
        return Identifier();
    }

    //(52) <union_body> ::= <union_case> [  <union_case> ] ...
    Rule UnionBody() {
        return OneOrMore(UnionCase());
    }

    //(52.1) <union_body_n_e> ::=  <union_case_n_e> [ <union_case_n_e> ] ...
    Rule UnionBodyNE() {
        return OneOrMore(UnionCaseNE());
    }

    //(53) <union_case> ::= <union_case_label> [ <union_case_label> ] ...
    //	<union_arm>
    //	| <default_case>
    Rule UnionCase() {
        return FirstOf(
            Sequence(OneOrMore(UnionCaseLabel()), UnionArm()),
            DefaultCase()
        );
    }

    //(53.1) <union_case_n_e> ::= <union_case_label_n_e> <union_arm>
    //	| <default_case_n_e>
    Rule UnionCaseNE() {
        return FirstOf(
            Sequence(OneOrMore(UnionCaseLabelNE()), UnionArm()),
            DefaultCaseNE()
        );
    }

    //(54) <union_case_label> ::= case <const_exp> :
    Rule UnionCaseLabel() {
        return Sequence(
            Terminal("case"),
            ConstExpression(),
            COLON
        );
    }

    //(54.1) <union_case_label_n_e> ::= <[> case ( <const_exp>
    //	[ , <const_exp> ] ... ) <]>
    Rule UnionCaseLabelNE() {
        return Sequence(
            LBRACKET,
            Terminal("case"),
            LPAREN,
            ConstExpression(),
            ZeroOrMore(Sequence(COMMA, ConstExpression())),
            RPAREN,
            RBRACKET
        );
    }

    //(55) <default_case> ::= default : <union_arm>
    Rule DefaultCase() {
        return Sequence(
            Terminal("default"),
            COLON,
            UnionArm()
        );
    }

    //(55.1) <default_case_n_e> ::= <[> default <]> <union_arm>
    Rule DefaultCaseNE() {
        return Sequence(
            LBRACKET,
            Terminal("default"),
            RBRACKET,
            UnionArm()
        );
    }

    //(55.2) <union_arm> ::= [ <field_declarator> ] ;
    Rule UnionArm() {
        return Sequence(
            Optional(FieldDeclarator()),
            SEMICOLON
        );
    }

    //(55.3) <union_type_switch_attr> ::= switch_type ( <switch_type_spec> )
    Rule UnionTypeSwitchAttr() {
        return Sequence(
            Terminal("switch_type"),
            LPAREN,
            SwitchTypeSpec(),
            RPAREN
        );
    }

    //(55.4) <union_instance_switch_attr> ::= switch_is ( <attr_var> )
    Rule UnionInstanceSwitchAttr() {
        return Sequence(
            Terminal("switch_is"),
            LPAREN,
            AttrVar(),
            RPAREN
        );
    }

    //(57) <enumeration_type> ::= enum { <Identifier> [ , <Identifier> ] ... }
    Rule EnumerationType() {
        return Sequence(
            Terminal("enum"),
            LBRACE,
            Identifier(),
            ZeroOrMore(Sequence(COMMA, Identifier())),
            TypeSpec(),
            RBRACE
        );
    }

    //(58) <pipe_type> ::= pipe <type_spec>
    Rule PipeType() {
        return Sequence(
            Terminal("pipe"),
            TypeSpec()
        );
    }

    //(61) <array_bounds_declarator> ::= <[> [ <array_bound> ] <]>
    //  | <[> <array_bounds_pair> <]>
    Rule ArrayBoundsDeclarator() {
        return Sequence(
            LBRACKET,
            FirstOf(
                ArrayBoundsPair(),
                Optional(ArrayBound())
            ),
            RBRACKET
        );
    }

    //(62) <array_bounds_pair> ::= <array_bound> .. <array_bound>
    Rule ArrayBoundsPair() {
        return Sequence(
            ArrayBound(),
            Terminal(".."),
            ArrayBound()
        );
    }

    //(63) <array_bound> ::= *
    //  | <integer_const_exp>
    //	| <Identifier>
    Rule ArrayBound() {
        return FirstOf(
            STAR,
            IntegerConstExpression(),
            Identifier()
        );
    }

    //(64) <type_attribute> ::= transmit_as ( <xmit_type> )
    //	| handle
    //	| <usage_attribute>
    //	| <union_type_switch_attr>
    //	| <ptr_attr>
    Rule TypeAttribute() {
        return FirstOf(
            Sequence(Terminal("transmit_as"), LPAREN, XmitType(), RPAREN),
            Terminal("handle"),
            UsageAttribute(),
            UnionTypeSwitchAttr(),
            PtrAttr()
        );
    }

    //(65) <usage_attribute> ::= string
    //	| context_handle
    Rule UsageAttribute() {
        return FirstOf(
            Terminal("string"),
            Terminal("context_handle")
        );
    }

    //(66) <xmit_type> ::= <simple_type_spec>
    Rule XmitType() {
        return SimpleTypeSpec();
    }

    //(67) <field_attribute> ::= first_is ( <attr_var_list> )
    //	| last_is ( <attr_var_list> )
    //	| length_is ( <attr_var_list> )
    //	| min_is ( <attr_var_list> )
    //	| max_is ( <attr_var_list> )
    //	| size_is ( <attr_var_list> )
    //	| <usage_attribute>
    //	| <union_instance_switch_attr>
    //	| ignore
    //	| <ptr_attr>
    Rule FieldAttribute() {
        return FirstOf(
            Sequence(Terminal("byte_count"), LPAREN, Identifier(), RPAREN),
            Sequence(Terminal("first_is"), LPAREN, AttrVarList(), RPAREN),
            Sequence(Terminal("last_is"), LPAREN, AttrVarList(), RPAREN),
            Sequence(Terminal("length_is"), LPAREN, AttrVarList(), RPAREN),
            Sequence(Terminal("min_is"), LPAREN, AttrVarList(), RPAREN),
            Sequence(Terminal("max_is"), LPAREN, AttrVarList(), RPAREN),
            Sequence(Terminal("range"), LPAREN, IntegerLiteral(), COMMA, IntegerLiteral(), RPAREN),
            Sequence(Terminal("size_is"), LPAREN, AttrVarList(), RPAREN),
            UsageAttribute(),
            UnionInstanceSwitchAttr(),
            Terminal("ignore"),
            PtrAttr()
        );
    }

    //(68) <attr_var_list> ::= <attr_var> [ , <attr_var> ] ...
    Rule AttrVarList() {
        return Sequence(
            AttrVar(),
            ZeroOrMore(Sequence(COMMA, AttrVar()))
        );
    }

    //(69) <attr_var> ::= [ [ * ] <Identifier> ]
    Rule AttrVar() {
        return Optional(Optional(STAR), Identifier());
    }

    //(70) <pointer_opt> ::= [<pointer>]
    Rule PointerOpt() {
        return Optional(Pointer());
    }

    //(70.1) <ptr_attr> ::= ref
    //	| unique
    //	| ptr
    Rule PtrAttr() {
        return FirstOf(
            Terminal("ref"),
            Terminal("unique"),
            Terminal("ptr")
        );
    }

    //(70.2) <pointer> ::= *...
    Rule Pointer() {
        return Sequence(OneOrMore("*"), Spacing());
    }

    //(71) <op_declarator> ::= [ <operation_attributes> ]
    //	<simple_type_spec> <Identifier> <param_declarators>
    Rule OpDeclarator() {
        return Sequence(
            Optional(OperationAttributes()),
            SimpleTypeSpec(),
            Identifier(),
            ParamDeclarators()
        );
    }

    //(72) <operation_attributes> ::= <[>  <operation_attribute>
    //	[ , <operation_attribute> ] ... <]>
    Rule OperationAttributes() {
        return Sequence(
            LBRACKET,
            OperationAttribute(),
            ZeroOrMore(COMMA, OperationAttribute()),
            RBRACKET
        );
    }

    //(73) <operation_attribute> ::= idempotent
    //	| broadcast
    //	| maybe
    //	| reflect_deletions
    //	| <usage_attribute>
    //	| <ptr_attr>
    Rule OperationAttribute() {
        return FirstOf(
            Terminal("idempotent"),
            Terminal("broadcast"),
            Terminal("maybe"),
            Terminal("reflect_deletions"),
            UsageAttribute(),
            PtrAttr()
        );
    }

    //(74) <param_declarators> ::= ( [ <param_declarator>
    //	[ , <param_declarator> ] ... ] )
    //        | ( void )
    Rule ParamDeclarators() {
        return Sequence(
            LPAREN,
            FirstOf(
                Terminal("void"),
                Sequence(ParamDeclarator(), ZeroOrMore(COMMA, ParamDeclarator()))
            ),
            RPAREN
        );
    }

    //(75) <param_declarator> ::= <param_attributes> <type_spec> <declarator>
    Rule ParamDeclarator() {
        return Sequence(ParamAttributes(), TypeSpec(), Declarator());
    }

    //(76) <param_attributes> ::= <[> <param_attribute>
    //	[ , <param_attribute> ] ... <]>
    Rule ParamAttributes() {
        return Sequence(
            LBRACKET,
            ParamAttribute(),
            ZeroOrMore(COMMA, ParamAttribute()),
            RBRACKET
        );
    }

    //(77) <param_attribute> ::= <directional_attribute>
    //	| <field_attribute>
    Rule ParamAttribute() {
        return FirstOf(
            DirectionalAttribute(),
            FieldAttribute()
        );
    }

    //(78) <directional_attribute> ::= in
    //	| out
    Rule DirectionalAttribute() {
        return FirstOf(
            Terminal("in"),
            Terminal("out")
        );
    }

    //(80) <predefined_type_spec> ::= error_status_t
    //	| <international_character_type>
    Rule PredefinedTypeSpec() {
        return FirstOf(
            Terminal("error_status_t"),
            InternationalCharacterType()
        );
    }

    //(81) <international_character_type> ::= ISO_LATIN_1
    //	| ISO_MULTI_LINGUAL
    //	| ISO_UCS
    Rule InternationalCharacterType() {
        return FirstOf(
            Terminal("ISO_LATIN_1"),
            Terminal("ISO_MULTI_LINGUAL"),
            Terminal("ISO_UCS")
        );
    }

    @SuppressNode
    @SuppressSubnodes
    Rule Spacing() {
        return ZeroOrMore(FirstOf(

            // whitespace
            OneOrMore(AnyOf(" \t\r\n\f").label("Whitespace")),

            // traditional comment
            Sequence("/*", ZeroOrMore(TestNot("*/"), ANY), "*/"),

            // end of line comment
            Sequence(
                "//",
                ZeroOrMore(TestNot(AnyOf("\r\n")), ANY),
                FirstOf("\r\n", '\r', '\n', EOI)
            )
        ));
    }

    Rule Uuid() {
        return Sequence(
            NTimes(8, HexDigit()),
            "-",
            NTimes(4, HexDigit()),
            "-",
            NTimes(4, HexDigit()),
            "-",
            NTimes(4, HexDigit()),
            "-",
            NTimes(12, HexDigit()),
            Spacing()
        );
    }

    @SuppressSubnodes
    @MemoMismatches
    Rule Identifier() {
        return Sequence(TestNot(Keyword()), Letter(), ZeroOrMore(LetterOrDigit()), Spacing());
    }

    @SuppressSubnodes
    Rule IntegerLiteral() {
        return Sequence(
            Optional(MINUS),
            OneOrMore(CharRange('0', '9')),
            Spacing()
        );
    }

    Rule Keyword() {
        return Sequence(
            FirstOf("boolean", "byte", "case", "char", "const", "default", "double",
                "enum", "FALSE", "float", "handle_t", "hyper", "import", "interface", "int",
                "long", "NULL", "pipe", "short", "small", "struct", "switch", "TRUE", "typedef",
                "union", "unsigned", "void"),
            TestNot(LetterOrDigit())
        );
    }

    Rule Letter() {
        // switch to this "reduced" character space version for a ~10% parser performance speedup
        //return FirstOf(CharRange('a', 'z'), CharRange('A', 'Z'), '_', '$');
        return new IDLLetterMatcher();
    }

    Rule LetterOrDigit() {
        return new IDLLetterOrDigitMatcher();
    }

    Rule HexDigit() {
        return AnyOf("0123456789abcdefABCDEF");
    }

    @SuppressNode
    @DontLabel
    Rule Terminal(String string) {
        return Sequence(string, Spacing()).label('\'' + string + '\'');
    }

    @SuppressNode
    @DontLabel
    Rule Terminal(String string, Rule mustNotFollow) {
        return Sequence(string, TestNot(mustNotFollow), Spacing()).label('\'' + string + '\'');
    }
}
