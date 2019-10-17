/**
 * Several simple math functions.
 *
 * They are rather dedicated to the X86_64 architecture but implemented for
 * X86 too, even if parameters loading is less adequate and slower under this
 * architecture.
 */
module iz.math;

import
    std.traits, std.complex;

/**
 * When the conditional version identifer "ensure_sseRoundingMode" is
 * set then the current rounding mode is reset to the default value.
 */
version(ensure_sseRoundingMode) static this()
{
    roundToNearest;
}

/**
 * Saves the rounding mode used in iz.math
 * (always the 13th and 14th bit of SSE MXCSR).
 *
 * Returns:
 *      The current rounding mode, as a read-only integer value.
 */
const(int) saveIzRoundingMode() @trusted pure nothrow
{
    version(X86) asm pure nothrow
    {
        naked;
        sub     ESP, 4;
        stmxcsr dword ptr [ESP-4];
        mov     EAX, dword ptr [ESP-4];
        add     ESP, 4;
        ret;
    }
    else version(X86_64) asm pure nothrow
    {
        naked;
        sub     RSP, 4;
        stmxcsr dword ptr [RSP-4];
        mov     EAX, dword ptr [RSP-4];
        add     RSP, 4;
        ret;
    }
    else static assert(0, "unsupported architecture");
}

/**
 * Restores the rounding mode used in iz.math (MXCSR).
 *
 * iz.math provides the functions to set all the possible modes and this function
 * should only be used to restore a value saved by the roundToXXX functions.
 * When used freely, this function will only work when the 13th and the 14th bit
 * modify the current MXCSR register.
 */
void setIzRoundingMode(int value) @trusted pure nothrow
{
    int curr;
    asm pure nothrow
    {
        stmxcsr curr;
    }
    import iz.sugar: maskBit;
    curr = maskBit!13(curr);
    curr = maskBit!14(curr);
    curr |= value;
    asm pure nothrow
    {
        ldmxcsr curr;
    }
}

/**
 * Resets the default rounding mode.
 *
 * After the call, floor() and ceil() are conform to the specifications.
 *
 * Returns:
 *   The previous rounding mode, which can be restored with setIzRoundingMode().
 */
const(int) roundToNearest() @trusted pure nothrow
{
    const int result = saveIzRoundingMode;
    import iz.sugar: maskBit;
    int newMode = result;
    newMode = maskBit!13(newMode);
    newMode = maskBit!14(newMode);
    setIzRoundingMode(newMode);
    return result;
}

/**
 * Sets round() to behave like floor().
 *
 * After the call, floor() and ceil() are not anymore reliable.
 *
 * Returns:
 *   The previous rounding mode, which can be restored with setIzRoundingMode().
 */
const(int) roundToPositive() @trusted pure nothrow
{
    const int result = saveIzRoundingMode;
    import iz.sugar: maskBit;
    int newMode = result;
    newMode = maskBit!13(newMode);
    newMode |= 1 << 14;
    setIzRoundingMode(newMode);
    return result;
}

/**
 * Sets round() to behave like ceil().
 *
 * After the call, floor() and ceil() are not anymore reliable.
 *
 * Returns:
 *   The previous rounding mode, which can be restored with setIzRoundingMode().
 */
const(int) roundToNegative() @trusted pure nothrow
{
    const int result = saveIzRoundingMode;
    import iz.sugar: maskBit;
    int newMode = result;
    newMode = maskBit!14(newMode);
    newMode |= 1 << 13;
    setIzRoundingMode(newMode);
    return result;
}

/**
 * Sets round() to behave like trunc().
 *
 * After the call, floor() and ceil() are not anymore reliable.
 *
 * Returns:
 *      The previous rounding mode, which can be restored with setIzRoundingMode().
 */
const(int) roundToZero() @trusted pure nothrow
{
    const int result = saveIzRoundingMode;
    int newMode = result;
    newMode |= 1 << 13;
    newMode |= 1 << 14;
    setIzRoundingMode(newMode);
    return result;
}
///
@safe pure nothrow unittest
{
    auto sav = roundToNearest;
    assert(round(0.4) == 0);
    auto dnc0 = roundToZero;
    assert(round(0.8) == 0);
    assert(round(-0.8) == 0);
    auto dnc1 = roundToNegative;
    assert(round(1.8) == 1);
    assert(round(-0.8) == -1);
    auto dnc2 = roundToPositive;
    assert(round(1.8) == 2);
    assert(round(-0.8) == 0);
    setIzRoundingMode(sav);
}

/**
 * Converts a floating point value to an integer.
 *
 * This function requires the SSE2 instruction set and it can't be inlined.
 *
 * Params:
 *      value = Either a float or a double.
 *
 * Returns:
 *      An integer equal to the nearest integral value.
 */
extern(C) int round(T)(T value) @trusted pure nothrow
{
    version(X86_64)
    {
        static if (is(T==float)) asm pure nothrow
        {
            naked;
            cvtss2si EAX, XMM0;
            ret;
        }
        else static if (is(T==double)) asm pure nothrow
        {
            naked;
            cvtsd2si EAX, XMM0;
            ret;
        }
        else static assert(0, "unsupported FP type");
    }
    else version(X86)
    {
        static if (is(T==float)) asm pure nothrow
        {
            naked;
            push EBP;
            mov EBP, ESP;
            movss XMM0, value;
            cvtss2si EAX, XMM0;
            mov ESP, EBP;
            pop EBP;
            ret;
        }
        else static if (is(T==double)) asm pure nothrow
        {
            naked;
            push EBP;
            mov EBP, ESP;
            movsd XMM0, value;
            cvtsd2si EAX, XMM0;
            mov ESP, EBP;
            pop EBP;
            ret;
        }
        else static assert(0, "unsupported FP type");          
    }
    else static assert(0, "unsupported architecture");
}
///
@safe pure nothrow unittest
{
    assert(round(0.2f) == 0);
    assert(round(0.8f) == 1);
    assert(round(-0.2f) == 0);
    assert(round(-0.8f) == -1);
}

/**
 * Converts a floating point value to an integer.
 *
 * This function requires the SSE2 instruction set and it can't be inlined.
 *
 * Params:
 *      value = Either a float or a double.
 *
 * Returns:
 *      The largest integral value that is not greater than $(D_PARAM value).
 */
int floor(T)(T value) @trusted pure nothrow
if (isFloatingPoint!T)
{
    const T offs = -0.5;
    return round(value + offs);
}
///
@safe pure nothrow unittest
{
    assert(floor(0.2f) == 0);
    assert(floor(0.8f) == 0);
    assert(floor(-0.2f) == -1);
    assert(floor(-0.8f) == -1);
}

/**
 * Converts a floating point value to an integer.
 *
 * This function requires the SSE2 instruction set and it can't be inlined.
 *
 * Params:
 *      value = Either a float or a double.
 *
 * Returns:
 *      The smallest integral value that is not less than $(D_PARAM value).
 */
int ceil(T)(T value) @trusted pure nothrow
if (isFloatingPoint!T)
{
    const T offs = 0.5;
    return round(value + offs);
}
///
@safe pure nothrow unittest
{
    assert(ceil(0.2f) == 1);
    assert(ceil(0.8f) == 1);
    assert(ceil(-0.2f) == 0);
    assert(ceil(-0.8f) == 0);
}

/**
 * Converts a floating point value to an integer.
 *
 * This function requires the SSE2 instruction set and it can't be inlined.
 *
 * Params:
 *      value = Either a float or a double.
 *
 * Returns:
 *       An integral value equal to the $(D_PARAM value) nearest integral toward 0.
 */
extern(C) int trunc(T)(T value) @trusted pure nothrow
{
    version(X86_64)
    {
        static if (is(T==float)) asm pure nothrow
        {
            naked;
            cvttss2si EAX, XMM0;
            ret;
        }
        else static if (is(T==double)) asm pure nothrow
        {
            naked;
            cvttsd2si EAX, XMM0;
            ret;
        }
        else static assert(0, "unsupported FP type");
    }
    else version(X86)
    {
        static if (is(T==float)) asm pure nothrow
        {
            naked;
            push EBP;
            mov EBP, ESP;
            movss XMM0, value;
            cvttss2si EAX, XMM0;
            mov ESP, EBP;
            pop EBP;
            ret;
        }
        else static if (is(T==double)) asm pure nothrow
        {
            naked;
            push EBP;
            mov EBP, ESP;
            movsd XMM0, value;
            cvttsd2si EAX, XMM0;
            mov ESP, EBP;
            pop EBP;
            ret;
        }
        else static assert(0, "unsupported FP type");          
    }
    else static assert(0, "unsupported architecture");
}
///
@safe pure nothrow unittest
{
    assert(trunc(0.2f) == 0);
    assert(trunc(0.8f) == 0);
    assert(trunc(-0.2f) == 0);
    assert(trunc(-8.8f) == -8);
}

/**
 * Converts a floating point value to an integer.
 *
 * This function relies on the D behavior when casting a floating point value
 * to an integer. It uses SSE2 on X86_64 and the FPU on X86 but it can always be 
 * inlined.
 *
 * Params:
 *      value = Either a float or a double.
 *
 * Returns:
 *      An integral value equal to the $(D_PARAM value) nearest integral toward 0.
 */
int dtrunc(T)(T value) @trusted pure nothrow
{
    static if (is(T==float) || is(T==double))
        return cast(int) value;
    else
        static assert(0, "unsupported FP type");
}
///
@safe pure nothrow unittest
{
    assert(dtrunc(0.2f) == 0);
    assert(dtrunc(0.8f) == 0);
    assert(dtrunc(-0.2f) == 0);
    assert(dtrunc(-8.8f) == -8);
}

/**
 * Computes the hypothenus of two FP numbers.
 *
 * This function requires the SSE2 instruction set and it can't be inlined.
 *
 * Params:
 *      x = Either a float or a double.
 *      y = Either a float or a double.
 *
 * Returns:
 *      A floating point value of type T.
 */
extern(C) T hypot(T)(T x, T y) pure @trusted nothrow
{
    version(X86_64)
    {
        static if (is(T==float)) asm pure nothrow
        {
            naked;
            mulps   XMM0, XMM0;
            mulps   XMM1, XMM1;
            addps   XMM0, XMM1;
            sqrtps  XMM0, XMM0;
            ret;
        }
        else static if (is(T==double)) asm pure nothrow
        {
            naked;
            mulpd   XMM0, XMM0;
            mulpd   XMM1, XMM1;
            addpd   XMM0, XMM1;
            sqrtpd  XMM0, XMM0;
            ret;
        }
        else static assert(0, "unsupported FP type");
    }
    else version(X86)
    {
        static if (is(T==float)) asm pure nothrow
        {
            naked;
            push    EBP;
            mov     EBP, ESP;
            sub     ESP, 4;
            movss   XMM0, x;
            movss   XMM1, y;
            mulps   XMM0, XMM0;
            mulps   XMM1, XMM1;
            addps   XMM0, XMM1;
            sqrtps  XMM0, XMM0;
            movss   dword ptr [ESP-4], XMM0;
            fld     dword ptr [ESP-4];
            add     ESP, 4;
            mov     ESP, EBP;
            pop     EBP;
            ret;
        }
        else static if (is(T==double)) asm pure nothrow
        {
            naked;
            push    EBP;
            mov     EBP, ESP;
            sub     ESP, 8;
            movsd   XMM0, x;
            movsd   XMM1, y;
            mulpd   XMM0, XMM0;
            mulpd   XMM1, XMM1;
            addpd   XMM0, XMM1;
            sqrtpd  XMM0, XMM0;
            movsd   qword ptr [ESP-8], XMM0;
            fld     qword ptr [ESP-8];
            add     ESP, 8;
            mov     ESP, EBP;
            pop     EBP;
            ret;
        }
        else static assert(0, "unsupported FP type");
    }
    else static assert(0, "unsupported architecture");
}
///
pure @safe nothrow unittest
{
    assert(hypot(3.0,4.0) == 5.0);
    assert(hypot(3.0f,4.0f) == 5.0f);
}

/**
 * Returns the magnitude of a complex number.
 *
 * Convenience function that calls hypot() either with a clfloat, a cdouble or
 * a std.complex.Complex.
 */
auto magn(T)(T t)
if (is(T==cfloat) || is(T==cdouble) || is(T==Complex!float) || is(T==Complex!double))
{
    return hypot(t.re, t.im);
}
///
nothrow pure @safe unittest
{
    assert(magn(cdouble(3.0+4.0i)) == 5.0);
    assert(magn(cfloat(3.0f+4.0fi)) == 5.0f);
    assert(magn(complex(3.0f,4.0f)) == 5.0f);
    assert(magn(complex(3.0,4.0)) == 5.0);
}

/**
 * Wraps a numeric value between 0 and a max value.
 *
 * Params:
 *      bound = a string that indicates if the max is excluded ($(D ")")), the default
 *          or if the max is included ($(D "]")).
 *      value = the value to wrap.
 *      max the maximal value.
 */
T wrap(string bound = ")", T)(T value, T max)
if (isNumeric!T && (bound == ")" || bound == "]"))
{
    static if (bound == ")")
    {
        if (value > max)
            return value - max;
    }
    static if (bound == "]")
    {
        if (value >= max)
            return value - max;
    }
    if (value < 0)
        return max + value;
    else
        return value;
}
///
nothrow @nogc @safe pure unittest
{
    import std.math: approxEqual;
    import std.math: modf;
    // wrap from max
    assert(wrap(1,1) == 1);
    assert(wrap(3,2) == 1);
    assert(wrap(-1,3) == 2);
    assert(wrap(1.5,1).approxEqual(0.5));
    assert(wrap(1.01,1).approxEqual(0.01));
    assert(wrap(-0.5,2).approxEqual(1.5));
    // wrap past max
    assert(wrap!"]"(1,1) == 0);
    assert(wrap!"]"(3,2) == 1);
    assert(wrap!"]"(3,3) == 0);
    assert(wrap!"]"(1.0,1.0) == 0.0);
    assert(wrap!"]"(-0.5,1.0) == 0.5);
    // two phases in sync
    double incr = 0.0125;
    double sync = 0.25;
    double phase1 = 0, phase2 = 0;
    foreach(i; 0 .. 100000)
    {
        phase1 = wrap(phase1 + incr, 1.0);
        phase2 = wrap(phase1 - sync, 1.0);
        assert(phase1 < 1.0 && phase2 < 1.0);
    }
}

/**
 * Allows to represent fractions of PI without using the usual suffixes such as
 * "two", "half", etc.
 */
template Pi(int a, int b = 1)
{
    import std.math: PI;
    enum Pi = PI * a / b;
}
///
unittest
{
    static assert(Pi!(2,1) == Pi!(4,2));
    static assert(Pi!(1,2) == Pi!(1,8) * 4);
    static assert(Pi!(4,2) == Pi!2);
}

/// Log(0.5) as a double;
enum logHalf = -0.69314718055994531;

/**
 * Retrieves the exponent part on the result of pow().
 *
 * Params:
 *      x = The first pow() argument.
 *      y = The pow() result.
 * Returns:
 *      The exponent N that verifies y = pow(x, N).
 */
double logN(double x, double y)
{
    import std.math;
    return log(y) / log(x);
}
///
unittest
{
    import std.math;
    double[] exps = [3, 1.4, 2.2, 0.0001, 0.9999];
    foreach(e; exps)
    {
        double y = pow(0.5, e);
        double n = logN(0.5, y);
        assert(n.approxEqual(e));
    }
}

/**
 * A simple and fast parametric easing function.
 *
 * Its name comes from the fact that the plot of f(x) in [0..1] and with a
 * control of 3 forms a regular parabolic curve.
 */
struct VariableParabol
{
    @disable this(this);

    /**
     * Applies the standard transformation.
     *
     * Params:
     *      x = The X coordinate, between 0.0 and 1.0
     *      c = The control of the speed, between 0 (similar to InExpo)
     *      and 3.0 (similar to OutExpo)
     *
     * Returns:
     *      The Y coordinate, a value between 0.0 and 1.0.
     */
    static double fx(int NC = 1)(double x, double c) pure @safe @nogc
    {
        version(X86_64) {version(DigitalMars){
            enum PureD = false;} else enum PureD = true;}
        else // X86 with FPU, better codegen with LDC, etc
            enum PureD = true;
        static if (PureD)
        {
            assert(0 <= x && x <= 1.0);
            assert(0 <= c && c <= 3.0);
            return x*x*x - x*x*c + x*c;
        }
        else asm pure nothrow @nogc @safe
        {
            naked;
            movapd  XMM2, XMM1; // saves x
            mulsd   XMM2, XMM2; // x²
            movapd  XMM3, XMM2; // saves x²
            mulsd   XMM3, XMM1; // x³
            mulsd   XMM2, XMM0; // cx²
            subsd   XMM3, XMM2; // x³ - cx²
            mulsd   XMM0, XMM1; // cx
            addsd   XMM0, XMM3; // x³ - cx² + cx
            ret;
        }
    }

    /**
     * Retrieves the control point coefficient.
     *
     * Params:
     *      y = The value as obtained by fx(0.5, control).
     *
     * Returns:
     *      a value between 0.0 and 3.0.
     */
    static double control(int N = 0)(double y) pure @safe @nogc
    in
    {
        assert(0 <= y && y <= 1.0);
    }
    out (c)
    {
        assert(0 <= c && c <= 3.0);
    }
    body
    {
        return controlClip(4 * (y - 0.125));
    }

    /**
     * Computes the Y coordinate of the control point.
     *
     * Returns:
     *      The same as fx(0.5, c) but faster.
     */
    static double controlFx(int N = 0)(double c) pure @safe @nogc
    in
    {
        assert(0 <= c && c <= 3.0);
    }
    out (y)
    {
        assert(0 <= y && y <= 1.0);
    }
    body
    {
        return 0.125 + 0.25 * c;
    }

    /**
     * Clips the control point coefficient.
     *
     * Params:
     *      c = The control point coefficient.
     * Returns:
     *      The control point, validated for fx().
     */
    static double controlClip(double c) pure @safe @nogc
    {
        import std.algorithm.comparison: clamp;
        return c.clamp(0.0, 3.0);
    }

    /**
     * Indicates that 1 control point is used.
     */
    enum numControls = 1;

    /// Uniform Easing API.
    static shared VariableParabol ease;
}
///
@nogc pure @safe unittest
{
    double x = 0;
    double x0 = 120;
    double x1 = 480;
    double h = 200;
    const int numPoints = 1000;
    const double increment = 1.0 / numPoints;

    // draw a curve...
    foreach(i; 0..numPoints)
    {
        double y = variableParabol.fx(x, 0.1);
        // lineTo(x * (480 - 120) + 120, y * h);
        x += increment;
    }
    import std.math: approxEqual;
    assert(x.approxEqual(1.0));

    enum inc = 0.001;
    double control = 0;
    foreach(i; 0..1000)
    {
        auto cy = variableParabol.fx(0.5, control);
        // Operation to get the control value as function of the cursor
        assert(variableParabol.control(cy) == control);
        control += 3 / 1000;
    }

    assert(variableParabol.fx(0.0,3.0) == 0.0);
    assert(variableParabol.fx(1.0,3.0) == 1.0);
    assert(variableParabol.fx(0.0,2.0) == 0.0);
    assert(variableParabol.fx(1.0,2.0) == 1.0);
}

///
alias variableParabol = VariableParabol;


/**
 * Parametric easing function using pow().
 */
struct VariablePow
{
    @disable this(this);

    /**
     * Applies the standard transformation.
     *
     * Params:
     *      x = The X coordinate, between 0.0 and 1.0
     *      c = The control of the speed, between 0 (InExpo)
     *      and 9.0 (OutExpo)
     *
     * Returns:
     *      The Y coordinate, a value between 0.0 and 1.0.
     */
    static double fx(int NC = 1)(double x, double c) pure @safe @nogc
    in
    {
        assert(0 <= x && x <= 1.0);
        assert(0.05 <= c && c <= 9.0);
    }
    body
    {
        import std.math: pow;
        return pow(x, c);
    }

    /**
     * Retrieves the control point coefficient.
     *
     * Params:
     *      y = The value as obtained by fx(0.5, control).
     *
     * Returns:
     *      a value between 0.0 and 9.0.
     */
    static double control(int N = 0)(double y) pure @safe @nogc
    in
    {
        assert(0 <= y && y <= 1.0);
    }
    out (c)
    {
        assert(0.05 <= c && c <= 9.0);
    }
    body
    {
        import std.math: log;
        return controlClip(log(y) / logHalf);
    }

    /**
     * Computes the Y coordinate of the control point.
     *
     * Returns:
     *      The same as fx(0.5, c).
     */
    static double controlFx(int N = 0)(double c) pure @safe @nogc
    in
    {
        assert(0.05 <= c && c <= 9.0);
    }
    out (y)
    {
        assert(0 <= y && y <= 1.0);
    }
    body
    {
        return fx(0.5, c);
    }

    /**
     * Clips the control point coefficient.
     *
     * Params:
     *      c = The control point coefficient.
     * Returns:
     *      The control point, validated for fx().
     */
    static double controlClip(double c) pure @safe @nogc
    {
        import std.algorithm.comparison: clamp;
        return c.clamp(0.05, 9.0);
    }

    /**
     * Indicates that 1 control point is used.
     */
    enum numControls = 1;

    /// Uniform Easing API.
    static shared VariablePow ease;
}

///
alias variablePow = VariablePow;

/**
 * An parametric easing function based on the Supper Ellipse.
 */
struct VariableEllipse
{
    @disable this(this);

    /**
     * Applies the standard transformation.
     *
     * Params:
     *      x = The X coordinate, between 0.0 and 1.0
     *      c = The control of the speed, between 0 (similar to InExpo)
     *      and 3.0 (similar to OutExpo)
     *
     * Returns:
     *      The Y coordinate, a value between 0.0 and 1.0.
     */
    static double fx(int NC = 1)(double x, double c) pure @safe @nogc
    in
    {
        assert(0 <= x && x <= 1.0);
        assert(0.1 <= c && x <= 8.0);
    }
    body
    {
        import std.math: pow;
        return 1.0 - pow(1.0 - pow(x, 2.0/c), c * 0.5);
    }

    /**
     * Retrieves the control point coefficient.
     *
     * Params:
     *      y = The value as obtained by fx(0.5, control).
     *
     * Returns:
     *      A value between 0.0 and 8.0.
     */
    static double control(int N = 0)(double y) pure @safe @nogc
    in
    {
        assert(0 <= y && y <= 1.0);
    }
    out (c)
    {
        assert(0.1 <= c && c <= 8.0);
    }
    body
    {
        return controlClip(variablePow.control(1 - y));
    }

    /**
     * Computes the Y coordinate of the control point.
     *
     * Returns:
     *      The same as fx(0.5, c).
     */
    static double controlFx(int N = 0)(double c) pure @safe @nogc
    in
    {
        assert(0.1 <= c && c <= 8.0);
    }
    out (y)
    {
        assert(0 <= y && y <= 1.0);
    }
    body
    {
        return fx(0.5, c);
    }

    /**
     * Clips the control point coefficient.
     *
     * Params:
     *      c = The control point coefficient.
     * Returns:
     *      The control point, validated for fx().
     */
    static double controlClip(double c) pure @safe @nogc
    {
        import std.algorithm.comparison: clamp;
        return c.clamp(0.1, 8.0);
    }

    /**
     * Indicates that 1 control point is used.
     */
    enum numControls = 1;

    /// Uniform Easing API.
    static shared VariableEllipse ease;
}

///
alias variableEllipse = VariableEllipse;

unittest
{
    assert(VariableEllipse.control(VariableEllipse
        .control(VariableEllipse.fx(0.5,0.5)) == 0.5));
}

