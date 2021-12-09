-module(day12gl).

-include_lib("wx/include/wx.hrl").
-include_lib("wx/include/gl.hrl").

-compile(export_all).

% The amount of frames between updating the simulation.
% This affects the interpolation logic.
-define(STEPS, 8).
% The fixed framerate of the update and draw logic.
% ~50FPS seems to be the upper limit for me.
-define(FRAMERATE, 144).

start_link() ->
    wx_object:start_link(?MODULE, [], []).

init(Config) ->
    WX = wx:new(Config),
    Frame = wxFrame:new(WX, ?wxID_ANY, "Advent of Code Day 12", [{size, {800, 600}}]),
    wxWindow:connect(Frame, close_window),
    wxFrame:show(Frame),
    Opts = [{size, {800, 600}}],
    GLAttrib = [{attribList, [
        ?WX_GL_RGBA,
        ?WX_GL_DOUBLEBUFFER,
        ?WX_GL_MIN_RED, 8,
        ?WX_GL_MIN_GREEN, 8,
        ?WX_GL_MIN_BLUE, 8,
        ?WX_GL_DEPTH_SIZE, 24, 0
    ]}],
    Canvas = wxGLCanvas:new(Frame, Opts ++ GLAttrib),
    wxGLCanvas:connect(Canvas, size),
    wxWindow:reparent(Canvas, Frame),
    Context = wxGLContext:new(Canvas),
    wxGLCanvas:setCurrent(Canvas, Context),
    setup_gl(Canvas),
    Timer = timer:send_interval(trunc(1000 / ?FRAMERATE), self(), update),
    Moons = parse_moons(["<x=-1, y=0, z=2>","<x=2, y=-10, z=-7>","<x=4, y=-8, z=8>","<x=3, y=5, z=-1>"]),
    { Frame, #{ canvas => Canvas, timer => Timer, moons => Moons, next_moons => simulate_gravity(Moons), step => 0 } }.

code_change(_, _, State) ->
    {stop, not_implemented, State}.

handle_cast(Msg, State) ->
    io:format("Cast: ~p~n", [Msg]),
    {noreply, State}.

handle_call(Msg, _From, State) ->
    io:format("Call: ~p~n", [Msg]),
    {reply, ok, State}.

handle_info(stop, State) ->
    timer:cancel(maps:get(timer, State)),
    wxGLCanvas:destroy(maps:get(canvas, State)),
    {stop, normal, State};

handle_info(update, #{ step := ?STEPS, next_moons := Moons } = State) ->
    NewState = State#{
        step := 0,
        moons := Moons,
        next_moons := simulate_gravity(Moons)
    },
    wx:batch(fun () -> render(NewState) end),
    {noreply, NewState};
handle_info(update, #{ step := Steps } = State) ->
    NewState = State#{
        step := Steps + 1
    },
    wx:batch(fun () -> render(NewState) end),
    {noreply, NewState}.

handle_event({wx, _, _, _, {wxClose, close_window}}, State) ->
    {stop, normal, State};

handle_event({wx, _, _, _, {wxSize, size, {Width, Height}, _}}, State) ->
    if
        Width /= 0 orelse Height /= 0 -> resize_gl_scene(Width, Height);
        true -> ok
    end,
    {noreply, State}.

terminate(_Reason, State) ->
    wxGLCanvas:destroy(maps:get(canvas, State)),
    timer:cancel(maps:get(timer, State)),
    timer:sleep(300).

setup_gl(Canvas) ->
    {W, H} = wxWindow:getClientSize(Canvas),
    resize_gl_scene(W, H),
    gl:shadeModel(?GL_SMOOTH),
    gl:clearColor(0.0, 0.0, 0.0, 0.0),
    gl:clearDepth(1.0),
    gl:enable(?GL_DEPTH_TEST),
    gl:depthFunc(?GL_LEQUAL),
    gl:hint(?GL_PERSPECTIVE_CORRECTION_HINT, ?GL_NICEST),
    ok.

resize_gl_scene(Width, Height) ->
    gl:viewport(0, 0, Width, Height),
    gl:matrixMode(?GL_PROJECTION),
    gl:loadIdentity(),
    glu:perspective(45.0, Width / Height, 0.1, 1000.0),
    gl:matrixMode(?GL_MODELVIEW),
    gl:loadIdentity(),
    ok.

draw(Moons) ->
    gl:clear(?GL_COLOR_BUFFER_BIT bor ?GL_DEPTH_BUFFER_BIT),
    gl:loadIdentity(),
    % Uncomment the following part to make the camera orbit the middle
    %Radius = 30.0,
    %CamX = math:sin(erlang:system_time() / 1000000) * Radius,
    %CamZ = math:cos(erlang:system_time() / 1000000) * Radius,
    %glu:lookAt(CamX, CamZ, 10.0, 0.0, 0.0, 0.0, 0.0, 0.0, -1.0),
    glu:lookAt(30.0, 0.0, 10.0, 0.0, 0.0, 0.0, 0.0, 0.0, -1.0),
    Quad = glu:newQuadric(),
    glu:quadricDrawStyle(Quad, ?GL_FILL),
    % Uncomment the following line to draw a 'planet' in the middle
    glu:sphere(Quad, 0.9, 36, 36),
    lists:foreach(fun ({X, Y, Z}) ->
        gl:pushMatrix(),
        gl:translatef(float(X), float(Y), float(Z)),
        glu:sphere(Quad, 0.5, 36, 36),
        gl:popMatrix()
    end, Moons),
    glu:deleteQuadric(Quad),
    ok.

get_intermediate_moons(MoonsA, MoonsB, Step) ->
    [ {AX + (BX - AX) * (Step / ?STEPS), AY + (BY - AY) * (Step / ?STEPS), AZ + (BZ - AZ) * (Step / ?STEPS) } || {{{AX, AY, AZ},_},{{BX, BY, BZ},_}} <- lists:zip(MoonsA, MoonsB)].

render(State) ->
    Canvas = maps:get(canvas, State),
    Moons = get_intermediate_moons(maps:get(moons, State), maps:get(next_moons, State), maps:get(step, State)),
    draw(Moons),
    wxGLCanvas:swapBuffers(Canvas),
    ok.

parse_moons(Lines) ->
    [ {{list_to_integer(X), list_to_integer(Y), list_to_integer(Z)}, {0, 0, 0}} ||
        {match,[_,X,Y,Z]} <- [ re:run(Line, "<x=([-\\d]+), y=([-\\d]+), z=([-\\d]+)>", [{capture, all, list}]) ||
            Line <- Lines ]].

get_increment(A, B) when A < B -> 1;
get_increment(A, B) when A > B -> -1;
get_increment(A, B) when A =:= B -> 0.

compare_all(El, Moon, Others) ->
    {_, Vel} = Moon,
    compare_all(El, Moon, Others, element(El, Vel)).

compare_all(El, {APos, AVel}, [{BPos, _}|Rest], Sum) ->
    Incr = get_increment(element(El, APos), element(El, BPos)),
    compare_all(El, {APos, AVel}, Rest, Sum + Incr);
compare_all(_, _, [], Sum) ->
    Sum.

simulate_gravity(Moons) ->
    UpdatedVelocities = update_velocities(Moons, Moons, []),
    [ {{X+XVel, Y+YVel, Z+ZVel}, {XVel, YVel, ZVel}} || {{X,Y,Z},{XVel,YVel,ZVel}} <- UpdatedVelocities ].

update_velocities([Moon|Rest], All, Acc) ->
    Others = All -- [Moon],
    XVel = compare_all(1, Moon, Others),
    YVel = compare_all(2, Moon, Others),
    ZVel = compare_all(3, Moon, Others),
    {Pos, _} = Moon,
    update_velocities(Rest, All, [{Pos, {XVel, YVel, ZVel}}|Acc]);
update_velocities([], _, Acc) ->
    lists:reverse(Acc).
