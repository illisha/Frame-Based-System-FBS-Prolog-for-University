
:- dynamic pce_post_expansion_hook/2.
:- multifile pce_post_expansion_hook/2.


sciences(a_kind_of, faculty).

course(a_part_of, department).

student(a_part_of, department).

:- dynamic pce_pre_expansion_hook/2.
:- multifile pce_pre_expansion_hook/2.

pce_pre_expansion_hook(In, Out) :-
    emacs_extend:emacs_expansion(In, Out).
pce_pre_expansion_hook(A, B) :-
    user:emacs_extend:emacs_expansion(A, B).
pce_pre_expansion_hook(A, B) :-
    user:emacs_extend:emacs_expansion(A, B).
pce_pre_expansion_hook(A, B) :-
    user:emacs_extend:emacs_expansion(A, B).

:- dynamic'Pooja Malik'/0.


administrationOffice(a_part_of, university).

girls(a_kind_of, hostel).

business(a_kind_of, faculty).

:- dynamic'Sidharth Singh'/0.


faculty(a_part_of, department).

:- dynamic'Pooja Malik'/2.

'Pooja Malik'(instance_of, faculty).

undergraduate(a_kind_of, student).

:- dynamic'Deepa'/0.


'Siddharth Singh'(instance_of, faculty).
'Siddharth Singh'(age, 48).
'Siddharth Singh'(intrests, 'Badminton, Chess, Reading, Singing').
'Siddharth Singh'(experience, '5 years').

department(a_part_of, university).

:- multifile prolog_predicate_name/2.

prolog_predicate_name(pce_principal:send_implementation(Id0, _, _), Id) :-
    pce_portray:
    (   method_from_id(Id0, SG),
        atom_from_method(SG, Id)
    ).
prolog_predicate_name(pce_principal:get_implementation(Id0, _, _, _), Id) :-
    pce_portray:
    (   method_from_id(Id0, SG),
        atom_from_method(SG, Id)
    ).
prolog_predicate_name(pce_principal:send_implementation(A, _, _), C) :-
    user:
    (   pce_portray:method_from_id(A, B),
        pce_portray:atom_from_method(B, C)
    ).
prolog_predicate_name(pce_principal:get_implementation(A, _, _, _), C) :-
    user:
    (   pce_portray:method_from_id(A, B),
        pce_portray:atom_from_method(B, C)
    ).
prolog_predicate_name(pce_principal:send_implementation(A, _, _), C) :-
    user:
    (   pce_portray:method_from_id(A, B),
        pce_portray:atom_from_method(B, C)
    ).
prolog_predicate_name(pce_principal:get_implementation(A, _, _, _), C) :-
    user:
    (   pce_portray:method_from_id(A, B),
        pce_portray:atom_from_method(B, C)
    ).
prolog_predicate_name(pce_principal:send_implementation(A, _, _), C) :-
    user:
    (   pce_portray:method_from_id(A, B),
        pce_portray:atom_from_method(B, C)
    ).
prolog_predicate_name(pce_principal:get_implementation(A, _, _, _), C) :-
    user:
    (   pce_portray:method_from_id(A, B),
        pce_portray:atom_from_method(B, C)
    ).

hostel(a_part_of, university).

:- dynamic'Siddharth Singh'/0.


:- dynamic'Illisha Singh'/2.

'Illisha Singh'(department, 'Computer Science').
'Illisha Singh'(age, 19).
'Illisha Singh'(instance_of, student).
'Illisha Singh'(department, 'Computer Science').
'Illisha Singh'(age, 19).
'Illisha Singh'(instance_of, student).

:- dynamic prolog_exception_hook/4.
:- multifile prolog_exception_hook/4.

prolog_exception_hook(error(E, context(Ctx0, Msg)), error(E, context(prolog_stack(Stack), Msg)), Fr, GuardSpec) :-
    prolog_stack:
    (   current_prolog_flag(backtrace, true),
        \+ is_stack(Ctx0, _Frames),
        (   atom(GuardSpec)
        ->  debug(backtrace,
                  'Got uncaught (guard = ~q) exception ~p (Ctx0=~p)',
                  [GuardSpec, E, Ctx0]),
            stack_guard(GuardSpec),
            Guard=GuardSpec
        ;   prolog_frame_attribute(GuardSpec,
                                   predicate_indicator,
                                   Guard),
            debug(backtrace,
                  'Got exception ~p (Ctx0=~p, Catcher=~p)',
                  [E, Ctx0, Guard]),
            stack_guard(Guard)
        ),
        (   current_prolog_flag(backtrace_depth, Depth)
        ->  Depth>0
        ;   Depth=20
        ),
        get_prolog_backtrace(Depth,
                             Stack0,
                             [frame(Fr), guard(Guard)]),
        debug(backtrace, 'Stack = ~p', [Stack0]),
        clean_stack(Stack0, Stack1),
        join_stacks(Ctx0, Stack1, Stack)
    ).
prolog_exception_hook(error(A, context(C, B)), error(A, context(prolog_stack(J), B)), G, D) :-
    user:
    (   prolog_stack:current_prolog_flag(backtrace, true),
        \+ prolog_stack:is_stack(C, _),
        (   atom(D)
        ->  prolog_stack:debug(backtrace, 'Got uncaught (guard = ~q) exception ~p (Ctx0=~p)', [D, A, C]),
            prolog_stack:stack_guard(D),
            E=D
        ;   prolog_stack:prolog_frame_attribute(D, predicate_indicator, E),
            prolog_stack:debug(backtrace, 'Got exception ~p (Ctx0=~p, Catcher=~p)', [A, C, E]),
            prolog_stack:stack_guard(E)
        ),
        (   prolog_stack:current_prolog_flag(backtrace_depth, F)
        ->  prolog_stack:(F>0)
        ;   F=20
        ),
        prolog_stack:get_prolog_backtrace(F, H, [frame(G), guard(E)]),
        prolog_stack:debug(backtrace, 'Stack = ~p', [H]),
        prolog_stack:clean_stack(H, I),
        prolog_stack:join_stacks(C, I, J)
    ).
prolog_exception_hook(error(A, context(C, B)), error(A, context(prolog_stack(J), B)), G, D) :-
    user:
    (   prolog_stack:current_prolog_flag(backtrace, true),
        \+ prolog_stack:is_stack(C, _),
        (   atom(D)
        ->  prolog_stack:debug(backtrace, 'Got uncaught (guard = ~q) exception ~p (Ctx0=~p)', [D, A, C]),
            prolog_stack:stack_guard(D),
            E=D
        ;   prolog_stack:prolog_frame_attribute(D, predicate_indicator, E),
            prolog_stack:debug(backtrace, 'Got exception ~p (Ctx0=~p, Catcher=~p)', [A, C, E]),
            prolog_stack:stack_guard(E)
        ),
        (   prolog_stack:current_prolog_flag(backtrace_depth, F)
        ->  prolog_stack:(F>0)
        ;   F=20
        ),
        prolog_stack:get_prolog_backtrace(F, H, [frame(G), guard(E)]),
        prolog_stack:debug(backtrace, 'Stack = ~p', [H]),
        prolog_stack:clean_stack(H, I),
        prolog_stack:join_stacks(C, I, J)
    ).
prolog_exception_hook(error(A, context(C, B)), error(A, context(prolog_stack(J), B)), G, D) :-
    user:
    (   prolog_stack:current_prolog_flag(backtrace, true),
        \+ prolog_stack:is_stack(C, _),
        (   atom(D)
        ->  prolog_stack:debug(backtrace, 'Got uncaught (guard = ~q) exception ~p (Ctx0=~p)', [D, A, C]),
            prolog_stack:stack_guard(D),
            E=D
        ;   prolog_stack:prolog_frame_attribute(D, predicate_indicator, E),
            prolog_stack:debug(backtrace, 'Got exception ~p (Ctx0=~p, Catcher=~p)', [A, C, E]),
            prolog_stack:stack_guard(E)
        ),
        (   prolog_stack:current_prolog_flag(backtrace_depth, F)
        ->  prolog_stack:(F>0)
        ;   F=20
        ),
        prolog_stack:get_prolog_backtrace(F, H, [frame(G), guard(E)]),
        prolog_stack:debug(backtrace, 'Stack = ~p', [H]),
        prolog_stack:clean_stack(H, I),
        prolog_stack:join_stacks(C, I, J)
    ).

:- dynamic'Co-Ed Hostel'/2.

'Co-Ed Hostel'(a_kind_of, hostel).

:- dynamic'Sociology'/2.

'Sociology'(credits, 4).
'Sociology'(enrolled, '57').
'Sociology'(a_kind_of, humanities).

:- dynamic'Sidharth Singh'/2.


value(A, B, C) :-
    iscallable(A),
    D=..[A, B, C],
    call(D).
value(A, C, D) :-
    parent(A, B),
    value(B, C, D).
value(A, B, C) :-
    D=..[A, B, C],
    call(D),
    !.
value(A, C, D) :-
    parent(A, B),
    value(B, C, D).

boys(a_kind_of, hostel).

save_to_file(filename) :-
    tell(filename),
    listing,
    told.
save_to_file(A) :-
    open(A, write, B),
    with_output_to(B, listing),
    close(B).

:- dynamic'Deepa'/2.


'Sumit Gupta'(instance_of, student).
'Sumit Gupta'(age, 19).
'Sumit Gupta'(year_of_study, 3).
'Sumit Gupta'(program, 'B.Tech').

parent(A, B) :-
    iscallable(A),
    (   C=..[A, a_kind_of, B]
    ;   C=..[A, instance_of, B]
    ),
    call(C).
parent(A, B) :-
    (   C=..[A, a_kind_of, B]
    ;   C=..[A, instance_of, B]
    ),
    call(C),
    save_to_file('frame3.pl').

iscallable(administrationOffice).
iscallable(department).
iscallable(hostel).
iscallable(girls).
iscallable(boys).
iscallable(student).

'Saroj Kaushik'(instance_of, faculty).
'Saroj Kaushik'(course, 'Airtificial Intelligence').
'Saroj Kaushik'(experience, '41').

humanities(a_kind_of, faculty).

:- multifile url_path/2.


saveNow :-
    save_to_file(filename).

:- multifile prolog_clause_name/2.

prolog_clause_name(Ref, Name) :-
    pce_portray:
    (   clause(Head, _, Ref),
        user:prolog_predicate_name(Head, Name)
    ).
prolog_clause_name(A, C) :-
    user:
    (   pce_portray:clause(B, _, A),
        prolog_predicate_name(B, C)
    ).
prolog_clause_name(A, C) :-
    user:
    (   pce_portray:clause(B, _, A),
        prolog_predicate_name(B, C)
    ).
prolog_clause_name(A, C) :-
    user:
    (   pce_portray:clause(B, _, A),
        prolog_predicate_name(B, C)
    ).

engineering(a_kind_of, faculty).

:- thread_local thread_message_hook/3.
:- dynamic thread_message_hook/3.
:- volatile thread_message_hook/3.


postGraduate(a_kind_of, student).
