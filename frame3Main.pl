administrationOffice(a_part_of, university).
department(a_part_of, university).
hostel(a_part_of, university).

girls(a_kind_of, hostel).
boys(a_kind_of, hostel).

course(a_part_of, department).
faculty(a_part_of, department).
student(a_part_of, department).

sciences(a_kind_of, faculty).
humanities(a_kind_of, faculty).
engineering(a_kind_of, faculty).
business(a_kind_of, faculty).

postGraduate(a_kind_of, student).
undergraduate(a_kind_of, student).

'Siddharth Singh'(instance_of, student).
'Siddharth Singh'(age, 18).

'Saroj Kaushik'(instance_of, faculty).
'Saroj Kaushik'(course, 'Airtificial Intelligence').
'Saroj Kaushik'(experience, '41').


iscallable(administrationOffice).
iscallable(department).
iscallable(hostel).
iscallable(girls).
iscallable(boys).

value(Frame, Slot, Value):-iscallable(Frame),Query =.. [Frame, Slot, Value],call(Query).
value(Frame, Slot, Value):-parent(Frame, ParentName),value(ParentName, Slot, Value).



%--------Inference in Frames
value(Frame, Slot, Value):-
    Query =.. [Frame, Slot, Value],
    call(Query), !.     %Value is directly retrieved
value(Frame, Slot, Value):-
    parent(Frame, ParentName),
    value(ParentName, Slot, Value).     % More general rule

parent(Frame, ParentName):-    (Query =.. [Frame, a_kind_of, ParentName];
    Query =.. [Frame, instance_of, ParentName]),    call(Query).
parent(Frame, ParentName):-iscallable(Frame),(Query =.. [Frame, a_kind_of, ParentName];Query =.. [Frame, instance_of, ParentName]),call(Query).
