:- multifile portray_message/2.

portray_message( informational, Mess ) :-
        ( (Mess=loading(_A,_B);Mess=loaded(_V,_W,_X,_Y,_Z);Mess=imported(_C,_D)) ->
                true
                ;
                fail
        ).
