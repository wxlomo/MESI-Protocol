-- Four-state 3-hop MESI protocol with self-downgrade               --
--                                                                  --
-- Editor             : Weixuan Yang                                --
-- Email              : endreim@outlook.com                         --
--                                                                  --
-- Last modified date : March. 07, 2022                             --
----------------------------------------------------------------------
-- Constants                                                        --
----------------------------------------------------------------------
const
  ProcCount   : 3;          -- number processors
  ValueCount  : 2;          -- number of data values
  VC0         : 0;          -- virtual channel #0 @ request
                            -- (gets, getm, puts, putm, putack)
  VC1         : 1;          -- virtual channel #1 @ forwarded 
                            -- (fwdgets, fwdgetm, dgd, dgdack)
  VC2         : 2;          -- virtual channel #2 @ response
                            -- (data, exdata, invack, pute)
  VC3         : 3;          -- virtual channel #3 @ invalidation
                            -- (inv)
  NumVCs: VC3 - VC0 + 1;
  NetMax: ProcCount * 2;
  
----------------------------------------------------------------------
-- Types                                                            --
----------------------------------------------------------------------
type
  Proc  : scalarset(ProcCount);   -- unordered range of processors
  Value : scalarset(ValueCount);  -- arbitrary values for tracking coherence
  Home  : enum { HomeType };      -- need enumeration for IsMember calls
  Node  : union { Home , Proc };

  VCType: VC0..NumVCs - 1;
  CNT   : -ProcCount..ProcCount;

  MessageType: enum {  GetS,            -- obtain block in shared (read-only) state
                       GetM,            -- obtain block in modified (read-only) state
                       PutS,            -- evict block in shared state
                       PutM,            -- evict block in modified state
                       PutAck,          -- acknowledge the eviction
                       FwdGetS,         -- forward get shared
                       FwdGetM,         -- forward get modified
                       Dgd,             -- self-downgrade request
                       DgdAck,          -- acknowledge the downgrade
                       Inv,             -- invalid
                       InvAck,          -- acknowledge the invalidation
                       PutE,            -- evict block in exclusive state
                       Data,            -- the data transmission
                       ExData           -- the exclusive data transmission
                    };

  Message:
    Record
      mtype: MessageType;     -- message
      src: Node;              -- source node
      vc: VCType;             -- virtual channel
      val: Value;             -- data
      cnt: CNT;               -- acknowledge count
      old: boolean;           -- inbox flag
    End;

  HomeState:          -- memory controller states
    Record
      state: enum { H_I, H_S, H_E, H_M, 				-- stable states (invalid, shared, exclusive, modified)
      				      H_Sd }; 				       	    -- transient states (stall)
      owner: Node;	
      sharers: multiset [ProcCount] of Node;    -- list of sharers
      val: Value; 
    End;

  ProcState:          -- cache controller states
    Record
      state: enum { P_I, P_S, P_M, P_E,         -- stable states (invalid, shared, modified)
                    P_ISd, P_IMad, P_IMa, P_SMad, P_SMa, P_MIa, P_EIa, P_SIa, P_IIa
                                                -- transient states
                  };
      val: Value;
      cnt: CNT;
    End;

----------------------------------------------------------------------
-- Variables                                                        --
----------------------------------------------------------------------
var
  HomeNode     : HomeState;
  Procs        : array [Proc] of ProcState;
  Net          : array [Node] of multiset [NetMax] of Message; 
  vc_blocking  : array [Node] of array [VCType] of boolean;
  Vcs  : array [Node] of array [VCType] of Message;         -- alternate approach
  msg_processed: boolean;
  LastWrite    : Value;  -- used to confirm that writes are not lost

----------------------------------------------------------------------
-- Procedures                                                       --
----------------------------------------------------------------------
Procedure Send(mtype:MessageType;
	             dst:Node;
	             src:Node;
               vc:VCType;
               val:Value;
               cnt:CNT;
         );
var msg:Message;
Begin
  Assert (MultiSetCount(i:Net[dst], true) < NetMax) "* Too many messages";
  msg.mtype    := mtype;
  msg.src      := src;
  msg.vc       := vc;
  msg.val      := val;
  msg.cnt      := cnt;
  msg.old      := false;
  MultiSetAdd(msg, Net[dst]);
End;

Procedure ErrorUnhandledMsg(msg:Message; n:Node);
Begin
  error "* Unhandled message type";
End;

Procedure ErrorUnhandledState();
Begin
  error "* Unhandled state";
End;

Procedure AddToSharersList(n:Node);
Begin
  if MultiSetCount(i:HomeNode.sharers, HomeNode.sharers[i] = n) = 0
  then
    MultiSetAdd(n, HomeNode.sharers);
  endif;
End;

Function IsSharer(n:Node) : Boolean;
Begin
  return MultiSetCount(i:HomeNode.sharers, HomeNode.sharers[i] = n) > 0
End;

Procedure RemoveFromSharersList(n:Node);
Begin
  MultiSetRemovePred(i:HomeNode.sharers, HomeNode.sharers[i] = n);
End;

Procedure SendInvReqToSharers(rqst:Node); -- sends a message to all sharers except rqst
Begin
  for n:Node do
    if (IsMember(n, Proc) &
        MultiSetCount(i:HomeNode.sharers, HomeNode.sharers[i] = n) != 0)
    then
      if n != rqst
      then 
        Send(Inv, n, rqst, VC3, UNDEFINED, 0);
      endif;
    endif;
  endfor;
End;

Procedure ClearOwner();
Begin
  Undefine HomeNode.owner;
End;

Procedure ClearSharer();
Begin
  undefine HomeNode.sharers;
End;

-- Memory controller finite state machine ----------------------------

Procedure HomeReceive(msg:Message);
var cnt:0..ProcCount;  -- for counting sharers
Begin
-- debug output:
--  put "Receiving "; put msg.mtype; put " on VC"; put msg.vc; 
--  put " at home -- "; put HomeNode.state; put " with value "; put msg.val;
--  put "\n";
  cnt := MultiSetCount(i:HomeNode.sharers, true);
  msg_processed := true;
  switch HomeNode.state
  case H_I:
    switch msg.mtype
    case GetS:
      HomeNode.state := H_E;
      Send(ExData, msg.src, HomeType, VC2, HomeNode.val, 0);
      HomeNode.owner := msg.src;
    case GetM:
      HomeNode.state := H_M;
      Send(Data, msg.src, HomeType, VC2, HomeNode.val, cnt);
      HomeNode.owner := msg.src;
    case PutS:
      Send(PutAck, msg.src, HomeType, VC0, UNDEFINED, 0);
    case PutM:
      assert(msg.src != HomeNode.owner) "* PutM + data from owner";
      Send(PutAck, msg.src, HomeType, VC0, UNDEFINED, 0);
    case PutE:
      assert(msg.src != HomeNode.owner) "* PutE from owner";
      Send(PutAck, msg.src, HomeType, VC0, UNDEFINED, 0);
    case Dgd:
      assert(msg.src != HomeNode.owner) "* Downgrade does not processed";
    else
      ErrorUnhandledMsg(msg, HomeType);
    endswitch;

  case H_S:
    switch msg.mtype
    case GetS:
      Send(Data, msg.src, HomeType, VC2, HomeNode.val, 0);
      AddToSharersList(msg.src);
    case GetM:
      HomeNode.state := H_M;
      if(IsSharer(msg.src)) then
        Send(Data, msg.src, HomeType, VC2, HomeNode.val, cnt-1);
      else
        Send(Data, msg.src, HomeType, VC2, HomeNode.val, cnt);
      endif;
      SendInvReqToSharers(msg.src);
      ClearSharer();
      HomeNode.owner := msg.src;
    case PutS:
      if(IsSharer(msg.src) & cnt = 1) then
        HomeNode.state := H_I;
      endif;
      Send(PutAck, msg.src, HomeType, VC0, UNDEFINED, 0);
      RemoveFromSharersList(msg.src);
    case PutM:
      assert(msg.src != HomeNode.owner) "* PutM + data from owner";
      if(IsSharer(msg.src) & cnt = 1) then          -- source may be the last sharer
        HomeNode.state := H_I;
      endif;
      Send(PutAck, msg.src, HomeType, VC0, UNDEFINED, 0);
      RemoveFromSharersList(msg.src);
    case PutE:
      assert(msg.src != HomeNode.owner) "* PutE from owner";
      Send(PutAck, msg.src, HomeType, VC0, UNDEFINED, 0);
      RemoveFromSharersList(msg.src);
    case Dgd:
      assert(msg.src != HomeNode.owner) "* Downgrade does not processed";
    else
      ErrorUnhandledMsg(msg, HomeType);
    endswitch;
  
  case H_E:
    switch msg.mtype
    case Dgd:
      if(msg.src = HomeNode.owner) then
        HomeNode.state := H_Sd;
        Send(DgdAck, HomeNode.owner, HomeType, VC1, UNDEFINED, 0);
        AddToSharersList(HomeNode.owner);
        ClearOwner();
      endif;
    case GetS:
      HomeNode.state := H_Sd;   
      Send(FwdGetS, HomeNode.owner, msg.src, VC1, UNDEFINED, 0);
      AddToSharersList(msg.src);
      AddToSharersList(HomeNode.owner); 
      ClearOwner();
    case GetM:
      HomeNode.state := H_M; 
      Send(FwdGetM, HomeNode.owner, msg.src, VC1, UNDEFINED, cnt);
      HomeNode.owner := msg.src;
    case PutS:
      Send(PutAck, msg.src, HomeType, VC0, UNDEFINED, 0);
    case PutM:
      if(msg.src = HomeNode.owner) then
        HomeNode.state := H_I;
        HomeNode.val := msg.val;
        ClearOwner();
      endif;
      Send(PutAck, msg.src, HomeType, VC0, UNDEFINED, 0);
    case PutE:
      if(msg.src = HomeNode.owner) then
        HomeNode.state := H_I;
        ClearOwner();
      endif;
      Send(PutAck, msg.src, HomeType, VC0, UNDEFINED, 0);
    else
      ErrorUnhandledMsg(msg, HomeType);
    endswitch;

  case H_M:
    switch msg.mtype
    case Dgd:
      if(msg.src = HomeNode.owner) then
        HomeNode.state := H_Sd;
        Send(DgdAck, HomeNode.owner, HomeType, VC1, UNDEFINED, 0);
        AddToSharersList(HomeNode.owner);
        ClearOwner();
      endif;
    case GetS:
      HomeNode.state := H_Sd;   
      Send(FwdGetS, HomeNode.owner, msg.src, VC1, UNDEFINED, 0);
      AddToSharersList(msg.src);
      AddToSharersList(HomeNode.owner); 
      ClearOwner();
    case GetM:
      Send(FwdGetM, HomeNode.owner, msg.src, VC1, UNDEFINED, cnt);
      HomeNode.owner := msg.src;
    case PutS:
      Send(PutAck, msg.src, HomeType, VC0, UNDEFINED, 0);
    case PutM:
      if(msg.src = HomeNode.owner) then
        HomeNode.state := H_I;
        HomeNode.val := msg.val;
        ClearOwner();
      endif;
      Send(PutAck, msg.src, HomeType, VC0, UNDEFINED, 0);
    case PutE:
      assert(msg.src != HomeNode.owner) "* PutE from owner";
      Send(PutAck, msg.src, HomeType, VC0, UNDEFINED, 0);
    else
      ErrorUnhandledMsg(msg, HomeType);
    endswitch;

  case H_Sd:
    switch msg.mtype
    case GetS:
      msg_processed := false;
    case GetM:
      msg_processed := false;         
    case PutS:
      Send(PutAck, msg.src, HomeType, VC0, UNDEFINED, 0);
      RemoveFromSharersList(msg.src);
    case PutM:
      assert(msg.src != HomeNode.owner) "* PutM + data from owner";
      Send(PutAck, msg.src, HomeType, VC0, UNDEFINED, 0);
      RemoveFromSharersList(msg.src);
    case PutE:
      assert(msg.src != HomeNode.owner) "* PutE from owner";
      Send(PutAck, msg.src, HomeType, VC0, UNDEFINED, 0);
      RemoveFromSharersList(msg.src);
    case Data:
      HomeNode.state := H_S;
      HomeNode.val := msg.val;
    case Dgd:
      assert(msg.src != HomeNode.owner) "* Downgrade does not processed";
    else
      ErrorUnhandledMsg(msg, HomeType);
    endswitch;
  else
    ErrorUnhandledState();
  endswitch;
End;

-- Cache controller finite state machine -----------------------------

Procedure ProcReceive(msg:Message; p:Proc);
Begin
-- debug output:
--  put "Receiving "; put msg.mtype; put " on VC"; put msg.vc; 
--  put " at proc "; put p; put " with value "; put msg.val;
--  put " from "; put msg.src;
--  put " current state "; put Procs[p].state; put " requestor "; put msg.rqst;
  msg_processed := true;
  alias ps:Procs[p].state do
  alias pv:Procs[p].val do
  alias pc:Procs[p].cnt do
  switch ps
  case P_I:
    ErrorUnhandledMsg(msg, p);
  
  case P_ISd:
    switch msg.mtype
    case FwdGetS:
      msg_processed := false;
    case FwdGetM:
      msg_processed := false;
    case Inv:
      msg_processed := false;
    case ExData:
      ps := P_E;
      pv := msg.val;
    case Data:
      if(msg.src = HomeType) then
        if(msg.cnt = 0) then
          ps := P_S;
        endif;
      else
        ps := P_S;
      endif;
      pv := msg.val;
    else
      ErrorUnhandledMsg(msg, p);
    endswitch;

  case P_IMad:
    switch msg.mtype
    case FwdGetS:
      msg_processed := false;
    case FwdGetM:
      msg_processed := false;
    case Data:
      if(msg.src = HomeType) then
        pc := pc + msg.cnt;
        if(pc = 0) then
          ps := P_M;
        elsif(pc > 0) then
          ps := P_IMa;
        endif;
      else
        ps := P_M;
      endif;
      pv := msg.val;    
    case InvAck:
      pc := pc - 1;
    else
      ErrorUnhandledMsg(msg, p);
    endswitch;
    
  case P_IMa:
    switch msg.mtype
    case FwdGetS:
      msg_processed := false;
    case FwdGetM:
      msg_processed := false;
    case InvAck:
      pc := pc - 1;
      if(pc = 0) then
        ps := P_M;
      endif;
    else
      ErrorUnhandledMsg(msg, p);
    endswitch;

  case P_S:
    switch msg.mtype
    case Inv:
      ps := P_I;
      Send(InvAck, msg.src, p, VC2, UNDEFINED, 0);
      undefine pv;
    else
      ErrorUnhandledMsg(msg, p);
    endswitch;

  case P_SMad:
    switch msg.mtype
    case FwdGetS:
      msg_processed := false;
    case FwdGetM:
      msg_processed := false;
    case Inv:
      ps := P_IMad;
      Send(InvAck, msg.src, p, VC2, UNDEFINED, 0);
    case Data:
      assert(msg.src != HomeNode.owner) "* Data from owner";
      pc := pc + msg.cnt;
      if(pc = 0) then
        ps := P_M;
      elsif(pc > 0) then
        ps := P_SMa;
      endif;
      pv := msg.val;
    case InvAck:
      pc := pc - 1;
    else
      ErrorUnhandledMsg(msg, p);
    endswitch;

  case P_SMa:
    switch msg.mtype
    case FwdGetS:
      msg_processed := false;
    case FwdGetM:
      msg_processed := false;
    case InvAck:
      pc := pc - 1;
      if(pc = 0) then
        ps := P_M;
      endif;
    else
      ErrorUnhandledMsg(msg, p);
    endswitch;
  
  case P_M:
    Send(Dgd, HomeType, p, VC1, UNDEFINED, 0);          -- self-downgrade
    switch msg.mtype
    case DgdAck:
      ps := P_S;
      Send(Data, HomeType, p, VC2, pv, 0);
    case FwdGetS:
      ps := P_S;
      Send(Data, msg.src, p, VC2, pv, 0);
      Send(Data, HomeType, p, VC2, pv, 0);
    case FwdGetM:
      ps := P_I;
      Send(Data, msg.src, p, VC2, pv, 0);
      undefine pv;
    else
      ErrorUnhandledMsg(msg, p);
    endswitch;

  case P_E:
    Send(Dgd, HomeType, p, VC1, UNDEFINED, 0);          -- self-downgrade
    switch msg.mtype
    case DgdAck:
      ps := P_S;
      Send(Data, HomeType, p, VC2, pv, 0);
    case FwdGetS:
      ps := P_S;
      Send(Data, msg.src, p, VC2, pv, 0);
      Send(Data, HomeType, p, VC2, pv, 0);
    case FwdGetM:
      ps := P_I;
      Send(Data, msg.src, p, VC2, pv, 0);
      undefine pv;
    else
      ErrorUnhandledMsg(msg, p);
    endswitch;

  case P_MIa:
    switch msg.mtype
    case FwdGetS:
      ps := P_SIa;
      Send(Data, msg.src, p, VC2, pv, 0);
      Send(Data, HomeType, p, VC2, pv, 0);
    case FwdGetM:
      ps := P_IIa;
      Send(Data, msg.src, p, VC2, pv, 0);
    case PutAck:
      ps := P_I;
      undefine pv;
    else
      ErrorUnhandledMsg(msg, p);
    endswitch;
  
  case P_EIa:
    switch msg.mtype
    case FwdGetS:
      ps := P_SIa;
      Send(Data, msg.src, p, VC2, pv, 0);
      Send(Data, HomeType, p, VC2, pv, 0);
    case FwdGetM:
      ps := P_IIa;
      Send(Data, msg.src, p, VC2, pv, 0);
    case PutAck:
      ps := P_I;
      undefine pv;
    else
      ErrorUnhandledMsg(msg, p);
    endswitch;

  case P_SIa:
    switch msg.mtype
    case Inv:
      ps := P_IIa;
      Send(InvAck, msg.src, p, VC2, UNDEFINED, 0);
      undefine pv;
    case PutAck:
      ps := P_I;
      undefine pv;
    else
      ErrorUnhandledMsg(msg, p);
    endswitch;

  case P_IIa:
    switch msg.mtype
    case PutAck:
      ps := P_I;
      undefine pv;
    else
      ErrorUnhandledMsg(msg, p);
    endswitch;

  else
    ErrorUnhandledState();
  endswitch; 
  endalias;
  endalias;
  endalias;
-- debug output: 
--  put " final state "; put Procs[p].state; put "\n";
End;

----------------------------------------------------------------------
-- Rules                                                            --
----------------------------------------------------------------------

-- Cache controller actions ------------------------------------------

ruleset n:Proc Do
  alias ps:Procs[n].state do
  alias pv:Procs[n].val do

  ruleset v:Value Do
    rule "modified->store new value"
      (ps = P_M)
      ==>
        pv := v;
        LastWrite := v;
    endrule;

    rule "exclusive->store new value"
      (ps = P_E)
      ==>
        ps := P_M;
        pv := v;
        LastWrite := v;
    endrule;

  endruleset;

  rule "invalid->load"
    (ps = P_I)
    ==>
      ps := P_ISd;
      Send(GetS, HomeType, n, VC0, UNDEFINED, 0);
  endrule;
  
  rule "invalid->store"
    (ps = P_I)
    ==>
      ps := P_IMad;
      Send(GetM, HomeType, n, VC0, UNDEFINED, 0);
  endrule;

  rule "shared->store"
    (ps = P_S)
    ==>
      ps := P_SMad;
      Send(GetM, HomeType, n, VC0, UNDEFINED, 0);
  endrule;

  rule "shared->replacement"
    (ps = P_S)
    ==>
      ps := P_SIa;
      Send(PutS, HomeType, n, VC0, UNDEFINED, 0);
  endrule;

  rule "modified->replacement"
    (ps = P_M)
    ==>
      ps := P_MIa;
      Send(PutM, HomeType, n, VC0, pv, 0);
  endrule;

  rule "exclusive->replacement"
    (ps = P_E)
    ==>
      ps := P_EIa;
      Send(PutE, HomeType, n, VC2, UNDEFINED, 0);
  endrule;

  endalias;
  endalias;
endruleset;

-- Receive rule ------------------------------------------------------

ruleset n:Node Do
  alias vcblk:vc_blocking[n] do
  alias chan:Net[n] do
  choose midx:Net[n] do
    alias msg:chan[midx] do

    rule "receive"
      (!vcblk[msg.vc] |     
      -- receive new message when virtual channel is not blocked
      msg.old) &  
      -- otherwise only the inbox message can be processed      
      (msg.vc != VC0 |
      (MultiSetCount(m:chan, chan[m].vc = VC1 | chan[m].vc = VC2 | chan[m].vc = VC3) = 0))
      -- priority rule: 
      -- the message from virtual channel #0 will not be processed until other channels 
      -- are empty
      ==>
        vcblk[msg.vc] := true;           -- block the channel
        msg.old := true;                 -- mark as inbox message
        if IsMember(n, Home) then
            HomeReceive(msg);
        else
            ProcReceive(msg, n);
        endif;

        -- channel will not be freed until the inbox message is processed
        if msg_processed then
            vcblk[msg.vc] := false;          -- free the channel
            MultiSetRemove(midx, chan);      -- destroy the message  
        endif; 

    endrule;  
    endalias;
  endchoose;
  endalias;
  endalias;
endruleset;

-- Alternative receive rule ------------------------------------------
/*
ruleset n:Node Do
  alias vcs:Vcs[n] do
  alias chan:Net[n] do
  choose midx:Net[n] do
    alias msg:chan[midx] do

    rule "receive"
      (isundefined(vcs[msg.vc].cnt)) & 
      ((msg.vc != VC0) |
      (MultiSetCount(m:chan, chan[m].vc = VC1 | chan[m].vc = VC2 | chan[m].vc = VC3) = 0 &
      isundefined(vcs[VC3].cnt) & isundefined(vcs[VC2].cnt) & isundefined(vcs[VC1].cnt)))
      ==>
        if IsMember(n, Home) then
            HomeReceive(msg);
		  	if !msg_processed then
	  			vcs[msg.vc] := msg;
			endif;
        else
            ProcReceive(msg, n);
			if !msg_processed then
	  			vcs[msg.vc] := msg;
			endif; 
        endif;
        MultiSetRemove(midx, chan);
    endrule;  
    endalias;
  endchoose;

  ruleset c:VCType Do
    rule "receive vc"
      (!isundefined(vcs[c].cnt))
      ==>
        if IsMember(n, Home) then
            HomeReceive(vcs[c]);
		  	if msg_processed then
	  			undefine vcs[c];
			endif;
        else
            ProcReceive(vcs[c], n);
			if msg_processed then
	  			undefine vcs[c];
			endif; 
        endif;
    endrule;
  endruleset;
  endalias;
  endalias;
endruleset;
*/

----------------------------------------------------------------------
-- Startstate                                                       --
----------------------------------------------------------------------
startstate
    put "\n==========================================================================";
    put "\nFour-state 3-hop MESI protocol with self-downgrade";
    put "\n";
    put "\nEditor : Weixuan Yang";
    put "\nEmail  : endreim@outlook.com";
    put "\n";
    put "\nLast modified date: March 07, 2022";
    put "\n==========================================================================";
    put "\n";

	for v:Value do
  -- home node initialization
    HomeNode.state := H_I;
    ClearOwner();
    ClearSharer();
    HomeNode.val := v;
	endfor;
  LastWrite := HomeNode.val;
  
  -- processor initialization
  for i:Proc do
    Procs[i].cnt := 0;
    Procs[i].state := P_I;
    undefine Procs[i].val;
  endfor;

  -- virtual channel initialization
  for n:Node do
    for vc:VCType do
      vc_blocking[n][vc] := false;
    endfor;
  endfor;

  -- network initialization
  undefine Net;
endstartstate;

----------------------------------------------------------------------
-- Invariants                                                       --
----------------------------------------------------------------------

-- Coherency assertions ----------------------------------------------

invariant "value in memory matches value of last write, when invalid"
  HomeNode.state = H_I 
    ->
			HomeNode.val = LastWrite;
	
invariant "value is undefined while invalid"
  Forall n : Proc Do	
     Procs[n].state = P_I
    ->
			IsUndefined(Procs[n].val)
	end;

invariant "value in memory matches value of last write, when shared"
  HomeNode.state = H_S
    ->
			HomeNode.val = LastWrite;

invariant "values in cache match value of last write, when shared"
  Forall n : Proc Do	
     Procs[n].state = P_S
    ->
			Procs[n].val = LastWrite
	end;

invariant "values in cache match value of last write, when smad"
  Forall n : Proc Do	
     Procs[n].state = P_SMad
    ->
			Procs[n].val = LastWrite
	end;

invariant "values in cache match value of last write, when sma"
  Forall n : Proc Do	
     Procs[n].state = P_SMa
    ->
			Procs[n].val = LastWrite
	end;

invariant "values in cache match value of last write, when modified"
  Forall n : Proc Do	
     Procs[n].state = P_M
    ->
			Procs[n].val = LastWrite
	end;

invariant "values in cache match value of last write, when exclusive"
  Forall n : Proc Do	
     Procs[n].state = P_E
    ->
			Procs[n].val = LastWrite
	end;

-- Correctness assertions --------------------------------------------

invariant "invalid implies empty sharer"
  HomeNode.state = H_I
    ->
			MultiSetCount(i:HomeNode.sharers, true) = 0;

invariant "invalid implies empty owner"
  HomeNode.state = H_I
    ->
      IsUndefined(HomeNode.owner);

invariant "shared implies not empty sharer"
  HomeNode.state = H_S
    ->
			MultiSetCount(i:HomeNode.sharers, true) > 0;

invariant "modified implies empty sharer"
  HomeNode.state = H_M
    ->
			MultiSetCount(i:HomeNode.sharers, true) = 0;

invariant "exclusive implies empty sharer"
  HomeNode.state = H_E
    ->
			MultiSetCount(i:HomeNode.sharers, true) = 0;
