function randNFA(min, max, tra,alph) {
  ran = Math.floor(Math.random() * (max-min));
  tra2 = 2 + Math.floor(Math.random() * (tra-2));
  alph2 = 2 + Math.floor(Math.random() * (alph-2));
  num = ran+min;
  
  var thisAutomaton = noam.fsm.createRandomFsm(noam.fsm.nfaType, num, alph2, tra2);

  statesOut = "[";
  for (var i = 0;i<thisAutomaton.states.length;i++){
    statesOut = statesOut + thisAutomaton.states[i].substring(1) + ",";
  }
  statesOut = statesOut.substring(0,statesOut.length-1) + "] ";

  alphOut = "[";
  for (var i=0;i<thisAutomaton.alphabet.length;i++){
    alphOut = alphOut + "'" + thisAutomaton.alphabet[i] + "',";
  }
  alphOut = alphOut.substring(0,alphOut.length-1) + "] ";

  transOut = "[";
  for (var i=0;i<thisAutomaton.transitions.length;i++){
    for(var j=0;j<thisAutomaton.transitions[i].toStates.length;j++){
      transOut = transOut + "('" + thisAutomaton.transitions[i].symbol +"',("+thisAutomaton.transitions[i].fromState.substring(1)+","+thisAutomaton.transitions[i].toStates[j].substring(1)+")),"
    }
  }
  transOut = transOut.substring(0,transOut.length-1) + "] ";
  
  initState = thisAutomaton.initialState.substring(1) + " ";

  finStates = "[";
  for(var i=0;i<thisAutomaton.acceptingStates.length;i++){
    finStates = finStates + thisAutomaton.acceptingStates[i].substring(1) + ",";
  }
  finStates = finStates.substring(0,finStates.length-1) + "]";

  //return thisAutomaton;
  return ("NFAe " + statesOut + alphOut + transOut + "[] " + initState + finStates );
}

function createNRands(min,max,tra,alph,n){
  output = "";
  for (var i=0;i<n;i++) {
     output = output + "Test" + (i+1) + " = " + randNFA(min, max, tra,alph) + "\n \n";
  }
  return output;
}

function randNFAPlusRegex(min, max, tra,alph) {
  ran = Math.floor(Math.random() * (max-min));
  num = ran+min;
  
  var thisAutomaton = noam.fsm.createRandomFsm(noam.fsm.nfaType, num, alph, tra);
  
  statesOut = "[";
  for (var i = 0;i<thisAutomaton.states.length;i++){
    statesOut = statesOut + thisAutomaton.states[i].substring(1) + ",";
  }
  statesOut = statesOut.substring(0,statesOut.length-1) + "] ";

  alphOut = "[";
  for (var i=0;i<thisAutomaton.alphabet.length;i++){
    alphOut = alphOut + "'" + thisAutomaton.alphabet[i] + "',";
  }
  alphOut = alphOut.substring(0,alphOut.length-1) + "] ";

  transOut = "[";
  for (var i=0;i<thisAutomaton.transitions.length;i++){
    for(var j=0;j<thisAutomaton.transitions[i].toStates.length;j++){
      transOut = transOut + "('" + thisAutomaton.transitions[i].symbol +"',("+thisAutomaton.transitions[i].fromState.substring(1)+","+thisAutomaton.transitions[i].toStates[j].substring(1)+")),"
    }
  }
  transOut = transOut.substring(0,transOut.length-1) + "] ";
  
  initState = thisAutomaton.initialState.substring(1) + " ";

  finStates = "[";
  for(var i=0;i<thisAutomaton.acceptingStates.length;i++){
    finStates = finStates + thisAutomaton.acceptingStates[i].substring(1) + ",";
  }
  finStates = finStates.substring(0,finStates.length-1) + "]";

  var r = noam.fsm.toRegex(thisAutomaton);
  r = noam.re.tree.simplify(r, {"useFsmPatterns": false});
  var s = noam.re.tree.toString(r);
  //return thisAutomaton;
  return [("NFAe " + statesOut + alphOut + transOut + "[] " + initState + finStates ),s];
}
