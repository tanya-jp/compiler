
# parsetab.py
# This file is automatically generated. Do not edit.
# pylint: disable=W,C,R
_tabversion = '3.10'

_lr_method = 'LALR'

_lr_signature = 'expressionrightORrightANDrightNOTrightPLUSMINUSrightTIMESDIVIDEnonassocMODrightUMINUSAND ASSIGN BEGIN COLON COMMA DEFAULT DIVIDE DO DONE ELSE END EQ FALSE GT GTEQ ID IF INT INTEGERCONSTANT LPAREN LT LTEQ MINUS MOD NEQ NOT OF OR PLUS PRINT PROGRAM REAL REALCONSTANT RPAREN SEMICOLON SWITCH THEN TIMES TRUE UMINUS VAR WHILEmarker : expression : expression OR marker expressionexpression : expression AND marker expressionexpression : NOT expressionexpression : LPAREN expression RPARENexpression : TRUEexpression : FALSEexpression : INTEGERCONSTANTexpression : REALCONSTANTexpression : IDexpression : UMINUS expressionexpression : expression PLUS expressionexpression : expression MINUS expressionexpression : expression TIMES expressionexpression : expression DIVIDE expressionexpression : expression MOD expressionexpression : expression LT expressionexpression : expression LTEQ expressionexpression : expression GT expressionexpression : expression GTEQ expressionexpression : expression NEQ expressionexpression : expression EQ expression'
    
_lr_action_items = {'NOT':([0,2,3,9,10,11,12,13,14,15,16,17,18,19,20,21,22,26,27,],[2,2,2,2,-1,-1,2,2,2,2,2,2,2,2,2,2,2,2,2,]),'LPAREN':([0,2,3,9,10,11,12,13,14,15,16,17,18,19,20,21,22,26,27,],[3,3,3,3,-1,-1,3,3,3,3,3,3,3,3,3,3,3,3,3,]),'TRUE':([0,2,3,9,10,11,12,13,14,15,16,17,18,19,20,21,22,26,27,],[4,4,4,4,-1,-1,4,4,4,4,4,4,4,4,4,4,4,4,4,]),'FALSE':([0,2,3,9,10,11,12,13,14,15,16,17,18,19,20,21,22,26,27,],[5,5,5,5,-1,-1,5,5,5,5,5,5,5,5,5,5,5,5,5,]),'INTEGERCONSTANT':([0,2,3,9,10,11,12,13,14,15,16,17,18,19,20,21,22,26,27,],[6,6,6,6,-1,-1,6,6,6,6,6,6,6,6,6,6,6,6,6,]),'REALCONSTANT':([0,2,3,9,10,11,12,13,14,15,16,17,18,19,20,21,22,26,27,],[7,7,7,7,-1,-1,7,7,7,7,7,7,7,7,7,7,7,7,7,]),'ID':([0,2,3,9,10,11,12,13,14,15,16,17,18,19,20,21,22,26,27,],[8,8,8,8,-1,-1,8,8,8,8,8,8,8,8,8,8,8,8,8,]),'UMINUS':([0,2,3,9,10,11,12,13,14,15,16,17,18,19,20,21,22,26,27,],[9,9,9,9,-1,-1,9,9,9,9,9,9,9,9,9,9,9,9,9,]),'$end':([1,4,5,6,7,8,23,25,28,29,30,31,32,33,34,35,36,37,38,39,40,41,],[0,-6,-7,-8,-9,-10,-4,-11,-12,-13,-14,-15,-16,-17,-18,-19,-20,-21,-22,-5,-2,-3,]),'OR':([1,4,5,6,7,8,23,24,25,28,29,30,31,32,33,34,35,36,37,38,39,40,41,],[10,-6,-7,-8,-9,-10,-4,10,-11,-12,-13,-14,-15,-16,10,10,10,10,10,10,-5,10,-3,]),'AND':([1,4,5,6,7,8,23,24,25,28,29,30,31,32,33,34,35,36,37,38,39,40,41,],[11,-6,-7,-8,-9,-10,-4,11,-11,-12,-13,-14,-15,-16,11,11,11,11,11,11,-5,11,11,]),'PLUS':([1,4,5,6,7,8,23,24,25,28,29,30,31,32,33,34,35,36,37,38,39,40,41,],[12,-6,-7,-8,-9,-10,12,12,-11,12,12,-14,-15,-16,12,12,12,12,12,12,-5,12,12,]),'MINUS':([1,4,5,6,7,8,23,24,25,28,29,30,31,32,33,34,35,36,37,38,39,40,41,],[13,-6,-7,-8,-9,-10,13,13,-11,13,13,-14,-15,-16,13,13,13,13,13,13,-5,13,13,]),'TIMES':([1,4,5,6,7,8,23,24,25,28,29,30,31,32,33,34,35,36,37,38,39,40,41,],[14,-6,-7,-8,-9,-10,14,14,-11,14,14,14,14,-16,14,14,14,14,14,14,-5,14,14,]),'DIVIDE':([1,4,5,6,7,8,23,24,25,28,29,30,31,32,33,34,35,36,37,38,39,40,41,],[15,-6,-7,-8,-9,-10,15,15,-11,15,15,15,15,-16,15,15,15,15,15,15,-5,15,15,]),'MOD':([1,4,5,6,7,8,23,24,25,28,29,30,31,32,33,34,35,36,37,38,39,40,41,],[16,-6,-7,-8,-9,-10,16,16,-11,16,16,16,16,None,16,16,16,16,16,16,-5,16,16,]),'LT':([1,4,5,6,7,8,23,24,25,28,29,30,31,32,33,34,35,36,37,38,39,40,41,],[17,-6,-7,-8,-9,-10,-4,17,-11,-12,-13,-14,-15,-16,17,17,17,17,17,17,-5,-2,-3,]),'LTEQ':([1,4,5,6,7,8,23,24,25,28,29,30,31,32,33,34,35,36,37,38,39,40,41,],[18,-6,-7,-8,-9,-10,-4,18,-11,-12,-13,-14,-15,-16,18,18,18,18,18,18,-5,-2,-3,]),'GT':([1,4,5,6,7,8,23,24,25,28,29,30,31,32,33,34,35,36,37,38,39,40,41,],[19,-6,-7,-8,-9,-10,-4,19,-11,-12,-13,-14,-15,-16,19,19,19,19,19,19,-5,-2,-3,]),'GTEQ':([1,4,5,6,7,8,23,24,25,28,29,30,31,32,33,34,35,36,37,38,39,40,41,],[20,-6,-7,-8,-9,-10,-4,20,-11,-12,-13,-14,-15,-16,20,20,20,20,20,20,-5,-2,-3,]),'NEQ':([1,4,5,6,7,8,23,24,25,28,29,30,31,32,33,34,35,36,37,38,39,40,41,],[21,-6,-7,-8,-9,-10,-4,21,-11,-12,-13,-14,-15,-16,21,21,21,21,21,21,-5,-2,-3,]),'EQ':([1,4,5,6,7,8,23,24,25,28,29,30,31,32,33,34,35,36,37,38,39,40,41,],[22,-6,-7,-8,-9,-10,-4,22,-11,-12,-13,-14,-15,-16,22,22,22,22,22,22,-5,-2,-3,]),'RPAREN':([4,5,6,7,8,23,24,25,28,29,30,31,32,33,34,35,36,37,38,39,40,41,],[-6,-7,-8,-9,-10,-4,39,-11,-12,-13,-14,-15,-16,-17,-18,-19,-20,-21,-22,-5,-2,-3,]),}

_lr_action = {}
for _k, _v in _lr_action_items.items():
   for _x,_y in zip(_v[0],_v[1]):
      if not _x in _lr_action:  _lr_action[_x] = {}
      _lr_action[_x][_k] = _y
del _lr_action_items

_lr_goto_items = {'expression':([0,2,3,9,12,13,14,15,16,17,18,19,20,21,22,26,27,],[1,23,24,25,28,29,30,31,32,33,34,35,36,37,38,40,41,]),'marker':([10,11,],[26,27,]),}

_lr_goto = {}
for _k, _v in _lr_goto_items.items():
   for _x, _y in zip(_v[0], _v[1]):
       if not _x in _lr_goto: _lr_goto[_x] = {}
       _lr_goto[_x][_k] = _y
del _lr_goto_items
_lr_productions = [
  ("S' -> expression","S'",1,None,None,None),
  ('marker -> <empty>','marker',0,'p_marker','Project.py',144),
  ('expression -> expression OR marker expression','expression',4,'p_expression_or','Project.py',153),
  ('expression -> expression AND marker expression','expression',4,'p_expression_and','Project.py',160),
  ('expression -> NOT expression','expression',2,'p_expression_unot','Project.py',167),
  ('expression -> LPAREN expression RPAREN','expression',3,'p_expression_group','Project.py',171),
  ('expression -> TRUE','expression',1,'p_expression_true','Project.py',176),
  ('expression -> FALSE','expression',1,'p_expression_false','Project.py',181),
  ('expression -> INTEGERCONSTANT','expression',1,'p_expression_integer','Project.py',186),
  ('expression -> REALCONSTANT','expression',1,'p_expression_real','Project.py',190),
  ('expression -> ID','expression',1,'p_expression_id','Project.py',194),
  ('expression -> UMINUS expression','expression',2,'p_expression_uminus','Project.py',199),
  ('expression -> expression PLUS expression','expression',3,'p_expression_plus','Project.py',203),
  ('expression -> expression MINUS expression','expression',3,'p_expression_minus','Project.py',207),
  ('expression -> expression TIMES expression','expression',3,'p_expression_times','Project.py',211),
  ('expression -> expression DIVIDE expression','expression',3,'p_expression_divide','Project.py',215),
  ('expression -> expression MOD expression','expression',3,'p_expression_mod','Project.py',219),
  ('expression -> expression LT expression','expression',3,'p_expression_less','Project.py',223),
  ('expression -> expression LTEQ expression','expression',3,'p_expression_lessequal','Project.py',228),
  ('expression -> expression GT expression','expression',3,'p_expression_greater','Project.py',233),
  ('expression -> expression GTEQ expression','expression',3,'p_expression_greaterqual','Project.py',238),
  ('expression -> expression NEQ expression','expression',3,'p_expression_notqual','Project.py',243),
  ('expression -> expression EQ expression','expression',3,'p_expression_qual','Project.py',248),
]
