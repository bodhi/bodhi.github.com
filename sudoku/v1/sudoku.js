"use strict";

function cellsWithValue(game) {
     log("Set cells with 1 possiblity");
     return game.map(function(cell) {
             var newCell = { value: cell.value, possible: cell.possible, deleted: [] }
             if (!!cell.value) {
                  newCell.possible = [];
             } else if (cell.possible.length == 1) {
                  newCell.value = cell.possible[0];
                  newCell.deleted = cell.possible;
                  newCell.possible = [];
             }
             return newCell;
     });
}

function reduceBlock(game) {
     return game.map(function(block) {
         var values = block.map(function(cell) {
              return cell.value;
         }).filter(function(val) { return !!val });

         return block.map(function(cell) {
             var possible = !!cell.value ? [] : cell.possible.filter(function(val) { return values.indexOf(val) == -1  });
var deleted = cell.possible.filter(function(val) { return values.indexOf(val) != -1  && val != cell.value });
             return {
                value: cell.value,
                possible: possible,
                deleted: deleted,
             };
         });
     });
}

function transpose(game) {
     var newGame = [];
     for (var i = 0; i < game.length; ++i) {
       newGame[i] = [];
       for (var j = 0; j < game[i].length; ++j) {
         newGame[i][j] = game[j][i];
       }
     }
     return newGame;
}

function log(action) {
   document.getElementById("log").textContent += action + "\n";
}

function reduceRows(game, type) {
  var newGame = inGroupsOf(game, 9);
  var reduced = reduceBlock(newGame);
  log("Eliminate by " + (type || "row"));
  return flatten(reduced);
}

function reduceColumns(game) {
  var newGame = transpose(inGroupsOf(game, 9));
  var reduced = reduceBlock(newGame);
  log("Eliminate by column");
  return flatten(transpose(reduced));
}

function reduceBlocks(game) {
  var newGame = toRows(game, 3, 9);
  var reduced = reduceRows(newGame, "block");
  return toRows(reduced, 3, 9);
}

function flatten(array) {
     return array.reduce(function(memo, val) {
       if (Array.isArray(val)) {
         return memo.concat(flatten(val));
       } else {
         memo.push(val);
         return memo;
       }
     }, []);
}

function inGroupsOf(arr, n) {
     return arr.reduce(function(memo, val, i) {
         if (i % n == 0) memo.push([]);
         memo[memo.length - 1].push(val);
         return memo;
     }, []);
}

function toRows(blocks, blockLength, rowLength) {
     var blockRows = inGroupsOf(blocks, blockLength * rowLength);
     var linearRows = blockRows.map(function(blockRow) {
       return flatten(transpose(inGroupsOf(inGroupsOf(inGroupsOf(blockRow, blockLength), rowLength)[0], blockLength)));
     });
     return flatten(linearRows);
}

function infer(rows) {
     var newRows = rows.map(function(row) {
     return row.map(function(cell) {
        var newCell = {
           value: cell.value,
           possible: cell.possible,
           deleted: []
        }
        if (!!cell.value) return newCell;

        var onlyHere = cell.possible.reduce(function(memo, val) {
          if (row.every(function(otherCell) {
              return (otherCell.value != val && otherCell.possible.indexOf(val) == -1) || otherCell === cell
            })) {
            memo.push(val);
          }
          return memo;
        }, []);

        if (onlyHere.length > 1) {
                         debugger;
           throw(onlyHere);
        } else if (onlyHere.length == 1) {
           return {
             value: onlyHere[0],
             possible: [],
             deleted: cell.possible
           }
        } else {
           return newCell;
        }
     });
     });
return newRows;
}

function inferRows(game) {
     log("Infer by row");
     var rows = inGroupsOf(game, 9);
     return flatten(infer(rows));
}


function inferColumns(game) {
     log("Infer by column");
     var rows = inGroupsOf(game, 9);
     return flatten(transpose(infer(transpose(rows))));
}

function inferBlocks(game) {
     log("Infer by block");
     var rows = inGroupsOf(toRows(game, 3, 9), 9);
     return toRows(flatten(infer(rows)), 3, 9);
}

var data = [
     [
     3, null, null,
     null, 5, null,
     null, null, 9
     ],
     [
     null, 2, null,
     null, 4, null,
     null, 6, null
     ],
     [
     null, null, 1,
     null, null, 8,
     null, null, 5
     ],
     [
     1, null, null,
     null, null, null,
     null, 7, null
     ],
     [
     null, null, null,
     null, null, null,
     null, null, null
     ],
     [
     null, null, null,
     null, null, null,
     null, null, null
     ],
     [
     null, null, null,
     null, null, null,
     null, null, null
     ],
     [
     null, null, null,
     null, null, null,
     null, null, null
     ],
     [
     null, null, null,
     null, null, null,
     null, null, null
     ]
     ]

var dataStr = "\
1..73....\
..42....7\
8...5...9\
.5...8...\
..7...38.\
3...9.4..\
.61.....2\
...5.....\
53....6..\
";

var dataStr2 = "\
8.5...4..\
9..18...2\
.3....8..\
...87...6\
.7.....1.\
6...13...\
..9....2.\
7...62..9\
..2...3.7\
";

var data2 = dataStr.split("").map(function(i) { return i == "." ? null : parseInt(i, 10) })

var converter = function(cell) {
         return {
             value: cell,
             possible: [1,2,3,4,5,6,7,8,9]
     }
}

var game1 = flatten(data).map(converter);
var game2 = data2.map(converter);

var game = game2;

                         var actions = [cellsWithValue, reduceColumns, reduceRows, reduceBlocks, inferRows, inferColumns, inferBlocks];
var actionId = 0;

var games = [game];

var noOpId = -1;

function step() {
     game = actions[actionId](games[games.length - 1]);
     games.push(game);
     actionId = (actionId + 1) % actions.length;

     if (false && flatten(game.map(function(cell) { return cell.deleted; })).length > 0) {
         step();
     }

     if (flatten(game.map(function(cell) { return cell.deleted; })).length == 0) {
        noOpId += 1;
     } else {
        noOpId = -1;
     }

     if (noOpId == actions.length) {
       log("Stuck!")
       document.getElementById("step").disabled = true;
     } else {
       step();
     }

     render(game);
}

function rewind() {
    log("Step back");
    games.pop();
    actionId = (actionId + actions.length - 1) % actions.length;
    render(games[games.length - 1]);

    noOpId = -1;

    document.getElementById("step").disabled = false;
}

render(game);
