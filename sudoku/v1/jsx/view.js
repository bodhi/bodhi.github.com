
var Option = React.createClass({displayName: 'Option',
    render: function() {
        var className = "option";
        if (this.props.digit == this.props.option.value) {
            className += " chosen";
        } else {
            if (this.props.option.possible.indexOf(this.props.digit) == -1) className += " excluded";
        }
        if (!!this.props.option.deleted && this.props.option.deleted.indexOf(this.props.digit) != -1) className += " deleted";
        return (
            React.createElement("div", {className: className}, 
            this.props.digit
            )
        );
    }
});

Cell = React.createClass({displayName: 'Cell',
    render: function() {
        var options = [];
        for (var i = 1; i <= 9; ++i) {
            options.push(React.createElement(Option, {digit: i, option: this.props.cell}));
        }

        return (
            React.createElement("div", {className: "cell"}, 
            options
            )
        );
    }
});

var Block = React.createClass({displayName: 'Block',
    render: function() {
        var cells = [];
        for (var i = 0; i < 9; ++i) {
            cells.push(React.createElement(Cell, {cell: this.props.block[i]}));
        }

        return (
            React.createElement("div", {className: "block"}, 
            cells
            )
        );
    }
});


var Game = React.createClass({displayName: 'Game',
    render: function() {
        var blockGame = toRows(this.props.game, 3, 9);
        var blocks = [];
        for (var i = 0; i < 81; i += 9) {
            blocks.push(React.createElement(Block, {block: blockGame.slice(i, i + 9)}));
        }
        return (
            React.createElement("div", {className: "game"}, 
            blocks
            )
        );
    }
});

function render(game) {
     React.render(
     React.createElement(Game, {game: game}),
     document.getElementById("game")
     );
};
