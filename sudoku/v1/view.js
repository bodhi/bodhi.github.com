
var Option = React.createClass({
    render: function() {
        var className = "option";
        if (this.props.digit == this.props.option.value) {
            className += " chosen";
        } else {
            if (this.props.option.possible.indexOf(this.props.digit) == -1) className += " excluded";
        }
        if (!!this.props.option.deleted && this.props.option.deleted.indexOf(this.props.digit) != -1) className += " deleted";
        return (
            <div className={className}>
            {this.props.digit}
            </div>
        );
    }
});

var Cell = React.createClass({
    render: function() {
        var options = [];
        for (var i = 1; i <= 9; ++i) {
            options.push(<Option digit={i} option={this.props.cell} />);
        }

        return (
            <div className="cell">
            {options}
            </div>
        );
    }
});

var Block = React.createClass({
    render: function() {
        var cells = [];
        for (var i = 0; i < 9; ++i) {
            cells.push(<Cell cell={this.props.block[i]} />);
        }

        return (
            <div className="block">
            {cells}
            </div>
        );
    }
});


var Game = React.createClass({
    render: function() {
        var blockGame = toRows(this.props.game, 3, 9);
        var blocks = [];
        for (var i = 0; i < 81; i += 9) {
            blocks.push(<Block block={blockGame.slice(i, i + 9)} />);
        }
        return (
            <div className="game">
            {blocks}
            </div>
        );
    }
});

function render(game) {
     React.render(
     <Game game={game} />,
     document.getElementById("game")
     );
};
