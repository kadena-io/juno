import React from 'react';

export default class NostroDetail extends React.Component {

  render() {

    var rows = [];
    const nostros = [this.props.tokyoNostro,this.props.londonNostro];
    if (this.props.model != null) {
      rows = this.props.model
        .filter(r => nostros.includes(r.from) && nostros.includes(r.to))
        .map(r => {
          const [londonAmt,tokyoAmt] = r.from == this.props.londonNostro ?
                [r.amount * -1,r.amount] : [r.amount,r.amount * -1];
          return (<tr><td>1/14/2016</td><td>{r.transId}</td>
                  <td className="currency">{prettyMoneyPrint(londonAmt)}</td>
                  <td className="currency">{prettyMoneyPrint(tokyoAmt)}</td></tr>);
        });
    }
    const ptitle="Nostro Accounts";
    return (<div className="panel-body">
                <table style={{ width: '100%', tableLayout: 'fixed'}} className="table table-striped">
                <thead>
                <tr>
                <td>Date</td><td>Txn ID</td><td className="currency">London Nostro USD</td><td className="currency">Tokyo Nostro USD</td>
                </tr></thead>
                <tbody>
                {rows}
                </tbody>
                </table>
			  </div>

            );
  }

}

function prettyMoneyPrint(val) {
  if (val) {
	var sign = '';

	if (val < 0) {
	  sign = '-';
	}

	return sign + Math.abs(val).toFixed(2).replace(/\d(?=(\d{3})+\.)/g, '$&,');
  }
}
