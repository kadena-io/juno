import React from 'react';
import R from 'ramda';
import { prettyMoneyPrint } from './util.js';

export default class NostroDetail extends React.Component {

  render() {

    var rows = [];
    const nostros = [this.props.tokyoNostro,this.props.londonNostro];
    if (this.props.nostroData != null) {
      rows = R.pipe
      (R.filter(r => nostros.includes(r.from) && nostros.includes(r.to)),
       R.sortBy(R.prop("transId")),
       R.map(r => {
          const [londonAmt,tokyoAmt] = r.from == this.props.londonNostro ?
                [r.amount * -1,r.amount] : [r.amount,r.amount * -1];
          return (<tr><td>1/14/2016</td><td>{r.transId}</td>
                  <td className="currency">{prettyMoneyPrint(londonAmt)}</td>
                  <td className="currency">{prettyMoneyPrint(tokyoAmt)}</td></tr>);
       }))(this.props.nostroData);
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
