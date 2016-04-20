import React from 'react';

export default function BranchDetail(props) {

  var rows = [];
  if (props.branchData != null && props.branchData[props.branch] != null) {
    rows = props.branchData[props.branch].trans
        .filter(r => props.branchAccts.includes(r.from) || props.branchAccts.includes(r.to))
        .map(r => {
          var [credit,debit,account] = r.to == props.branch ?
              [null,r.amount,r.from] :
              [r.amount,null,r.to];
          var accountName = props.accounts[account];
          return (
            <tr>
         <td>1/14/2016</td>
         <td><a href="#" className="txn-detail">{ r.transId }</a></td>
         <td>{ accountName }</td>
         <td>{ account }</td>
         <td>USD</td>
         <td className="currency">{ prettyMoneyPrint(debit) }</td>
         <td className="currency">{ prettyMoneyPrint(credit) }</td>
         </tr>);
        });
  }
  return (<div><table style={{ width: '100%', tableLayout: 'fixed'}} className="table table-striped">
          <thead>
          <tr><td>Date</td><td>Txn ID</td><td>Account Name</td><td>Account Number</td>
                <td>Currency</td><td className="currency">Debit</td><td className="currency">Credit</td>
          </tr></thead>
          <tbody>
          {rows}
          </tbody>
          </table></div>);
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
