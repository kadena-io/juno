import React from 'react';
import R from 'ramda';
import { prettyMoneyPrint,detailPane } from './util.js';
import SwiftDetail from './swift-detail.js';

export default function BranchDetail(props) {

  var rows = [];
  if (props.branchData != null && props.branchData[props.branch] != null) {
    rows = R.pipe
    (R.filter(r => props.branchAccts.includes(r.from) || props.branchAccts.includes(r.to)),
     R.sortBy(R.prop("transId")),
     R.map(r => {
          var [credit,debit,account] = r.to == props.branch ?
              [null,r.amount,r.from] :
              [r.amount,null,r.to];
          var accountName = props.acctInfo[account]["name"];
          return (
            <tr>
         <td>1/14/2016</td>
              <td><a href="#" onClick={()=>props.fetchTx(r.transId,props.branch)} className="txn-detail">{ r.transId }</a></td>
         <td>{ accountName }</td>
         <td>{ account }</td>
         <td>USD</td>
         <td className="currency">{ prettyMoneyPrint(debit) }</td>
         <td className="currency">{ prettyMoneyPrint(credit) }</td>
         </tr>);
     }))(props.branchData[props.branch].trans);
  }

  const deets=detailPane
  (`Branch Statement [${props.branch}]`,
   (<div className="panel-body"><table style={{ width: '100%', tableLayout: 'fixed'}} className="table table-striped">
          <thead>
          <tr><td>Date</td><td>Txn ID</td><td>Account Name</td><td>Account Number</td>
                <td>Currency</td><td className="currency">Debit</td><td className="currency">Credit</td>
          </tr></thead>
          <tbody>
          {rows}
          </tbody>
    </table></div>));
  return (<div>
          {deets}
          <SwiftDetail {...props}/>
          </div>)
}
