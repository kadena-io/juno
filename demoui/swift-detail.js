import React from 'react';
import R from 'ramda';
import { prettyMoneyPrint,detailPane } from './util.js';

export default function SwiftDetail(props) {
  if (props.txData == null || props.txData[props.branch] == null) {
    return (<div></div>);
  }
  const data = props.txData[props.branch];
  const trans = data["trans"];

  var rows = [];
  trans.map(o => {
    rows.push(
    (<tr>
     <td>{ props.acctInfo[o.from]["type"] }</td>
     <td>{ props.acctInfo[o.from]["name"] }</td>
     <td>{ o.from }</td>
     <td>{ prettyMoneyPrint(o.amount) }</td>
     <td></td>
     </tr>));
    rows.push(
    (<tr>
     <td>{ props.acctInfo[o.to]["type"] }</td>
     <td>{ props.acctInfo[o.to]["name"] }</td>
     <td>{ o.to }</td>
     <td></td>
     <td>{ prettyMoneyPrint(o.amount) }</td>
        </tr>));
  });

  var txDeets = null;
  var panelTitle = null;
  const swifts = data["swifts"];
  var txId = Object.keys(swifts)[0];
  if (txId != null) {
    const sw = swifts[txId];
    panelTitle = `Swift Details [${txId}]`;
    txDeets =
      (<div>
    <div style={{ width: '50%', float: 'left', paddingRight: '10px'}}>
    <table style={{width: '100%', tableLayout: 'fixed'}} className="table table-striped"><tbody>
    <tr><td>Senders Reference: { sw.ref }</td></tr>
    <tr><td>Bank Operation Code: { sw.opCode }</td></tr>
    <tr><td>Value Date: 20{ sw.valueDate }</td></tr>
    <tr><td>Settled Amount: { prettyMoneyPrint(sw.settled) }</td></tr>
    <tr><td>Details of Charges: { sw.details }</td></tr>
    </tbody></table>
    </div>
    <div style={{width: '50%', float: 'right', paddingLeft: '10px'}}>
    <table style={{width: '100%', tableLayout: 'fixed'}} className="table table-striped"><tbody>
    <tr><td>Ordering Account: { sw.orderingAcct }</td></tr>
    <tr><td>Ordering Customer:<br/>{ sw.orderingAcctDescription }</td></tr>
    <tr><td>Beneficiary Account: { sw.beneficiaryAcct }</td></tr>
    <tr><td>Beneficiary Customer:<br/>{ sw.beneficiaryAcctDescription }</td></tr>
    </tbody></table>
    </div>
       </div>);
  } else { //not a SWIFT tx (txId == null)
    const inputs = data["inputs"];
    txId = Object.keys(inputs)[0];
    const inp = inputs[txId];
    panelTitle = `Message Body [${txId}]`;
    txDeets =
      (<div style={{ width: '100%', float: 'left', paddingRight: '10px'}}>
       <pre>{ inp }</pre>
       </div>);
  }
  return detailPane
  (panelTitle,
   (<div className="panel-body">
    { txDeets }
    <br/>
    <h3 style={{clear: 'both'}}>Transaction History</h3>
    <table style={{width: '100%', tableLayout: 'fixed'}} className="table table-striped">
    <thead><tr><td>Account Type</td><td>Account Name</td><td>Account Number</td>
    <td>Debit</td><td>Credit</td>
    </tr></thead>
    <tbody>{ rows }</tbody>
    </table>
    </div>
   ));
}
