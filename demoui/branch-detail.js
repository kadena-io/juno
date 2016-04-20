import React from 'react';

export default function BranchDetail(props) {

  const rows = [];
  var foo = "hi";
  if (props.model != null) { foo = props.model.length; }
  return (<div>{foo}<table style={{ width: '100%', tableLayout: 'fixed'}} className="table table-striped">
          <thead>
          <tr><td>Date</td><td>Txn ID</td><td>Account Name</td><td>Account Number</td>
                <td>Currency</td><td className="currency">Debit</td><td className="currency">Credit</td>
          </tr></thead>
          <tbody>
          {rows}
          </tbody>
          </table></div>);
}
