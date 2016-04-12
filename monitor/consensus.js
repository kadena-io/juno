import React from 'react';

function consensusStatus(data) {
  const consensus = data.juno.consensus;
  return {
    commit_index: consensus.commit_index.val,
    commit_period: consensus.commit_period.val,
    current_leader: consensus.current_leader.val,
    term: consensus.term.val,
    hash: consensus.hash.val,
  };
}

export default function Transactions({data}) {
  if (data == null) {
    return <span />;
  }

  const {
    commit_index,
    term,
    commit_period,
    current_leader,
    hash,
  } = consensusStatus(data);

  return (
    <div className="section">
      <h2>
        TRANSACTIONS
        <div className="border-underline" />
      </h2>
      <div className="transaction-point">
        Commit Index: <span className="transaction-datum">{commit_index}</span>
      </div>
    </div>
  );
}
