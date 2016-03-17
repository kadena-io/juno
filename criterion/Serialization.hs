{-# LANGUAGE OverloadedStrings #-}

module Main where

import Criterion.Main

import Codec.Crypto.RSA
import qualified Data.Map as Map
import Data.Set (Set)
import qualified Data.Set as Set
import Data.Sequence (Seq)
import qualified Data.Sequence as Seq
import Juno.Runtime.Types

main = defaultMain [
  bgroup "AppendEntries" [
      bench "Two LogEntries" $ whnf ((\(Right v) -> v) . fromWire keySet :: SignedRPC -> AppendEntries) aeSignedRPC]
  ]

-- Old Stuff


-- ##########################################################
-- ####### All the stuff we need to actually run this #######
-- ##########################################################

-- #######################################################################
-- NodeID's + Keys for Client (10002), Leader (10000) and Follower (10001)
-- #######################################################################
nodeIdLeader, nodeIdFollower, nodeIdClient :: NodeID
nodeIdLeader = NodeID "localhost" 10000
nodeIdFollower = NodeID "localhost" 10001
nodeIdClient = NodeID "localhost" 10002

privKeyLeader, privKeyFollower, privKeyClient :: PrivateKey
privKeyLeader = PrivateKey {private_pub = PublicKey {public_size = 128, public_n = 143263094045483237413672742110806909557046787889291330401749751443369156063681858601975919855307842489235960299544273678048933580691559560784431105517057635455255146471809872702665529086654681916300619471314964172045542731305151294857601932085639875342307156397078951049517603494584167375520281289470658070821, public_e = 65537}, private_d = 140377589915906999969388627494845466088238301690443892219802001746171431327974394828006891455945553216217795482793454427014791152345543438285149034941924235746522892080902348016215668071955199106399324875029853484007717381391953823509488195274062416447784661420846624275968351998125095977024136079063438454573, private_p = 12322642714345408536017571405614321258874483339719624321916754832065122801766236021586867686343013657373047283868589815935715896579364378876832667679254511, private_q = 11626004045277029718858993149611335122220600272505403342820851017531587390596697925738903796399688718560222927972268099165561188726104695590946150339079211, private_dP = 0, private_dQ = 0, private_qinv = 0}
privKeyFollower = PrivateKey {private_pub = PublicKey {public_size = 128, public_n = 119734587589518990148219161431156442604320649075830559997839966910663760506098828080257917947217357217568770968568912187185962902759815037372954576824527044099430785533420222306796220280055320145935645419601056718741502607027250695371136410369973248364689692994952014429670625261164422497595026998844835804983, public_e = 65537}, private_d = 42831647335835988907561377856661605511629969283515749097294050448656502453651844385821240342929394427707741681601684198489215470532693039598570991944614668183055075411290182049507917324514952869097870506339758634913916485661089305995857595662086838794391326313280954244219693809814987046399359265702339099489, private_p = 11149448648727520741344534515175453743631376308425947520306431957535748167889134814371076822226395101702128442944739479928637847681449963148838547576756349, private_q = 10739059065775796910540038518886518635626397455942041662941214452735467326713118657850293569672623570913891867630588355426481112134066746585198501737910467, private_dP = 0, private_dQ = 0, private_qinv = 0}
privKeyClient = PrivateKey {private_pub = PublicKey {public_size = 128, public_n = 106252368639070633938244027838812466487634971459399633948485579058064750184804467292629472697808998989066368983424031700876663753846147139515953278402011718215436563286250115914185011343557536197541367961085118851954966833333275851022828003953887053315417450038702459750117667520446623825484282212798093912159, public_e = 65537}, private_d = 75403076323825260750931405019383386543073605560297504849323524061393311661276393019407420933249900574980358500542720758900663053353532467335207150543954743785029033469647938675096518795993183160202689835102089214515513968926523692041172327354495493314285943948188194911060155947074855453679649323415024020601, private_p = 10533116134787235979430606573617029447681668880981449216358619427605789055226200529358027255251243796670635247358375658748310100973047917014533032664997323, private_q = 10087458191802883990555912812743556937883302367567842795334469443655041316171806703582222153719435177125303551647636770526849653208414415981311349629193533, private_dP = 0, private_dQ = 0, private_qinv = 0}

pubKeyLeader, pubKeyFollower, pubKeyClient :: PublicKey
pubKeyLeader = private_pub privKeyLeader
pubKeyFollower = private_pub privKeyFollower
pubKeyClient = private_pub privKeyClient

keySet :: KeySet
keySet = KeySet
  { _ksCluster = Map.fromList [(nodeIdLeader, pubKeyLeader),(nodeIdFollower, pubKeyFollower)]
  , _ksClient = Map.fromList [(nodeIdClient, pubKeyClient)] }


-- #####################################
-- Commands, with and without provenance
-- #####################################
cmdRPC1, cmdRPC2 :: Command
cmdRPC1 = Command
  { _cmdEntry = CommandEntry "CreateAccount foo"
  , _cmdClientId = nodeIdClient
  , _cmdRequestId = RequestId 0
  , _cmdProvenance = NewMsg }
cmdRPC2 = Command
  { _cmdEntry = CommandEntry "CreateAccount foo"
  , _cmdClientId = nodeIdClient
  , _cmdRequestId = RequestId 1
  , _cmdProvenance = NewMsg }

cmdSignedRPC1, cmdSignedRPC2 :: SignedRPC
cmdSignedRPC1 = toWire nodeIdClient privKeyClient cmdRPC1
cmdSignedRPC2 = toWire nodeIdClient privKeyClient cmdRPC2

-- these are signed (received) provenance versions
cmdRPC1', cmdRPC2' :: Command
cmdRPC1' = (\(Right v) -> v) $ fromWire keySet cmdSignedRPC1
cmdRPC2' = (\(Right v) -> v) $ fromWire keySet cmdSignedRPC2

-- #############################################
-- CommandResponse, with and without provenance
-- #############################################
cmdrRPC :: CommandResponse
cmdrRPC = CommandResponse
  { _cmdrResult     = CommandResult "account created: foo"
  , _cmdrLeaderId   = nodeIdLeader
  , _cmdrNodeId     = nodeIdLeader
  , _cmdrRequestId  = RequestId 1
  , _cmdrProvenance = NewMsg
  }

cmdrSignedRPC :: SignedRPC
cmdrSignedRPC = toWire nodeIdLeader privKeyLeader cmdrRPC

-- ########################################################
-- LogEntry(s) and Seq LogEntry with correct hashes.
-- LogEntry is not an RPC but is(are) nested in other RPCs.
-- Given this, they are handled differently (no provenance)
-- ########################################################
logEntry1, logEntry2 :: LogEntry
logEntry1 = LogEntry
  { _leTerm    = Term 0
  , _leCommand = cmdRPC1'
  , _leHash    = "\237\157D\GS\158k\214\188\219.,\248\226\232\174\227\228\236R\t\189\&3v\NUL\255\&5\224\&4|\178\STX\252"
  }
logEntry2 = LogEntry
  { _leTerm    = Term 0
  , _leCommand = cmdRPC2'
  , _leHash    = "\244\136\187c\222\164\131\178;D)M\DEL\142|\251Kv\213\186\247q;3`\194\227O\US\223Q\157"
  }

leSeq, leSeqDecoded :: Seq LogEntry
leSeq = Seq.fromList [logEntry1, logEntry2]
leSeqDecoded = (\(Right v) -> v) $ decodeLEWire keySet leWire

leWire :: [LEWire]
leWire = encodeLEWire nodeIdLeader privKeyLeader leSeq

-- ################################################
-- RequestVoteResponse, with and without provenance
-- ################################################

rvrRPC1, rvrRPC2 :: RequestVoteResponse
rvrRPC1 = RequestVoteResponse
  { _rvrTerm        = Term 0
  , _rvrCurLogIndex = LogIndex (-1)
  , _rvrNodeId      = nodeIdLeader
  , _voteGranted    = True
  , _rvrCandidateId = nodeIdLeader
  , _rvrProvenance  = NewMsg
  }
rvrRPC2 = RequestVoteResponse
  { _rvrTerm        = Term 0
  , _rvrCurLogIndex = LogIndex (-1)
  , _rvrNodeId      = nodeIdFollower
  , _voteGranted    = True
  , _rvrCandidateId = nodeIdLeader
  , _rvrProvenance  = NewMsg
  }

rvrSignedRPC1, rvrSignedRPC2 :: SignedRPC
rvrSignedRPC1 = toWire nodeIdLeader privKeyLeader rvrRPC1
rvrSignedRPC2 = toWire nodeIdFollower privKeyFollower rvrRPC2

rvrRPC1', rvrRPC2' :: RequestVoteResponse
rvrRPC1' = (\(Right v) -> v) $ fromWire keySet rvrSignedRPC1
rvrRPC2' = (\(Right v) -> v) $ fromWire keySet rvrSignedRPC2

rvrRPCSet' :: Set RequestVoteResponse
rvrRPCSet' = Set.fromList [rvrRPC1', rvrRPC2']

rvrSignedRPCList :: [SignedRPC]
rvrSignedRPCList = [rvrSignedRPC1, rvrSignedRPC2]

-- #############################################
-- AppendEntries, with and without provenance
-- #############################################
aeRPC :: AppendEntries
aeRPC = AppendEntries
  { _aeTerm        = Term 0
  , _leaderId      = nodeIdLeader
  , _prevLogIndex  = LogIndex (-1)
  , _prevLogTerm   = Term 0
  , _aeEntries     = leSeq
  , _aeQuorumVotes = rvrRPCSet'
  , _aeProvenance  = NewMsg
  }

aeSignedRPC :: SignedRPC
aeSignedRPC = toWire nodeIdLeader privKeyLeader aeRPC

aeRPC' :: RequestVoteResponse
aeRPC' = (\(Right v) -> v) $ fromWire keySet aeSignedRPC

-- #####################
-- AppendEntriesResponse
-- #####################
aerRPC :: AppendEntriesResponse
aerRPC = AppendEntriesResponse
  { _aerTerm       = Term 0
  , _aerNodeId     = nodeIdFollower
  , _aerSuccess    = True
  , _aerConvinced  = True
  , _aerIndex      = LogIndex 1
  , _aerHash       = "\244\136\187c\222\164\131\178;D)M\DEL\142|\251Kv\213\186\247q;3`\194\227O\US\223Q\157"
  , _aerProvenance = NewMsg
  }

aerSignedRPC :: SignedRPC
aerSignedRPC = toWire nodeIdFollower privKeyFollower aerRPC

-- ###########
-- RequestVote
-- ###########
rvRPC :: RequestVote
rvRPC = RequestVote
  { _rvTerm        = Term 0
  , _rvCandidateId = nodeIdLeader
  , _lastLogIndex  = LogIndex (-1)
  , _lastLogTerm   = Term (-1)
  , _rvProvenance  = NewMsg
  }

rvSignedRPC :: SignedRPC
rvSignedRPC = toWire nodeIdLeader privKeyLeader rvRPC

-- ##########
-- Revolution
-- ##########
revRPC :: Revolution
revRPC = Revolution
  { _revClientId   = nodeIdClient
  , _revLeaderId   = nodeIdLeader
  , _revRequestId  = RequestId 2
  , _revProvenance = NewMsg
  }

revSignedRPC :: SignedRPC
revSignedRPC = toWire nodeIdClient privKeyClient revRPC
