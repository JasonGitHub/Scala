package hacker_rank

object basketball_game {
  case class Player(val name: String, val onField: Boolean, val draft: Int, val perc: Int, val height: Int, val timePlayed: Int) {
    override def toString: String = name
    def doField = copy(onField = true)
    def doBench = copy(onField = false)
    def play = copy(timePlayed = timePlayed + 1)
  }

  def field(team: List[Player]) =
    team filter (p => p onField)                  //> field: (team: List[hacker_rank.basketball_game.Player])List[hacker_rank.bask
                                                  //| etball_game.Player]

  def bench(team: List[Player]) =
    team filterNot (p => p onField)               //> bench: (team: List[hacker_rank.basketball_game.Player])List[hacker_rank.bask
                                                  //| etball_game.Player]

  def play(team: List[Player]): List[Player] =
    team map (p => p play)                        //> play: (team: List[hacker_rank.basketball_game.Player])List[hacker_rank.baske
                                                  //| tball_game.Player]

  def rotate(team: List[Player]): List[Player] = {
    val max = field(team).maxBy(p => (p.timePlayed, p.draft))
    val min = bench(team).minBy(p => (p.timePlayed, p.draft))
    team updated (team indexOf max, max doField) updated (team indexOf min, min doBench)
  }                                               //> rotate: (team: List[hacker_rank.basketball_game.Player])List[hacker_rank.bas
                                                  //| ketball_game.Player]

  def setField(players: List[Player], f: Player => Boolean) =
    players map (p => if (f(p)) p doField else p) //> setField: (players: List[hacker_rank.basketball_game.Player], f: hacker_rank
                                                  //| .basketball_game.Player => Boolean)List[hacker_rank.basketball_game.Player]

  def setDraft(players: List[Player]) =
    players map (p => p.copy(draft = players.indexOf(p) + 1))
                                                  //> setDraft: (players: List[hacker_rank.basketball_game.Player])List[hacker_ra
                                                  //| nk.basketball_game.Player]

  def init(players: List[Player], P: Int) = {
    val drafted = setDraft(players.sortBy(p => (p.perc, p.height)).reverse)
    setField(drafted, (p: Player) => p.draft <= 2 * P)
  }                                               //> init: (players: List[hacker_rank.basketball_game.Player], P: Int)List[hacke
                                                  //| r_rank.basketball_game.Player]

  val players = List(new Player("Wai", false, 0, 99, 131, 0),
    new Player("Weiyan", false, 0, 81, 155, 0),
    new Player("Lin", false, 0, 80, 100, 0),
    new Player("Purav", false, 0, 86, 198, 0),
    new Player("Slawek", false, 0, 80, 192, 0),
    new Player("Meihong", false, 0, 44, 109, 0))  //> players  : List[hacker_rank.basketball_game.Player] = List(Wai, Weiyan, Lin
                                                  //| , Purav, Slawek, Meihong)
  val P = 2                                       //> P  : Int = 2
  val M = 3                                       //> M  : Int = 3

  val teamA = init(players, P) filter (p => p.draft % 2 == 1)
  val teamB = init(players, P) filter (p => p.draft % 2 == 0)

  def solve(team: List[Player], m: Int): List[Player] =
    if (m == 1) team
    else solve(rotate(team), m - 1)
  
  val res = (field(solve(teamA, M)) ++ field(solve(teamB, M))).sortBy(p => p.name)
}