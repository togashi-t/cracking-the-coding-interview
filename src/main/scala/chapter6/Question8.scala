package chapter6

object Question8 {

  // 卵を落とす問題:100階建ての建物があります。
  // N階以上の高さから卵を落とすと卵は割れてしまいます。
  // N階より下からであれば卵は割れません。
  // 2つの卵を使い、落とす回数をできるだけ少なくなるようにNを見つけてください。



  // アプローチ方法は以下。
  // 一定間隔飛ばしで卵1を落としていく。例）10階 -> 20階 -> 30階 -> ...
  // もし卵1が割れたら、卵2を今後は1回刻みで落としていく。例）卵1が70階で割れた場合。卵2は61階 -> 60階 -> 63階 -> ...

  // ただし上記方法だと、卵が10階で割れた場合と、100階で割れた場合では、落とす階数Nに大きな乖離が生じる。
  // バランスがとれた方法ならば、卵1を落とす回数＋卵2を落とす回数が一定値になる。
  // 上記は言い換えると、卵1を落とす回数が増えるにつれて、卵2を落とす回数が減るということ。
  // すなわち、x + (x - 1) + (x - 2) + .... + 1 = 100
  // x * (x + 1) / 2 = 100 -> x * (x + 1) = 200
  // xは13と14の間

  // xが14の場合
  // 1回目 14階
  // 2回目 27階
  // 3回目 39階
  // 4回目 50階
  // 5回目 60階
  // 6回目 69階
  // 7回目 77階
  // 8回目 84階
  // 9回目 90階
  // 10回目 95階
  // 11回目 99階

  // xが13の場合。最上階まで到達しないのでNG。
  // 1回目 13階
  // 2回目 25階
  // 3回目 36階
  // 4回目 46階
  // 5回目 55階
  // 6回目 63階
  // 7回目 70階
  // 8回目 76階
  // 9回目 81階
  // 10回目 85階
  // 11回目 88階
  // 12回目 90階
  // 13回目 91階

  // したがって、N=14

}
