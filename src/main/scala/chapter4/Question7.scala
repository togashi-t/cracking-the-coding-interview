package chapter4

object Question7 extends App {

  // プロジェクトのリストと依存関係(プロジェクトのペアで、1番目のプロジェクトは2番目のプロジェクトに依存する)のリストが与えられます
  // 依存関係のあるプロジェクトは、そのプロジェクトの前にすべて完成していなければな りません。
  // このとき、実行可能なプロジェクトの順序を見つけてください。そのような順序づけが不可能な場合はエラーを返してください。
  //例
  //入力:プロジェクト:a,b,c,d,e,f
  //依存関係:(d,a),(b,f),(d,b),(a,f),(c,d)
  //出力:f,e,a,b,d,c


  // todo very difficult to understand
  def findBuildOrder(projects: List[String], dependencies: List[(String, String)]): Either[String, List[String]] = {
    // 依存関係を表すグラフを構築。依存関係がなく独立しているプロジェクトも含める。
    val graph = {
      val initialGraph = projects.map(_ -> List.empty[String]).toMap
      dependencies.foldLeft(initialGraph) { case (acc, (afterProject, formerProject)) =>
        acc.updated(formerProject, afterProject :: acc.getOrElse(formerProject, Nil))
      }
    }

    /**
     * 探索の状態を保持する。
     * @param visited 訪れたプロジェクト
     * @param order ビルド順（逆順）
     * @param visiting 訪問中のプロジェクト
     */
    case class State(visited: Set[String] = Set.empty, order: List[String] = Nil, visiting: Set[String] = Set.empty)

    // 深さ優先探索を行う。循環検出とビルド順序決定を行う。
    def dfs(project: String, state: State): Either[String, State] = {
      if (state.visited.contains(project)) { // 探索完了済プロジェクトの場合
        Right(state)
      } else if (state.visiting.contains(project)) { // 探索パスに循環がある場合
        Left("dependency cycle error")
      } else {
        val newState = state.copy(visited = state.visiting + project)
        val afterProjects = graph.getOrElse(project, Nil)
        val (isCyclic, finalState) = afterProjects.foldLeft((false, newState)) { case ((tmpIsCyclic, tmpState), afterProject) =>
          dfs(afterProject, tmpState) match {
            case Left(_) => (true, tmpState) // 循環が検出された場合、走査を中止。
            case Right(updatedState) => (tmpIsCyclic, updatedState) // 循環がなければ状態を更新。
          }
        }

        if (isCyclic) {
          Left("dependency cycle error")
        } else {
          // 探索が完了したプロジェクトをvisitedに移動し、ビルド順序に追加。visitingからは削除。
          Right(finalState.copy(visited = finalState.visited + project, order = project :: finalState.order, visiting = finalState.visiting - project))
        }
      }
    }

    // すべてのプロジェクトに対してDFSを実行し、ビルド順序を決定
    val (_, resultState) = projects.foldLeft((false, State())) {
      case ((cycle, state), project) => if (cycle) (cycle, state) else dfs(project, state) match {
        case Left(_) => (true, state) // 循環が検出された場合、走査を中止。
        case Right(updatedState) => (cycle, updatedState) // 循環がなければ状態を更新。
      }
    }

    // 全てのプロジェクトがビルド順序に含まれているか確認。含まれていなければエラー。
    if (resultState.visited.size != projects.size) Left("全てのプロジェクトをビルドできません")
    else Right(resultState.order.reverse) // ビルド順序を逆順にして返す。
  }

}
