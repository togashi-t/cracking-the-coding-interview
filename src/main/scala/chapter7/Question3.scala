package chapter7

import scala.collection.immutable.Queue

object Question3 {

  // ジュークボックス:オブジェクト指向でジュークボックスを設計してください。


  case class CD(title: String)
  case class CDPlayer(cd: Option[CD], playlist: Option[Playlist])
  case class User(id: Long, name: String)
  case class Song(songName: String)
  case class Playlist(currentSong: Option[Song], queue: Queue[Song]) {
    def getNextSongToPlay: Option[(Song, Playlist)] = queue.dequeueOption.map { case (nextSong, updatedQueue) => (nextSong, this.copy(queue = updatedQueue))}

    def queueUpSong(song: Song): Playlist = this.copy(queue = queue.enqueue(song))
  }

  case class SongSelector(currentSong: Option[Song]) {

    def setSong(song: Song): SongSelector = this.copy(currentSong = Some(song))

    def getCurrentSong: Option[Song] = currentSong
  }


  case class JukeBox(cdPlayer: CDPlayer, user: Option[User], cdCollection: Set[CD], ts: SongSelector) {
    def setUser(newUser: User): JukeBox = this.copy(user = Some(newUser))

    def addCD(cd: CD): JukeBox = this.copy(cdCollection = cdCollection + cd)

    def selectSong(song: Song): JukeBox = this.copy(cdPlayer = cdPlayer.copy(playlist = cdPlayer.playlist.map(_.queueUpSong(song))))

    def playNextSong(): Either[String, JukeBox] = cdPlayer.playlist.flatMap(_.getNextSongToPlay) match {
      case Some((nextSong, updatedPlaylist)) =>
        Right(this.copy(cdPlayer = cdPlayer.copy(playlist = Some(updatedPlaylist)), ts = ts.setSong(nextSong)))
      case None => Left("プレイリストに次の曲がありません")
    }
  }


}
