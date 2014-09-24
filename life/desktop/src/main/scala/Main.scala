package life

import com.badlogic.gdx.backends.lwjgl._

object Main extends App {
    val cfg = new LwjglApplicationConfiguration
    cfg.title = "life"
    cfg.height = 720
    cfg.width = 1280
    cfg.forceExit = false
    new LwjglApplication(new Life, cfg)
}
