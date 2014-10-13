package life

import com.badlogic.gdx.backends.lwjgl._

object Main extends App {
    val cfg = new LwjglApplicationConfiguration
    cfg.title = "life"
    cfg.width = 1420
    cfg.height = 880
    cfg.forceExit = false
    new LwjglApplication(new Life, cfg)
}
