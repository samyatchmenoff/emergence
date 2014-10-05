package life

import com.badlogic.gdx.backends.lwjgl._

object Main extends App {
    val cfg = new LwjglApplicationConfiguration
    cfg.title = "life"
    cfg.width = 1280 // 71 by 40
    cfg.height = 720
    cfg.forceExit = false
    new LwjglApplication(new Life, cfg)
}
