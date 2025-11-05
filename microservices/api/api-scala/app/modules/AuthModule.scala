package modules

import com.google.inject.AbstractModule
import play.api.{Configuration, Environment}
import services.JwtService
import actions.AuthAction

/**
 * Authentication module
 */
class AuthModule(environment: Environment, configuration: Configuration) extends AbstractModule {
  
  override def configure(): Unit = {
    // Bind authentication services
    bind(classOf[JwtService]).asEagerSingleton()
    bind(classOf[AuthAction]).asEagerSingleton()
  }
}
