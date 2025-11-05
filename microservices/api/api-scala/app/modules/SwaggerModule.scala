package modules

import com.google.inject.AbstractModule
import play.api.{Configuration, Environment}
import io.swagger.config.{ScannerFactory, SwaggerConfig}
import io.swagger.models.{Info, Contact, License, Swagger}

/**
 * Swagger configuration module
 */
class SwaggerModule(environment: Environment, configuration: Configuration) extends AbstractModule {
  
  override def configure(): Unit = {
    // Configure Swagger scanner
    val scanner = new io.swagger.jaxrs.config.BeanConfig()
    scanner.setBasePath("/")
    scanner.setHost("localhost:8060")
    scanner.setSchemes(Array("http", "https"))
    scanner.setResourcePackage("controllers")
    scanner.setPrettyPrint(true)
    scanner.setScan(true)
    
    // Set scanner
    ScannerFactory.setScanner(scanner)
  }
}
