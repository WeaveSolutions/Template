import play.Application;
import play.ApplicationLoader;
import play.BuiltInComponentsFromContext;
import play.LoggerConfigurator;
import play.filters.components.HttpFiltersComponents;
import play.filters.cors.CORSComponents;
import play.routing.Router;
import router.Routes;

/**
 * Application loader with compile-time dependency injection
 */
public class ApplicationLoader implements play.ApplicationLoader {
    
    @Override
    public Application load(Context context) {
        LoggerConfigurator.apply(
            context.environment().classLoader()
        ).ifPresent(loggerConfigurator ->
            loggerConfigurator.configure(
                context.environment(), 
                context.initialConfig(), 
                java.util.Map.of()
            )
        );
        
        return new ApplicationComponents(context).application();
    }
    
    /**
     * Application components
     */
    static class ApplicationComponents extends BuiltInComponentsFromContext 
            implements HttpFiltersComponents, CORSComponents {
        
        public ApplicationComponents(Context context) {
            super(context);
        }
        
        // Services
        private final services.JwtService jwtService = 
            new services.JwtService(config());
            
        private final actions.AuthAction authAction = 
            new actions.AuthAction(jwtService);
        
        // Controllers
        private final controllers.HealthController healthController = 
            new controllers.HealthController(config());
            
        private final controllers.AuthController authController = 
            new controllers.AuthController(wsClient(), config());
            
        private final controllers.UserController userController = 
            new controllers.UserController(jwtService);
            
        private final controllers.DataController dataController = 
            new controllers.DataController(wsClient(), config());
            
        private final controllers.ApiDocController apiDocController = 
            new controllers.ApiDocController();
        
        @Override
        public Router router() {
            return new Routes(
                httpErrorHandler(),
                healthController,
                assets(),
                apiDocController,
                authController,
                userController,
                dataController
            );
        }
    }
}
