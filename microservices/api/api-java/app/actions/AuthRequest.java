package actions;

import com.auth0.jwt.interfaces.DecodedJWT;
import play.libs.typedmap.TypedKey;
import play.mvc.With;
import java.lang.annotation.ElementType;
import java.lang.annotation.Retention;
import java.lang.annotation.RetentionPolicy;
import java.lang.annotation.Target;

/**
 * Authentication annotation for controller methods
 */
@With(AuthAction.class)
@Target({ElementType.TYPE, ElementType.METHOD})
@Retention(RetentionPolicy.RUNTIME)
public @interface AuthRequest {
    /**
     * Typed key for storing the decoded JWT token in the request
     */
    TypedKey<DecodedJWT> TOKEN = TypedKey.create("token");
}
