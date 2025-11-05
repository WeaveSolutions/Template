package services;

import com.auth0.jwk.Jwk;
import com.auth0.jwk.JwkProvider;
import com.auth0.jwk.UrlJwkProvider;
import com.auth0.jwt.JWT;
import com.auth0.jwt.algorithms.Algorithm;
import com.auth0.jwt.interfaces.DecodedJWT;
import com.auth0.jwt.interfaces.JWTVerifier;
import com.auth0.jwt.exceptions.JWTVerificationException;
import com.auth0.jwt.exceptions.TokenExpiredException;
import com.typesafe.config.Config;
import play.Logger;
import javax.inject.Inject;
import javax.inject.Singleton;
import java.security.interfaces.RSAPublicKey;
import java.util.Arrays;
import java.util.List;
import java.util.concurrent.CompletableFuture;
import java.util.concurrent.CompletionStage;

/**
 * JWT validation service for Auth0 tokens
 */
@Singleton
public class JwtService {
    
    private static final Logger.ALogger logger = Logger.of(JwtService.class);
    
    private final String auth0Domain;
    private final String auth0Audience;
    private final String auth0Issuer;
    private final String jwksUrl;
    private final JwkProvider jwkProvider;
    
    @Inject
    public JwtService(Config config) {
        this.auth0Domain = config.getString("auth0.domain");
        this.auth0Audience = config.getString("auth0.audience");
        this.auth0Issuer = config.getString("auth0.issuer");
        this.jwksUrl = config.getString("auth0.jwksUrl");
        this.jwkProvider = new UrlJwkProvider(jwksUrl);
    }
    
    /**
     * Validate JWT token
     */
    public CompletionStage<Either<String, DecodedJWT>> validateToken(String token) {
        return CompletableFuture.supplyAsync(() -> {
            try {
                // Decode the token to get the key ID
                DecodedJWT decodedToken = JWT.decode(token);
                String keyId = decodedToken.getKeyId();
                
                if (keyId == null) {
                    return Either.left("No key ID found in token");
                }
                
                // Get the JWK from Auth0
                Jwk jwk = jwkProvider.get(keyId);
                RSAPublicKey publicKey = (RSAPublicKey) jwk.getPublicKey();
                Algorithm algorithm = Algorithm.RSA256(publicKey, null);
                
                // Create verifier with all required claims
                JWTVerifier verifier = JWT.require(algorithm)
                    .withIssuer(auth0Issuer)
                    .withAudience(auth0Audience)
                    .build();
                
                // Verify the token
                DecodedJWT verified = verifier.verify(token);
                return Either.right(verified);
                
            } catch (TokenExpiredException e) {
                logger.debug("Token expired", e);
                return Either.left("Token expired");
            } catch (JWTVerificationException e) {
                logger.debug("Token verification failed", e);
                return Either.left("Invalid token: " + e.getMessage());
            } catch (Exception e) {
                logger.error("Unexpected error validating token", e);
                return Either.left("Token validation error");
            }
        });
    }
    
    /**
     * Extract user ID from token
     */
    public String getUserId(DecodedJWT token) {
        return token.getSubject();
    }
    
    /**
     * Extract user email from token
     */
    public String getUserEmail(DecodedJWT token) {
        return token.getClaim("email").asString();
    }
    
    /**
     * Extract user roles from token
     */
    public List<String> getUserRoles(DecodedJWT token) {
        String[] roles = token.getClaim("roles").asArray(String.class);
        return roles != null ? Arrays.asList(roles) : List.of();
    }
    
    /**
     * Extract user permissions from token
     */
    public List<String> getUserPermissions(DecodedJWT token) {
        String[] permissions = token.getClaim("permissions").asArray(String.class);
        return permissions != null ? Arrays.asList(permissions) : List.of();
    }
    
    /**
     * Check if token has specific scope
     */
    public boolean hasScope(DecodedJWT token, String scope) {
        String scopeClaim = token.getClaim("scope").asString();
        if (scopeClaim == null) {
            return false;
        }
        return Arrays.asList(scopeClaim.split(" ")).contains(scope);
    }
    
    /**
     * Check if token has specific permission
     */
    public boolean hasPermission(DecodedJWT token, String permission) {
        return getUserPermissions(token).contains(permission);
    }
    
    /**
     * Either type for error handling
     */
    public static class Either<L, R> {
        private final L left;
        private final R right;
        
        private Either(L left, R right) {
            this.left = left;
            this.right = right;
        }
        
        public static <L, R> Either<L, R> left(L value) {
            return new Either<>(value, null);
        }
        
        public static <L, R> Either<L, R> right(R value) {
            return new Either<>(null, value);
        }
        
        public boolean isLeft() {
            return left != null;
        }
        
        public boolean isRight() {
            return right != null;
        }
        
        public L getLeft() {
            return left;
        }
        
        public R getRight() {
            return right;
        }
    }
}
