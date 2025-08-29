package ktc.spring_project.exceptions;

import org.springframework.http.HttpStatus;

public class EntityDuplicateException extends RuntimeException {
    private final HttpStatus status;

    public EntityDuplicateException(String message) {
        super(message);
        this.status = HttpStatus.CONFLICT;
    }

    public HttpStatus getStatus() {
        return status;
    }
}
