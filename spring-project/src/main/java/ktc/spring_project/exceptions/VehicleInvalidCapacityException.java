package ktc.spring_project.exceptions;

import org.springframework.http.HttpStatus;

public class VehicleInvalidCapacityException extends RuntimeException {
    private final HttpStatus status;

    public VehicleInvalidCapacityException(String message) {
        super(message);
        this.status = HttpStatus.BAD_REQUEST;
    }

    public HttpStatus getStatus() {
        return status;
    }
}
