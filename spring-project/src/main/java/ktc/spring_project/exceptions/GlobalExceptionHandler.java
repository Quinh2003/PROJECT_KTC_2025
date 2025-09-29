package ktc.spring_project.exceptions;

import org.springframework.http.HttpStatus;
import ktc.spring_project.exceptions.EntityDuplicateException;
import org.springframework.http.ResponseEntity;
import org.springframework.http.converter.HttpMessageNotReadableException;
import org.springframework.security.access.AccessDeniedException;
import org.springframework.security.core.AuthenticationException;
import org.springframework.validation.FieldError;
import org.springframework.web.bind.MethodArgumentNotValidException;
import org.springframework.web.bind.annotation.ControllerAdvice;
import org.springframework.web.bind.annotation.ExceptionHandler;

import java.util.ArrayList;
import java.util.HashMap;
import java.util.List;
import java.util.Map;

// This class handles global exceptions for the application.
// It intercepts exceptions thrown by controllers
// and provides a consistent error and response format.

@ControllerAdvice
public class GlobalExceptionHandler {
    // Handle malformed JSON or parse errors
    @ExceptionHandler(HttpMessageNotReadableException.class)
    public ResponseEntity<ErrorResponse> handleHttpMessageNotReadableException(HttpMessageNotReadableException ex) {
    ex.printStackTrace();
        ErrorResponse errorResponse = new ErrorResponse(
                HttpStatus.BAD_REQUEST.value(),
                List.of("Malformed JSON request: " + ex.getMessage()),
                HttpStatus.BAD_REQUEST.getReasonPhrase()
        );
        return new ResponseEntity<>(errorResponse, HttpStatus.BAD_REQUEST);
    }
    @ExceptionHandler(VehicleInvalidCapacityException.class)
    public ResponseEntity<ErrorResponse> handleVehicleInvalidCapacityException(VehicleInvalidCapacityException ex) {
    ex.printStackTrace();
        ErrorResponse errorResponse = new ErrorResponse(ex.getStatus().value(), List.of(ex.getMessage()),
                ex.getStatus().getReasonPhrase());
        return new ResponseEntity<>(errorResponse, ex.getStatus());
    }

    // Data validation error handler
    @ExceptionHandler(MethodArgumentNotValidException.class)
    public ResponseEntity<ErrorResponse> handleValidationErrors(MethodArgumentNotValidException ex) {
    ex.printStackTrace();
        List<String> errorMessages = new ArrayList<>();

        ex.getBindingResult().getAllErrors().forEach(error -> {
            String errorMessage = error.getDefaultMessage();
            String fieldName = ((FieldError) error).getField();
            errorMessages.add(fieldName + ": " + errorMessage);

        });

        ErrorResponse errorResponse = new ErrorResponse(
                HttpStatus.BAD_REQUEST.value(),
                errorMessages,
                HttpStatus.BAD_REQUEST.getReasonPhrase());

        return new ResponseEntity<>(errorResponse, HttpStatus.BAD_REQUEST);
    }

    // Access denied error handler
    @ExceptionHandler(AccessDeniedException.class)
    public ResponseEntity<Map<String, Object>> handleAccessDeniedException(AccessDeniedException ex) {
    ex.printStackTrace();
        Map<String, Object> errorResponse = new HashMap<>();
        errorResponse.put("status", 403);
        errorResponse.put("error", "Forbidden");
        errorResponse.put("messages", List.of("Access denied: You don't have permission to access this resource"));

        return ResponseEntity.status(HttpStatus.FORBIDDEN).body(errorResponse);
    }

    // Authentication error handler
    @ExceptionHandler(AuthenticationException.class)
    public ResponseEntity<Map<String, Object>> handleAuthenticationException(AuthenticationException ex) {
    ex.printStackTrace();
        Map<String, Object> errorResponse = new HashMap<>();
        errorResponse.put("status", 401);
        errorResponse.put("error", "Unauthorized");
        errorResponse.put("messages", List.of("Authentication failed: " + ex.getMessage()));

        return ResponseEntity.status(HttpStatus.UNAUTHORIZED).body(errorResponse);
    }

    // Custom entity not found exception handler
    @ExceptionHandler(EntityNotFoundException.class)
    public ResponseEntity<ErrorResponse> handleEntityNotFoundException(EntityNotFoundException ex) {
    ex.printStackTrace();
        ErrorResponse errorResponse = new ErrorResponse(ex.getStatus().value(), List.of(ex.getMessage()),
                ex.getStatus().getReasonPhrase());
        return new ResponseEntity<>(errorResponse, ex.getStatus());
    }

    @ExceptionHandler(EntityDuplicateException.class)
    public ResponseEntity<ErrorResponse> handleEntityDuplicateException(EntityDuplicateException ex) {
    ex.printStackTrace();
        ErrorResponse errorResponse = new ErrorResponse(ex.getStatus().value(), List.of(ex.getMessage()),
                ex.getStatus().getReasonPhrase());
        return new ResponseEntity<>(errorResponse, ex.getStatus());
    }

    // Custom HTTP exception handler
    @ExceptionHandler(HttpException.class)
    public ResponseEntity<ErrorResponse> handleHttpException(HttpException ex) {
    ex.printStackTrace();
        ErrorResponse errorResponse = new ErrorResponse(
                ex.getStatus().value(),
                List.of(ex.getMessage()),
                ex.getStatus().getReasonPhrase()
        );
        return new ResponseEntity<>(errorResponse, ex.getStatus());
    }

    // Generic exception handler for uncaught exceptions
    @ExceptionHandler(Exception.class)
    public ResponseEntity<ErrorResponse> handleGenericException(Exception ex) {
    ex.printStackTrace();
        ErrorResponse errorResponse = new ErrorResponse(HttpStatus.INTERNAL_SERVER_ERROR.value(),
                List.of(ex.getMessage()), HttpStatus.INTERNAL_SERVER_ERROR.getReasonPhrase());
        return new ResponseEntity<>(errorResponse, HttpStatus.INTERNAL_SERVER_ERROR);
    }
}