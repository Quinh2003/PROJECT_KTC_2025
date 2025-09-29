package ktc.spring_project.controllers;

import org.springframework.http.ResponseEntity;
import org.springframework.web.bind.annotation.GetMapping;
import org.springframework.web.bind.annotation.RequestParam;
import org.springframework.web.bind.annotation.RestController;
import org.springframework.web.client.RestTemplate;
import org.springframework.web.util.UriComponentsBuilder;

@RestController
public class NominatimProxyController {
    private final RestTemplate restTemplate = new RestTemplate();

    @GetMapping("/api/nominatim/search")
    public ResponseEntity<String> proxyNominatim(
            @RequestParam("q") String query,
            @RequestParam(value = "format", defaultValue = "json") String format,
            @RequestParam(value = "addressdetails", defaultValue = "1") String addressdetails,
            @RequestParam(value = "limit", defaultValue = "5") String limit,
            @RequestParam(value = "countrycodes", defaultValue = "vn") String countrycodes
    ) {
    String url = UriComponentsBuilder.newInstance()
        .scheme("https")
        .host("nominatim.openstreetmap.org")
        .path("/search")
        .queryParam("q", query)
        .queryParam("format", format)
        .queryParam("addressdetails", addressdetails)
        .queryParam("limit", limit)
        .queryParam("countrycodes", countrycodes)
        .build()
        .toUriString();
        String response = restTemplate.getForObject(url, String.class);
        return ResponseEntity.ok(response);
    }
}
