package ktc.spring_project.services;

import ktc.spring_project.entities.Product;
import ktc.spring_project.repository.ProductRepository;
import lombok.RequiredArgsConstructor;
import org.springframework.stereotype.Service;

import java.util.List;
import java.util.Optional;

@Service
@RequiredArgsConstructor
public class ProductService {
    private final ProductRepository productRepository;

    public List<Product> findAll() {
        return productRepository.findAll();
    }

    public Optional<Product> findById(Long id) {
        return productRepository.findById(id);
    }

    public Product save(Product entities) {
        return productRepository.save(entities);
    }

    public void delete(Long id) {
        productRepository.deleteById(id);
    }
}
