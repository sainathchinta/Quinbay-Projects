package com.gdn.aggregate.platform.module.product.listener.service.processor.raw;

import java.util.ArrayList;
import java.util.List;
import java.util.Optional;

import org.apache.commons.lang3.StringUtils;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.stereotype.Component;

import com.gdn.aggregate.platform.module.product.listener.model.raw.Product;
import com.gdn.aggregate.platform.module.product.listener.repository.raw.ProductRepository;

@Component
public class ProductServiceV2 {

  @Autowired
  private ProductRepository productRepository;

  public Product findByProductSku(String productSku) {
    return productRepository.findByProductSku(productSku);
  }

  public Product getExistingProduct(String id, List<Product> allProducts) {
    return Optional.ofNullable(allProducts).orElseGet(ArrayList::new).stream()
        .filter(product -> Optional.ofNullable(id).orElse(StringUtils.EMPTY).equals(product.getId())).findFirst()
        .orElse(null);
  }
}
