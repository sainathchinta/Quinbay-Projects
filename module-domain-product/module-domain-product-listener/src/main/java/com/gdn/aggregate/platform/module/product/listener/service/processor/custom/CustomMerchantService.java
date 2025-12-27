package com.gdn.aggregate.platform.module.product.listener.service.processor.custom;

import com.gdn.aggregate.platform.module.product.listener.model.custom.CustomMerchant;
import com.gdn.aggregate.platform.module.product.listener.model.raw.Product;
import com.gdn.aggregate.platform.module.product.listener.repository.custom.CustomMerchantRepository;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.stereotype.Component;

import java.util.Optional;

@Component("ProductCustomMerchantService")
public class CustomMerchantService {

  @Autowired
  private CustomMerchantRepository customMerchantRepository;

  public CustomMerchant getExistingCustomMerchant(String id) {
    return Optional.ofNullable(id)
        .flatMap(customMerchantRepository::findById)
        .orElse(null);
  }

  public CustomMerchant getExistingCustomMerchantByProduct(Product product) {
    return Optional.ofNullable(product)
        .map(Product::getMerchantCode)
        .map(this::getExistingCustomMerchant)
        .orElse(null);
  }

}
