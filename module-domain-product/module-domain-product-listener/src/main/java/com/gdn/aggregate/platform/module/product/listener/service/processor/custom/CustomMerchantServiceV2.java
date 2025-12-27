package com.gdn.aggregate.platform.module.product.listener.service.processor.custom;

import java.util.ArrayList;
import java.util.List;
import java.util.Optional;
import java.util.Set;

import org.apache.commons.lang3.StringUtils;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.stereotype.Component;

import com.gdn.aggregate.platform.module.product.listener.model.custom.CustomMerchant;
import com.gdn.aggregate.platform.module.product.listener.model.raw.Product;
import com.gdn.aggregate.platform.module.product.listener.repository.custom.CustomMerchantRepository;

@Component("ProductCustomMerchantServiceV2")
public class CustomMerchantServiceV2 {

  @Autowired
  private CustomMerchantRepository customMerchantRepository;

  public List<CustomMerchant> findAllByIds(Set<String> ids) {
    return customMerchantRepository.findByIdIn(ids);
  }

  public CustomMerchant getExistingCustomMerchant(String id, List<CustomMerchant> allCustomMerchants) {
    return Optional.ofNullable(allCustomMerchants).orElseGet(ArrayList::new).stream()
        .filter(customMerchant -> Optional.ofNullable(id).orElse(StringUtils.EMPTY).equals(customMerchant.getId()))
        .findFirst().orElse(null);
  }

  public CustomMerchant getExistingCustomMerchantByProduct(Product product, List<CustomMerchant> allCustomMerchants) {
    return Optional.ofNullable(product).map(Product::getMerchantCode)
        .map(merchantCode -> getExistingCustomMerchant(merchantCode, allCustomMerchants)).orElse(null);
  }
}
