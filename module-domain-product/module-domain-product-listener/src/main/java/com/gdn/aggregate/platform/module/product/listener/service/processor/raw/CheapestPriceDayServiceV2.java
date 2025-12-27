package com.gdn.aggregate.platform.module.product.listener.service.processor.raw;

import java.util.ArrayList;
import java.util.List;
import java.util.Optional;

import org.apache.commons.lang3.StringUtils;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.stereotype.Component;

import com.gdn.aggregate.platform.module.product.listener.model.raw.CheapestPriceDay;
import com.gdn.aggregate.platform.module.product.listener.repository.raw.CheapestPriceDayRepository;

@Component
public class CheapestPriceDayServiceV2 {

  @Autowired
  private CheapestPriceDayRepository cheapestPriceDayRepository;

  public List<CheapestPriceDay> findAllByProductSku(String productSku) {
    return cheapestPriceDayRepository.findAllByProductSku(productSku);
  }

  public CheapestPriceDay getExistingCheapestPriceDay(String id, List<CheapestPriceDay> allCheapestPriceDays) {
    return Optional.ofNullable(allCheapestPriceDays).orElseGet(ArrayList::new)
        .stream()
        .filter(cheapestPriceDay -> Optional.ofNullable(id).orElse(StringUtils.EMPTY).equals(cheapestPriceDay.getId()))
        .findFirst()
        .orElse(null);
  }
}
