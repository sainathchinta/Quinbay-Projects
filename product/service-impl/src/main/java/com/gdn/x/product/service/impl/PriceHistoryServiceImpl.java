package com.gdn.x.product.service.impl;

import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.stereotype.Service;

import com.gdn.x.product.dao.api.PriceHistoryRepository;
import com.gdn.x.product.model.entity.PriceHistory;
import com.gdn.x.product.service.api.PriceHistoryService;

@Service
public class PriceHistoryServiceImpl implements PriceHistoryService {

  @Autowired
  private PriceHistoryRepository priceHistoryRepository;

  @Override
  public PriceHistory savePriceHistory(PriceHistory priceHistory) {
    return this.priceHistoryRepository.save(priceHistory);
  }

}
