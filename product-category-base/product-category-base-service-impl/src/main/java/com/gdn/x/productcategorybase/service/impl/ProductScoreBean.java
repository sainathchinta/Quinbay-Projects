package com.gdn.x.productcategorybase.service.impl;

import java.util.List;

import org.apache.commons.collections.CollectionUtils;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.data.domain.Page;
import org.springframework.data.domain.Pageable;
import org.springframework.stereotype.Service;
import org.springframework.transaction.annotation.Transactional;

import com.gdn.x.productcategorybase.repository.ProductScoreUpdateRepository;
import com.gdn.x.productcategorybase.service.ProductScoreService;
import lombok.extern.slf4j.Slf4j;

@Slf4j
@Service
public class ProductScoreBean implements ProductScoreService {

  @Autowired
  private ProductScoreUpdateRepository productScoreUpdateRepository;

  @Transactional(readOnly = true)
  @Override
  public Page<String> findByMarkForDeleteFalseAndUpdatedFalse(Pageable pageable) {
    return this.productScoreUpdateRepository.findByMarkForDeleteFalseAndUpdatedFalse(pageable);
  }

  @Transactional(readOnly = false, rollbackFor = Exception.class)
  @Override
  public void updateProducts(List<String> products) {
    if (CollectionUtils.isNotEmpty(products)) {
      productScoreUpdateRepository.updateProducts(products);
    }
  }
}
