package com.gdn.x.mta.distributiontask.service.impl;

import com.gdn.x.mta.distributiontask.dao.api.AppealProductRepository;
import com.gdn.x.mta.distributiontask.model.AppealedProduct;
import com.gdn.x.mta.distributiontask.service.api.AppealProductService;
import lombok.RequiredArgsConstructor;
import lombok.extern.slf4j.Slf4j;
import org.springframework.stereotype.Service;
import org.springframework.transaction.annotation.Transactional;

@Slf4j
@Service
@RequiredArgsConstructor
public class AppealProductServiceImpl implements AppealProductService {

  private final AppealProductRepository appealProductRepository;

  @Override
  public AppealedProduct findAppealProductByProductCode(String productCode) {
    return appealProductRepository.findByProductCode(productCode);
  }

  @Override
  @Transactional(readOnly = false, rollbackFor = Exception.class)
  public void upsertAppealProduct(AppealedProduct appealedProduct) {
    appealProductRepository.save(appealedProduct);
  }

}
