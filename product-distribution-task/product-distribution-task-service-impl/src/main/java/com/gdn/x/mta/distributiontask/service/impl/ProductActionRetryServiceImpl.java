package com.gdn.x.mta.distributiontask.service.impl;

import java.util.List;
import java.util.Objects;

import org.slf4j.MDC;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.data.domain.PageRequest;
import org.springframework.stereotype.Service;
import org.springframework.transaction.annotation.Transactional;

import com.gdn.x.mta.distributiontask.util.GdnMandatoryRequestParameterUtil;
import com.gdn.mta.domain.event.modal.ProductActionRetryEvent;
import com.gdn.x.mta.distributiontask.dao.api.ProductActionRetryRepository;
import com.gdn.x.mta.distributiontask.model.Constants;
import com.gdn.x.mta.distributiontask.model.ProductActionRetry;
import com.gdn.x.mta.distributiontask.model.enums.ActionRetryStatus;
import com.gdn.x.mta.distributiontask.service.api.ProductActionRetryService;
import lombok.extern.slf4j.Slf4j;

@Service
@Slf4j
public class ProductActionRetryServiceImpl implements ProductActionRetryService {

  @Autowired
  private ProductActionRetryRepository productActionRetryRepository;

  @Override
  public List<ProductActionRetry> findProductsToRetryActionOrderByCreatedDateAsc(String storeId,
      String action, ActionRetryStatus actionRetryStatus, int maximumAllowedProductForActionRetry) {
    List<ProductActionRetry> productActionRetryList =
        productActionRetryRepository.findByStoreIdAndActionAndStatusOrderByCreatedDateAsc(storeId, action, actionRetryStatus,
            PageRequest.of(Constants.PAGE_ZERO, maximumAllowedProductForActionRetry));
    return productActionRetryList;
  }

  @Override
  public List<ProductActionRetry> findProductsToRetryActionOrderByCreatedDateDesc(String storeId,
      String action, ActionRetryStatus actionRetryStatus, int maximumAllowedProductForActionRetry) {
    List<ProductActionRetry> productActionRetryList =
        productActionRetryRepository.findByStoreIdAndActionAndStatusOrderByCreatedDateDesc(storeId, action, actionRetryStatus,
            PageRequest.of(Constants.PAGE_ZERO, maximumAllowedProductForActionRetry));
    return productActionRetryList;
  }

  @Override
  public List<ProductActionRetry> findProductsToRetryActionOrderByUpdatedDateAsc(String storeId,
      String action, ActionRetryStatus actionRetryStatus, int maximumAllowedProductForActionRetry) {
    List<ProductActionRetry> productActionRetryList =
        productActionRetryRepository.findByStoreIdAndActionAndStatusOrderByUpdatedDateAsc(storeId, action, actionRetryStatus,
            PageRequest.of(Constants.PAGE_ZERO, maximumAllowedProductForActionRetry));
    return productActionRetryList;
  }

  @Override
  public List<ProductActionRetry> findProductsToRetryActionOrderByUpdatedDateDesc(String storeId,
      String action, ActionRetryStatus actionRetryStatus, int maximumAllowedProductForActionRetry) {
    List<ProductActionRetry> productActionRetryList =
        productActionRetryRepository.findByStoreIdAndActionAndStatusOrderByUpdatedDateDesc(storeId, action, actionRetryStatus,
            PageRequest.of(Constants.PAGE_ZERO, maximumAllowedProductForActionRetry));
    return productActionRetryList;
  }

  @Override
  @Transactional(readOnly = false)
  public void updateProductActionRetryDetails(ProductActionRetry productActionRetry) {
    MDC.put(GdnMandatoryRequestParameterUtil.USERNAME_KEY_PARAMETER, Constants.SYSTEM);
    if(ActionRetryStatus.PENDING.equals(productActionRetry.getStatus())) {
      productActionRetry.setRetryCount(productActionRetry.getRetryCount() + 1);
    } else {
      productActionRetry.setMarkForDelete(Boolean.TRUE);
    }
    productActionRetryRepository.save(productActionRetry);
  }

  @Override
  @Transactional
  public ProductActionRetry getProductActionRetryByProductCodeAndAction(String storeId, String productCode, String action){
    return productActionRetryRepository.findByStoreIdAndProductCodeAndAction(storeId, productCode, action);
  }

  @Override
  @Transactional(readOnly = false)
  public void upsertProductActionRetry(ProductActionRetryEvent productActionRetryEvent) {
    ProductActionRetry productActionRetry = productActionRetryRepository
        .findByStoreIdAndProductCodeAndAction(productActionRetryEvent.getStoreId(),
            productActionRetryEvent.getProductCode(), productActionRetryEvent.getAction());
    if (Objects.nonNull(productActionRetry)) {
      setProductActionRetryData(productActionRetryEvent, productActionRetry);
    } else {
      productActionRetry = new ProductActionRetry();
      setProductActionRetryData(productActionRetryEvent, productActionRetry);
    }
    productActionRetryRepository.save(productActionRetry);
  }

  @Transactional(readOnly = false, rollbackFor = Exception.class)
  @Override
  public void saveProductActionRetryList(List<ProductActionRetry> productActionRetries) {
    productActionRetryRepository.saveAll(productActionRetries);
  }

  private void setProductActionRetryData(ProductActionRetryEvent productActionRetryEvent,
      ProductActionRetry productActionRetry) {
    productActionRetry.setStoreId(productActionRetryEvent.getStoreId());
    productActionRetry.setStatus(ActionRetryStatus.PENDING);
    productActionRetry.setMarkForDelete(false);
    productActionRetry.setData(productActionRetryEvent.getData());
    productActionRetry.setAction(productActionRetryEvent.getAction());
    productActionRetry.setRetryCount(0);
    productActionRetry.setProductCode(productActionRetryEvent.getProductCode());
  }
}
