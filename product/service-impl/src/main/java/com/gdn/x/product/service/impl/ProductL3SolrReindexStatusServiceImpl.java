package com.gdn.x.product.service.impl;

import com.gdn.x.product.dao.api.ProductL3SolrReindexStatusRepository;
import com.gdn.x.product.model.entity.ProductL3SolrReindexStatus;
import com.gdn.x.product.service.api.ProductL3SolrReindexStatusService;
import org.apache.commons.collections.CollectionUtils;
import org.springframework.beans.BeanUtils;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.stereotype.Service;

import java.util.ArrayList;
import java.util.List;
import java.util.Map;
import java.util.Objects;
import java.util.function.Function;
import java.util.stream.Collectors;

@Service
public class ProductL3SolrReindexStatusServiceImpl implements ProductL3SolrReindexStatusService {

  @Autowired
  private ProductL3SolrReindexStatusRepository productL3SolrReindexStatusRepository;

  @Override
  public void insertProductSkuToReindexStatusCollection(ProductL3SolrReindexStatus productL3SolrReindexStatus) {
    ProductL3SolrReindexStatus productL3SolrReindexStatusSaved = productL3SolrReindexStatusRepository
        .findByStoreIdAndProductSku(productL3SolrReindexStatus.getStoreId(),
            productL3SolrReindexStatus.getProductSku());
    if (Objects.nonNull(productL3SolrReindexStatusSaved)) {
      productL3SolrReindexStatusSaved.setProductReindexStatus(productL3SolrReindexStatus.getProductReindexStatus());
      productL3SolrReindexStatusSaved.setMarkForDelete(false);
    } else {
      productL3SolrReindexStatusSaved = new ProductL3SolrReindexStatus();
      BeanUtils.copyProperties(productL3SolrReindexStatus, productL3SolrReindexStatusSaved);
    }
    productL3SolrReindexStatusRepository.save(productL3SolrReindexStatusSaved);
  }

  @Override
  public void insertProductSkusToReindexStatusCollection(String storeId,
      List<ProductL3SolrReindexStatus> productL3SolrReindexStatuses) {
    List<ProductL3SolrReindexStatus> productL3SolrReindexStatusesSaved =
        productL3SolrReindexStatusRepository.findByStoreIdAndProductSkuIn(storeId,
            productL3SolrReindexStatuses.stream().map(ProductL3SolrReindexStatus::getProductSku)
                .collect(Collectors.toList()));
    List<ProductL3SolrReindexStatus> updatedList = new ArrayList<>();
    Map<String, ProductL3SolrReindexStatus> productSkuL3StatusMap = productL3SolrReindexStatusesSaved.stream()
        .collect(Collectors.toMap(ProductL3SolrReindexStatus::getProductSku, Function.identity()));
    for (ProductL3SolrReindexStatus productL3SolrReindexStatus : productL3SolrReindexStatuses) {
      ProductL3SolrReindexStatus productL3SolrReindexStatusSaved;
      if (productSkuL3StatusMap.containsKey(productL3SolrReindexStatus.getProductSku())) {
        productL3SolrReindexStatusSaved = productSkuL3StatusMap.get(productL3SolrReindexStatus.getProductSku());
        productL3SolrReindexStatusSaved.setProductReindexStatus(productL3SolrReindexStatus.getProductReindexStatus());
        productL3SolrReindexStatusSaved.setMarkForDelete(false);
      } else {
        productL3SolrReindexStatusSaved = new ProductL3SolrReindexStatus();
        BeanUtils.copyProperties(productL3SolrReindexStatus, productL3SolrReindexStatusSaved);
      }
      updatedList.add(productL3SolrReindexStatusSaved);
    }
    if (CollectionUtils.isNotEmpty(updatedList)) {
      productL3SolrReindexStatusRepository.saveAll(updatedList);
    }
  }
}
