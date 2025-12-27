package com.gdn.x.product.service.impl;

import java.util.Date;
import java.util.List;
import java.util.stream.Collectors;

import org.apache.commons.collections.CollectionUtils;
import org.apache.commons.lang3.time.DateUtils;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.scheduling.annotation.Async;
import org.springframework.stereotype.Service;

import com.gdn.x.product.dao.api.ItemArchiveRepository;
import com.gdn.x.product.dao.api.ItemPickupPointArchiveRepository;
import com.gdn.x.product.dao.api.ProductArchiveRepository;
import com.gdn.x.product.enums.SystemParameterNames;
import com.gdn.x.product.model.entity.ProductArchive;
import com.gdn.x.product.service.api.ProductDataArchivalService;
import com.gdn.x.product.service.api.SystemParameterService;

import lombok.extern.slf4j.Slf4j;

@Slf4j
@Service
public class ProductDataArchivalServiceImpl implements ProductDataArchivalService {

  @Autowired
  private ProductArchiveRepository productArchiveRepository;

  @Autowired
  private ItemArchiveRepository itemArchiveRepository;

  @Autowired
  private ItemPickupPointArchiveRepository itemPickupPointArchiveRepository;

  @Autowired
  private SystemParameterService systemParameterService;


  @Override
  @Async
  public void deleteProductArchivedData(String storeId) {
    int batchSize = Integer.parseInt(systemParameterService.findValueByStoreIdAndVariable(storeId,
        SystemParameterNames.BATCH_SIZE_TO_DELETE_ARCHIVED_PRODUCTS).getValue());
    if (batchSize != 0) {
      int dateThreshold = Integer.parseInt(systemParameterService.findValueByStoreIdAndVariable(storeId,
          SystemParameterNames.DAYS_THRESHOLD_FOR_ARCHIVED_PRODUCT_DELETION).getValue());
      Date dateFilter = DateUtils.addDays(new Date(), -dateThreshold);
      List<ProductArchive> productList = productArchiveRepository.fetchArchivedListOfProduct(dateFilter, batchSize);
      if (CollectionUtils.isNotEmpty(productList)) {
        List<String> productSkus = productList.stream().map(ProductArchive::getProductSku).collect(Collectors.toList());
        productArchiveRepository.deleteByProductSku(productSkus);
        itemArchiveRepository.deleteByProductSku(productSkus);
        itemPickupPointArchiveRepository.deleteByProductSku(productSkus);
      }
    }
  }

}
