package com.gdn.partners.product.analytics.service.impl;

import java.util.ArrayList;
import java.util.Arrays;
import java.util.List;
import java.util.stream.Collectors;

import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.stereotype.Service;

import com.gdn.partners.product.analytics.entity.AutoQCDetail;
import com.gdn.partners.product.analytics.model.Constants;
import com.gdn.partners.product.analytics.properties.ApplicationProperties;
import com.gdn.partners.product.analytics.properties.GCPProperties;
import com.gdn.partners.product.analytics.repository.AutoQCRepository;
import com.gdn.partners.product.analytics.service.AutoQCDetailService;
import com.gdn.partners.product.analytics.service.KafkaProducerService;
import com.gdn.partners.product.analytics.service.cache.AutoQCDetailCacheableService;
import com.gdn.partners.product.analytics.service.impl.helper.ResponseHelper;
import com.gdn.partners.product.analytics.web.model.AutoQCDetailResponse;
import com.gdn.partners.product.analytics.web.model.SellerFieldsChangeResponse;
import com.google.common.collect.Lists;
import com.mongodb.bulk.BulkWriteResult;
import lombok.extern.slf4j.Slf4j;

@Service
@Slf4j
public class AutoQCDetailServiceImpl implements AutoQCDetailService {

  @Autowired
  private AutoQCDetailCacheableService autoQCDetailCacheableService;

  @Autowired
  private AutoQCRepository autoQCRepository;

  @Autowired
  private ApplicationProperties applicationProperties;

  @Autowired
  private GCPProperties gcpProperties;

  @Autowired
  private KafkaProducerService kafkaProducerService;

  @Override
  public AutoQCDetailResponse findByMerchantCodeAndCategoryCode(String merchantCode, String categoryCode)
      throws Exception {
    AutoQCDetail autoQCDetails =
        autoQCDetailCacheableService.findCacheablesByMerchantCodeAndCategoryCode(merchantCode, categoryCode);
    return ResponseHelper.getAutoQCDetailResponse(autoQCDetails);
  }

  @Override
  public void updateOfficialStoreFlagBySellerCode(String sellerCode, boolean officialStore) {
    List<AutoQCDetail> filteredAutoQCDetailList = fetchFilteredAutoQcAndUpdateData(sellerCode, officialStore);
    List<SellerFieldsChangeResponse> sellerFieldsChangeResponseList = new ArrayList<>();
    Lists.partition(filteredAutoQCDetailList, gcpProperties.getBulkUpdateBatchSize()).forEach(autoQCDetailList1 -> {
      BulkWriteResult bulkWriteResult = autoQCRepository
          .bulkWriteAutoQcDetail(autoQCDetailList1, sellerFieldsChangeResponseList,
              Arrays.asList(applicationProperties.getChangeFieldList().split(Constants.COMMA)));
      log.info("Oxford event Documents total updated: {}", bulkWriteResult.getModifiedCount());
    });
    clearCacheAndPublishEvent(sellerFieldsChangeResponseList);
  }

  @Override
  public void clearCacheAndPublishEvent(List<SellerFieldsChangeResponse> sellerFieldsChangeResponseList) {
    sellerFieldsChangeResponseList.forEach(sellerFieldsChangeResponse -> autoQCDetailCacheableService
        .evictCacheByMerchantCodeAndCategoryCode(sellerFieldsChangeResponse.getSellerCode(),
            sellerFieldsChangeResponse.getCategoryCode()));
    sellerFieldsChangeResponseList
        .forEach(sellerFieldsChangeResponse -> kafkaProducerService.publishMessage(sellerFieldsChangeResponse));
  }

  private List<AutoQCDetail> fetchFilteredAutoQcAndUpdateData(String sellerCode, boolean officialStore) {
    List<AutoQCDetail> autoQCDetailList = autoQCRepository.findByBusinessPartnerCode(sellerCode);
    List<AutoQCDetail> filteredAutoQCDetailList =
        autoQCDetailList.stream().filter(autoQCDetail -> (officialStore != autoQCDetail.getIsOfficialStore()))
            .collect(Collectors.toList());
    filteredAutoQCDetailList.forEach(autoQCDetail -> autoQCDetail.setIsOfficialStore(officialStore));
    return filteredAutoQCDetailList;
  }
}
