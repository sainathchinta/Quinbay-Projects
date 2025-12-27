package com.gdn.partners.pbp.workflow.product;

import java.util.ArrayList;
import java.util.Date;
import java.util.HashSet;
import java.util.List;
import java.util.Set;
import java.util.stream.Collectors;

import org.apache.commons.collections.CollectionUtils;
import org.apache.commons.collections4.ListUtils;
import org.apache.commons.lang3.time.DateUtils;
import org.slf4j.Logger;
import org.slf4j.LoggerFactory;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.beans.factory.annotation.Value;
import org.springframework.scheduling.annotation.Async;
import org.springframework.stereotype.Service;

import com.gdn.mta.product.config.ApplicationProperties;
import com.gdn.mta.product.entity.ProductCollection;
import com.gdn.mta.product.repository.BusinessPartnerRepository;
import com.gdn.mta.product.repository.ProductCollectionRepository;
import com.gdn.mta.product.service.EmailNotificationService;
import com.gdn.partners.pbp.repository.productlevel3.ProductLevel3WipRepository;
import com.gdn.x.businesspartner.dto.BusinessPartnerCodesRequest;
import com.gdn.x.businesspartner.dto.ProfileResponse;
import com.newrelic.api.agent.Trace;

@Service
public class ProductWipAutoRejectServiceBean implements ProductWipAutoRejectService {

  private static final Logger LOGGER = LoggerFactory.getLogger(ProductWipAutoRejectServiceBean.class);
  private static final String NOTES = "scheduler auto reject product wip need correction expired";


  @Value("${business.partner.fetch.size:100}")
  public Integer businessPartnerFetchSize;

  @Autowired
  private ProductCollectionRepository productCollectionRepository;

  @Autowired
  private ProductWfService productWfService;

  @Autowired
  private ApplicationProperties applicationProperties;

  @Autowired
  private BusinessPartnerRepository businessPartnerRepository;

  @Autowired
  private ProductLevel3WipRepository productLevel3WipRepository;

  @Autowired
  private EmailNotificationService emailNotificationService;

  @Trace(dispatcher=true)
  @Override
  @Async("autoRejectProductWipNeedCorrectionExecutor")
  public void autoRejectProductWipNeedCorrectionExpired(String storeId) throws Exception {
    try {
      Date maxDate = DateUtils.addDays(new Date(), -Integer.parseInt(applicationProperties.getProductRejectMaxDays()));
      List<ProductCollection> productCollections =
          productCollectionRepository.findByStoreIdAndStateNeedCorrectionMoreThanMaxDays(storeId, maxDate);
      if (CollectionUtils.isNotEmpty(productCollections)) {
        Set<String> distictBusinessPartnerCodesSet = new HashSet<>();
        for (ProductCollection productCollection : productCollections) {
          distictBusinessPartnerCodesSet.add(productCollection.getBusinessPartnerCode());
        }
        List<String> distictBusinessPartnerCodes = new ArrayList<>(distictBusinessPartnerCodesSet);
        List<List<String>> subListsOfDistictBusinessPartnerCodes =
            ListUtils.partition(distictBusinessPartnerCodes, businessPartnerFetchSize);
        List<ProfileResponse> profileResponses = new ArrayList<>();
        for (List<String> subListOfDistictBusinessPartnerCodes : subListsOfDistictBusinessPartnerCodes) {
          BusinessPartnerCodesRequest businessPartnerCodesRequest = new BusinessPartnerCodesRequest();
          businessPartnerCodesRequest.setBusinessPartnerCodes(subListOfDistictBusinessPartnerCodes);
          profileResponses
              .addAll(businessPartnerRepository.filterDetailsByBusinessPartnerCodeList(businessPartnerCodesRequest));

        }
        if (CollectionUtils.isNotEmpty(profileResponses)) {
          List<String> businessPartnerCodesWithO2OFlagFalse =
              profileResponses.stream().filter(profileResponse -> !profileResponse.getCompany().isOfflineToOnlineFlag())
                  .map(ProfileResponse::getBusinessPartnerCode).collect(Collectors.toList());
          for (String busineePartnerCode : businessPartnerCodesWithO2OFlagFalse) {
            List<String> productCodes = productCollections.stream()
                .filter(productCollection -> productCollection.getBusinessPartnerCode().equals(busineePartnerCode))
                .map(ProductCollection::getProductCode).collect(Collectors.toList());
            productWfService.delete(productCodes, NOTES);
            List<ProductCollection> deletedProductCollections =
                productCollectionRepository.findByProductCodeInAndStoreIdAndMarkForDeleteTrue(productCodes, storeId);
            emailNotificationService.sendEmailToBusinessPartnerForDeletedProducts(
                storeId, busineePartnerCode, deletedProductCollections);
          }
        }
      }
    } catch (Exception e) {
      ProductWipAutoRejectServiceBean.LOGGER
          .error("Error when invoking autoRejectProductWipNeedCorrection at ProductWipAutoRejectServiceBean", e);
    }
  }

}
