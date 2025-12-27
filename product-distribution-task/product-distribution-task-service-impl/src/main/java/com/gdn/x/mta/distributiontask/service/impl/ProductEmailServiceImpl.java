package com.gdn.x.mta.distributiontask.service.impl;

import com.gdn.x.mta.distributiontask.dao.api.ProductEmailsRepository;
import com.gdn.x.mta.distributiontask.domain.event.model.ProductEmailDomainEvent;
import com.gdn.x.mta.distributiontask.domain.event.model.ProductEmailEventModel;
import com.gdn.x.mta.distributiontask.model.Constants;
import com.gdn.x.mta.distributiontask.model.ProductEmails;
import com.gdn.x.mta.distributiontask.service.api.ProductEmailService;
import com.gdn.x.mta.distributiontask.service.impl.publisher.ProductEmailEventPublisherImpl;
import com.google.common.collect.Lists;
import lombok.RequiredArgsConstructor;
import lombok.extern.slf4j.Slf4j;
import org.apache.commons.collections.CollectionUtils;
import org.springframework.beans.BeanUtils;
import org.springframework.beans.factory.annotation.Value;
import org.springframework.scheduling.annotation.Async;
import org.springframework.stereotype.Service;
import org.springframework.transaction.annotation.Transactional;

import java.util.ArrayList;
import java.util.List;

@Slf4j
@Service
@RequiredArgsConstructor
public class ProductEmailServiceImpl implements ProductEmailService {

  private final ProductEmailsRepository productEmailsRepository;
  private final ProductEmailEventPublisherImpl productEmailEventPublisher;

  @Value("${product.sku.mail.batch.size}")
  private int productSkuMailBatchSize;

  @Override
  @Transactional
  @Async
  public void sendProductMailEventsToBusinessPartnersForSuspension(String storeId,
    String productEmailType) {
    log.info("Sending evidence requested mail");
    List<String> distinctBusinessPartnerCodeList =
      productEmailsRepository.findBusinessPartnerCodesAndProductEmailTypeAndStatus(storeId,
        productEmailType, Constants.PENDING);
    if (CollectionUtils.isNotEmpty(distinctBusinessPartnerCodeList)) {
      for (String businessPartnerCode : distinctBusinessPartnerCodeList) {
        sendBusinessPartnerMailForEvidenceRequested(storeId, productEmailType, businessPartnerCode);
      }
    }
  }

  @Override
  @Transactional
  public void addProductToEmailProcess(ProductEmailEventModel productEmailEventModel) {
    if (productEmailEventModel.isResetStatus()) {
      productEmailsRepository.deleteMailRecord(productEmailEventModel.getProductSku(),
        productEmailEventModel.getBusinessPartnerCode());
    } else {
      ProductEmails productEmails = new ProductEmails();
      BeanUtils.copyProperties(productEmailEventModel, productEmails);
      productEmails.setStatus(Constants.PENDING);
      productEmailsRepository.save(productEmails);
    }
  }

  public void sendBusinessPartnerMailForEvidenceRequested(String storeId, String productMailType,
    String businessPartnerCode) {
    List<ProductEmails> productEmailsList =
      productEmailsRepository.findByStoreIdAndBusinessPartnerCodeAndProductEmailTypeAndStatus(
        storeId, businessPartnerCode, productMailType, Constants.PENDING);
    List<List<ProductEmails>> productEmailListPartitonList =
      Lists.partition(productEmailsList, productSkuMailBatchSize);
    if (CollectionUtils.isNotEmpty(productEmailsList)) {
      for (List<ProductEmails> productEmails : productEmailListPartitonList) {
        publishEmailEventForEvidenceRequested(productEmails, businessPartnerCode, productMailType);
      }
    }
  }

  public void publishEmailEventForEvidenceRequested(List<ProductEmails> productEmailsList,
    String businessPartnerCode, String productEmailType) {
    List<List<String>> productEmailData = new ArrayList<>();
    for (ProductEmails productEmail : productEmailsList) {
      List<String> productData =
        List.of(productEmail.getProductSku(), productEmail.getProductName(),
          productEmail.getNotes());
      productEmailData.add(productData);
      productEmail.setStatus(Constants.SUCCESS);
      productEmailsRepository.save(productEmail);
    }
    ProductEmailDomainEvent productEmailDomainEvent =
      ProductEmailDomainEvent.builder().productData(productEmailData)
        .notificationType(productEmailType).businessPartnerCode(businessPartnerCode).build();
    productEmailEventPublisher.publishProductMailDomainEventForIprEvidenceRequestedProduct(
      productEmailDomainEvent);
  }
}
