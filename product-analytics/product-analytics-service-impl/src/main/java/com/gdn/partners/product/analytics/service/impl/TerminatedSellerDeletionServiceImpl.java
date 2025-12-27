package com.gdn.partners.product.analytics.service.impl;

import com.gdn.partners.product.analytics.client.XProduct.XProductOutbound;
import com.gdn.partners.product.analytics.entity.TerminatedSellerDeletion;
import com.gdn.partners.product.analytics.model.TerminatedSellerDeletionConstants;
import com.gdn.partners.product.analytics.properties.KafkaTopicProperties;
import com.gdn.partners.product.analytics.repository.TerminatedSellerDeletionRepository;
import com.gdn.partners.product.analytics.service.KafkaProducerService;
import com.gdn.partners.product.analytics.service.TerminatedSellerDeletionService;
import lombok.RequiredArgsConstructor;
import lombok.extern.slf4j.Slf4j;
import model.TerminatedSellerDeletionEventModel;
import org.apache.commons.collections.CollectionUtils;
import org.springframework.beans.factory.annotation.Value;
import org.springframework.scheduling.annotation.Async;
import org.springframework.stereotype.Service;

import java.time.Instant;
import java.util.HashMap;
import java.util.HashSet;
import java.util.List;
import java.util.Map;
import java.util.Objects;
import java.util.Set;
import java.util.UUID;

@Slf4j
@Service
@RequiredArgsConstructor
public class TerminatedSellerDeletionServiceImpl implements TerminatedSellerDeletionService {

  private final TerminatedSellerDeletionRepository terminatedSellerDeletionRepository;

  private final KafkaProducerService kafkaProducerService;

  private final KafkaTopicProperties kafkaTopicProperties;

  private final XProductOutbound xProductOutbound;

  private final Set<String> statusToPublish =
      Set.of(TerminatedSellerDeletionConstants.PENDING, TerminatedSellerDeletionConstants.FAILED);

  @Value("#{'${status.fetch.product.delete.agp}'.split(',')}")
  private Set<String> statusToFetchForAGPProductDeletion;

  @Value("#{'${success.statuses.for.seller.termination}'.split(',')}")
  private Set<String> successStatusesForSellerTermination;

  @Override
  @Async
  public void publishEventsForProductDeletion(String storeId, boolean publishForAGPDeletion) {
    String uuid = UUID.randomUUID().toString();
    log.info("[{}][{}] Starting publishEventsForProductDeletion, publishForAGPDeletion={}",
        Instant.now().toString(), uuid, publishForAGPDeletion);
    Set<String> statusToPublish = getStatusToPublish(publishForAGPDeletion);
    List<TerminatedSellerDeletion> list =
        terminatedSellerDeletionRepository.fetchTerminatedSellerProductsForDeletion(storeId,
            statusToPublish);
    list.forEach(terminatedSellerDeletion -> terminatedSellerDeletion.setFinalResult(
        TerminatedSellerDeletionConstants.PICKED));
    terminatedSellerDeletionRepository.saveAll(list);
    if(CollectionUtils.isNotEmpty(list)) {
      if (publishForAGPDeletion) {
        list.forEach(this::publishEventsForAGPDeletion);
      } else {
        list.forEach(this::validateAndPublishToDownStreams);
      }
    }
    log.info("[{}][{}] Finished publishEventsForProductDeletion, processed {} records",
        Instant.now().toString(), uuid, list.size());
  }

  private Set<String> getStatusToPublish(boolean publishForAGPDeletion) {
    Set<String> statusToPublish =
      Set.of(TerminatedSellerDeletionConstants.PENDING, TerminatedSellerDeletionConstants.FAILED);
    if (publishForAGPDeletion) {
      statusToPublish = statusToFetchForAGPProductDeletion;
    }
    return statusToPublish;
  }

  @Override
  public void updateStatusForParticularService(String productCode, String sellerCode,
      String service, String result) {
    Set<String> statusesToBeIgnored = new HashSet<>(statusToFetchForAGPProductDeletion);
    statusesToBeIgnored.add(TerminatedSellerDeletionConstants.SUCCESS);
    TerminatedSellerDeletion terminatedSellerDeletion =
        terminatedSellerDeletionRepository.findByProductCodeAndSellerCodeAndFinalResultNotIn(productCode, sellerCode,
            statusesToBeIgnored);

    switch (service) {
      case TerminatedSellerDeletionConstants.PBP:
        terminatedSellerDeletion.setPbp(result);
        break;
      case TerminatedSellerDeletionConstants.PDT:
        terminatedSellerDeletion.setPdt(result);
        break;
      case TerminatedSellerDeletionConstants.PCB:
        terminatedSellerDeletion.setPcb(result);
        break;
      case TerminatedSellerDeletionConstants.X_PRODUCT:
        terminatedSellerDeletion.setXProduct(result);
        break;
      default:
        return;
    }
    if (successStatusesForSellerTermination.contains(terminatedSellerDeletion.getPdt())
        && successStatusesForSellerTermination.contains(terminatedSellerDeletion.getPcb())
        && successStatusesForSellerTermination.contains(terminatedSellerDeletion.getPbp())
        && successStatusesForSellerTermination.contains(terminatedSellerDeletion.getXProduct())) {
      terminatedSellerDeletion.setFinalResult(TerminatedSellerDeletionConstants.SUCCESS);
    } else if (TerminatedSellerDeletionConstants.FAILED.equals(terminatedSellerDeletion.getPdt()) || TerminatedSellerDeletionConstants.FAILED.equals(
        terminatedSellerDeletion.getPcb()) || TerminatedSellerDeletionConstants.FAILED.equals(terminatedSellerDeletion.getPbp())
        || TerminatedSellerDeletionConstants.FAILED.equals(terminatedSellerDeletion.getXProduct())) {
      terminatedSellerDeletion.setFinalResult(TerminatedSellerDeletionConstants.FAILED);
    }
    terminatedSellerDeletionRepository.save(terminatedSellerDeletion);
  }

  /**
   * Making the object terminatedSellerDeletion to a map of service names as key and status as
   * value and iterating it to publish different events for different services
   *
   * @param terminatedSellerDeletion
   */

  private void validateAndPublishToDownStreams(TerminatedSellerDeletion terminatedSellerDeletion) {
    Map<String, String> serviceNameToStatusMap = getMapFromObject(terminatedSellerDeletion);
    boolean isFailedEntryRetrying = false;
    TerminatedSellerDeletionEventModel terminatedSellerDeletionEventModel =
      constructEventModelFromEntity(terminatedSellerDeletion);
    Set<String> eventsToPublish = new HashSet<>();
    try {
      if (xProductOutbound.isSharedProduct(terminatedSellerDeletion.getSellerCode(),
        terminatedSellerDeletion.getProductCode())) {
        terminatedSellerDeletionEventModel.setSharedProduct(true);
        terminatedSellerDeletion.setFinalResult(TerminatedSellerDeletionConstants.SUCCESS);
        terminatedSellerDeletion.setSkipped(true);
      } else {
        for (Map.Entry<String, String> entry : serviceNameToStatusMap.entrySet()) {
          if (TerminatedSellerDeletionConstants.FAILED.equals(entry.getValue())) {
            isFailedEntryRetrying = true;
          }
          if (statusToPublish.contains(entry.getValue())) {
            eventsToPublish.add(
              kafkaTopicProperties.getPermanentDeleteProductEventsMappedToService()
                .get(entry.getKey()));
            setPublishedStatus(entry.getKey(), terminatedSellerDeletion);
          }
        }
      }
      if (isFailedEntryRetrying) {
        terminatedSellerDeletion.setRetryCount(terminatedSellerDeletion.getRetryCount() + 1);
      }
      terminatedSellerDeletion = terminatedSellerDeletionRepository.save(terminatedSellerDeletion);
      eventsToPublish.forEach(event -> kafkaProducerService.publishMessageForProductDeletion(
        terminatedSellerDeletionEventModel, event));
    } catch (Exception ex) {
      log.error("Exception while publishing for product code {}",
        terminatedSellerDeletionEventModel.getProductCode(), ex);
      setPendingStatus(terminatedSellerDeletion);
      terminatedSellerDeletionRepository.save(terminatedSellerDeletion);
    }
  }

  private void setPublishedStatus(String serviceName,
      TerminatedSellerDeletion terminatedSellerDeletion) {
    switch (serviceName) {
      case TerminatedSellerDeletionConstants.PDT:
        terminatedSellerDeletion.setPdt(TerminatedSellerDeletionConstants.PUBLISHED);
        break;
      case TerminatedSellerDeletionConstants.PBP:
        terminatedSellerDeletion.setPbp(TerminatedSellerDeletionConstants.PUBLISHED);
        break;
      case TerminatedSellerDeletionConstants.PCB:
        terminatedSellerDeletion.setPcb(TerminatedSellerDeletionConstants.PUBLISHED);
        break;
      case TerminatedSellerDeletionConstants.X_PRODUCT:
        terminatedSellerDeletion.setXProduct(TerminatedSellerDeletionConstants.PUBLISHED);
        break;
    }
  }

  private void setPendingStatus(TerminatedSellerDeletion terminatedSellerDeletion) {
    terminatedSellerDeletion.setPdt(TerminatedSellerDeletionConstants.PENDING);
    terminatedSellerDeletion.setPbp(TerminatedSellerDeletionConstants.PENDING);
    terminatedSellerDeletion.setPcb(TerminatedSellerDeletionConstants.PENDING);
    terminatedSellerDeletion.setXProduct(TerminatedSellerDeletionConstants.PENDING);
    terminatedSellerDeletion.setFinalResult(TerminatedSellerDeletionConstants.PENDING);
  }

  private Map<String, String> getMapFromObject(TerminatedSellerDeletion terminatedSellerDeletion) {
    Map<String, String> serviceNameToStatusMap = new HashMap<>();
    serviceNameToStatusMap.put(TerminatedSellerDeletionConstants.X_PRODUCT,
        terminatedSellerDeletion.getXProduct());
    serviceNameToStatusMap.put(TerminatedSellerDeletionConstants.PBP,
        terminatedSellerDeletion.getPbp());
    serviceNameToStatusMap.put(TerminatedSellerDeletionConstants.PCB,
        terminatedSellerDeletion.getPcb());
    serviceNameToStatusMap.put(TerminatedSellerDeletionConstants.PDT,
        terminatedSellerDeletion.getPdt());
    return serviceNameToStatusMap;
  }

  private TerminatedSellerDeletionEventModel constructEventModelFromEntity(
      TerminatedSellerDeletion terminatedSellerDeletion) {
    return TerminatedSellerDeletionEventModel.builder()
        .productCode(terminatedSellerDeletion.getProductCode())
        .sellerCode(terminatedSellerDeletion.getSellerCode())
        .productSku(terminatedSellerDeletion.getProductSku()).build();
  }

  private void publishEventsForAGPDeletion(TerminatedSellerDeletion terminatedSellerDeletion) {
    TerminatedSellerDeletionEventModel terminatedSellerDeletionEventModel =
        constructEventModelFromEntity(terminatedSellerDeletion);
    try {
      boolean sharedProduct = Objects.nonNull(terminatedSellerDeletion.getSharedProduct()) ?
        terminatedSellerDeletion.getSharedProduct() :
        xProductOutbound.isSharedProduct(terminatedSellerDeletion.getSellerCode(),
          terminatedSellerDeletion.getProductCode());
      if (sharedProduct) {
        terminatedSellerDeletionEventModel.setSharedProduct(true);
      }
      terminatedSellerDeletion.setFinalResult(TerminatedSellerDeletionConstants.SUCCESS);
      terminatedSellerDeletion.setMarkForDelete(true);
      kafkaProducerService.publishMessageForProductDeletion(terminatedSellerDeletionEventModel,
          kafkaTopicProperties.getAgpPermanentDeleteProductEventName());
      terminatedSellerDeletionRepository.save(terminatedSellerDeletion);
    } catch (Exception e) {
      log.error(
          "Exception while publishing for terminated seller deletion in AGP for product sku {}",
          terminatedSellerDeletionEventModel.getProductSku(), e);
      terminatedSellerDeletion.setFinalResult(TerminatedSellerDeletionConstants.FAILED);
      terminatedSellerDeletion.setMarkForDelete(true);
      terminatedSellerDeletionRepository.save(terminatedSellerDeletion);
    }
  }
}
