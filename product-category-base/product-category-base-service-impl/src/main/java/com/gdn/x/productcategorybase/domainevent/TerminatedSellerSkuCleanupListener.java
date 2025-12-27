package com.gdn.x.productcategorybase.domainevent;

import static com.google.common.base.Preconditions.checkArgument;

import java.util.Objects;

import org.apache.commons.lang3.StringUtils;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.beans.factory.annotation.Value;
import org.springframework.kafka.annotation.KafkaListener;
import org.springframework.stereotype.Service;

import com.fasterxml.jackson.databind.ObjectMapper;
import com.gdn.x.productcategorybase.Constants;
import com.gdn.x.productcategorybase.ErrorMessage;
import com.gdn.x.productcategorybase.domain.event.model.TerminatedSellerSkuCleanupEventModel;
import com.gdn.x.productcategorybase.dto.TerminatedSellerSkuCleanupStatusDTO;
import com.gdn.x.productcategorybase.entity.Product;
import com.gdn.x.productcategorybase.enums.TerminatedSellerSkuStatus;
import com.gdn.x.productcategorybase.service.ProductDeletionService;
import com.gdn.x.productcategorybase.service.ProductDeletionWrapperService;
import com.gdn.x.productcategorybase.service.config.KafkaTopicProperties;
import lombok.extern.slf4j.Slf4j;

@Service
@Slf4j
public class TerminatedSellerSkuCleanupListener {

  @Autowired
  private KafkaTopicProperties kafkaTopicProperties;

  @Autowired
  private ObjectMapper objectMapper;

  @Autowired
  private ProductDeletionService productDeletionService;

  @Autowired
  private ProductDeletionWrapperService productDeletionWrapperService;

  @Value("${terminated.seller.sku.cleanup.service.name}")
  private String terminatedSellerSkuCleanupServiceName;

  @KafkaListener(topics = "#{kafkaTopicProperties.getTerminatedSellerSkuCleanup()}", autoStartup = "#{kafkaTopicProperties.isAutoStartup()}")
  public void onDomainEventConsumed(String message) throws Exception {
    log.info("Message received, from topic {}, message :{}", kafkaTopicProperties.getTerminatedSellerSkuCleanup(), message);

    TerminatedSellerSkuCleanupStatusDTO statusDTO = new TerminatedSellerSkuCleanupStatusDTO();
    boolean publishEventToDeleteImage = false, isPickedForDeletion = false;
    TerminatedSellerSkuStatus deletionStatus = TerminatedSellerSkuStatus.SUCCESS;
    try {
      TerminatedSellerSkuCleanupEventModel request =
          objectMapper.readValue(message, TerminatedSellerSkuCleanupEventModel.class);
      setSellerSkuCleanupStatusDTO(statusDTO, request);
      checkArgument(StringUtils.isNotBlank(request.getProductCode()), ErrorMessage.PRODUCT_CODE_MUST_NOT_BE_BLANK);
      checkArgument(StringUtils.isNotBlank(request.getSellerCode()), ErrorMessage.SELLER_CODE_MUST_NOT_BE_BLANK);

      Product product =
          productDeletionWrapperService.getProductDetails(Constants.DEFAULT_STORE_ID, request.getProductCode());
      if (Objects.nonNull(product)) {
        isPickedForDeletion = productDeletionService.pickedForDeletion(product);
        if (!isPickedForDeletion) {
          productDeletionService.updatePickedForDeletionFlag(product, Boolean.TRUE);
          publishEventToDeleteImage = productDeletionService.isAllowedToDeleteImage(product.getProductItems());
          productDeletionWrapperService.hardDeleteProductAndClearCache(product.getStoreId(), product);
        }
      }

    } catch (Exception e) {
      log.error("Exception caught while processing terminated seller cleanup event , message : {} ", message, e);
      deletionStatus = TerminatedSellerSkuStatus.FAILED;
      publishEventToDeleteImage = false;
    } finally {
      if (!isPickedForDeletion) {
        publishEventToUpdateStatusAndDeleteImage(statusDTO, deletionStatus, publishEventToDeleteImage);
      }
    }
  }

  private void publishEventToUpdateStatusAndDeleteImage(TerminatedSellerSkuCleanupStatusDTO statusDTO,
      TerminatedSellerSkuStatus status, boolean publishEventToDeleteImage) {
    statusDTO.setPublishImageDeletionEvent(publishEventToDeleteImage);
    statusDTO.setResult(status.name());
    productDeletionService.publishEventToUpdateStatusAndDeleteImage(statusDTO);
  }

  private void setSellerSkuCleanupStatusDTO(TerminatedSellerSkuCleanupStatusDTO statusDTO,
      TerminatedSellerSkuCleanupEventModel request) {
    statusDTO.setSellerCode(request.getSellerCode());
    statusDTO.setProductCode(request.getProductCode());
    statusDTO.setService(terminatedSellerSkuCleanupServiceName);
  }

}
