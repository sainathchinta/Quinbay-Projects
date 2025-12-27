package com.gdn.x.mta.distributiontask.service.impl;

import com.fasterxml.jackson.core.JsonProcessingException;
import com.fasterxml.jackson.databind.ObjectMapper;
import com.gdn.partners.pdt.dto.configuration.distribution.IPRActionResponseDto;
import com.gdn.x.mta.distributiontask.domain.event.model.AddProductToIprSolrEventModel;
import com.gdn.x.mta.distributiontask.domain.event.model.IPRHistoryEventModel;
import com.gdn.x.mta.distributiontask.domain.event.model.ProductChange;
import com.gdn.x.mta.distributiontask.model.Constants;
import com.gdn.x.mta.distributiontask.model.ErrorCategory;
import com.gdn.x.mta.distributiontask.model.ErrorMessages;
import com.gdn.x.mta.distributiontask.inbound.config.KafkaTopicPropertiesConsumer;
import com.gdn.x.mta.distributiontask.model.ProductIPR;
import com.gdn.x.mta.distributiontask.model.dto.BrandReport;
import com.gdn.x.mta.distributiontask.model.dto.IPRHistoryDescription;
import com.gdn.x.mta.distributiontask.model.dto.IPRUpdateAssigneeRequest;
import com.gdn.x.mta.distributiontask.model.dto.IprActionRequest;
import com.gdn.x.mta.distributiontask.model.dto.SubmitEvidenceRequest;
import com.gdn.x.mta.distributiontask.model.dto.AddingIprProductDTO;
import com.gdn.x.mta.distributiontask.model.enums.IPRHistoryActivity;
import com.gdn.x.mta.distributiontask.model.enums.ProductStateIPR;
import com.gdn.x.mta.distributiontask.service.api.IprWrapperService;
import com.gdn.x.mta.distributiontask.service.api.IprService;
import com.gdn.x.mta.distributiontask.service.impl.config.KafkaPublisher;
import com.gdn.x.mta.distributiontask.service.impl.util.ConverterUtil;
import com.gdn.x.mta.distributiontask.service.impl.util.ValidationUtil;
import com.gdn.x.mta.distributiontask.util.GdnMandatoryRequestParameterUtil;
import lombok.RequiredArgsConstructor;
import lombok.extern.slf4j.Slf4j;

import org.apache.commons.collections.CollectionUtils;
import org.apache.commons.lang3.StringUtils;
import org.apache.commons.lang3.tuple.Pair;
import org.slf4j.MDC;
import org.springframework.beans.BeanUtils;
import org.springframework.stereotype.Service;

import java.util.Date;
import java.util.List;
import java.util.Objects;

@Service
@RequiredArgsConstructor
@Slf4j
public class IprWrapperServiceImpl implements IprWrapperService {

  private final IprService iprService;

  private final KafkaPublisher kafkaProducer;

  private final KafkaTopicPropertiesConsumer kafkaTopicPropertiesConsumer;

  private final ObjectMapper objectMapper;

  private static final String PRODUCT_ADDED_TO_IPR_DESCRIPTION = "Product added to IPR portal";

  @Override
  public String addProductToIPR(String productSku, String storeId, String source, String assignee,
      BrandReport brandReport) throws Exception {
    AddingIprProductDTO addingIprProductDTO =
        iprService.addProductToIPR(productSku, storeId, source, assignee, brandReport);
    if (Objects.nonNull(addingIprProductDTO.getIprProductSolr())) {
      kafkaProducer.send(kafkaTopicPropertiesConsumer.getAddIprProductSolrEvent(), productSku,
          AddProductToIprSolrEventModel.builder()
              .iprProductSolr(addingIprProductDTO.getIprProductSolr()).build());
      if(!addingIprProductDTO.isOnlyAssigneeUpdated()) {
        log.info("adding product sku: {} to IPR solr.", productSku);
        kafkaProducer.send(kafkaTopicPropertiesConsumer.getPublishHistoryForIprEvent(), productSku,
            IPRHistoryEventModel.builder().productSku(productSku)
                .activity(IPRHistoryActivity.ADD_TO_IPR.getValue())
                .description(PRODUCT_ADDED_TO_IPR_DESCRIPTION)
                .updatedBy(GdnMandatoryRequestParameterUtil.getUsername()).updatedDate(new Date())
                .build());
      }
      if(StringUtils.isNotBlank(addingIprProductDTO.getIprProductSolr().getAssignedTo())) {
        log.info("assignee updated for product sku: {} in IPR.", productSku);
        kafkaProducer.send(kafkaTopicPropertiesConsumer.getPublishHistoryForIprEvent(), productSku,
          IPRHistoryEventModel.builder().productSku(productSku)
            .activity(IPRHistoryActivity.ASSIGNEE_UPDATE.getValue()).description(
              objectMapper.writeValueAsString(
                IPRHistoryDescription.builder().previous(addingIprProductDTO.getExistingAssignee())
                  .current(addingIprProductDTO.getIprProductSolr().getAssignedTo()).build()))
            .updatedDate(new Date()).updatedBy(GdnMandatoryRequestParameterUtil.getUsername())
            .build());
      }
    }
    return addingIprProductDTO.getErrorCategory().getMessage();
  }

  @Override
  public void updateProductOnStateChange(ProductChange productChange) throws Exception {
    Pair<ProductIPR, IPRHistoryEventModel> productIPRAndIprHistoryPair =
        iprService.updateProductOnStateChange(productChange);
    publishSolrUpdateAndHistoryEvent(productIPRAndIprHistoryPair);
  }

  @Override
  public void submitEvidenceForProduct(SubmitEvidenceRequest submitEvidenceRequest)
    throws JsonProcessingException {
    ValidationUtil.checkParameter(StringUtils.isNotBlank(submitEvidenceRequest.getProductSku()),
      ErrorMessages.PRODUCT_SKU_MUST_NOT_BE_EMPTY);
    ValidationUtil.checkParameter(CollectionUtils.isNotEmpty(submitEvidenceRequest.getEvidenceUrl())
            || CollectionUtils.isNotEmpty(submitEvidenceRequest.getEvidenceFilePath()),
        ErrorMessages.EVIDENCE_FILE_OR_URL_IS_NEEDED_FOR_SUBMITTING_EVIDENCE);
    Pair<ProductIPR, IPRHistoryEventModel> productIPRAndIprHistoryPair =
      iprService.submitEvidenceForProduct(submitEvidenceRequest);
    publishSolrUpdateAndHistoryEvent(productIPRAndIprHistoryPair);
  }

  @Override
  public void updateAssignee(IPRUpdateAssigneeRequest iprUpdateAssigneeRequest) throws Exception {
    ValidationUtil.checkParameter(
        CollectionUtils.isNotEmpty(iprUpdateAssigneeRequest.getProductSku()),
        ErrorMessages.PRODUCT_SKU_MUST_NOT_BE_EMPTY);
    Pair<List<ProductIPR>, List<IPRHistoryEventModel>> productIPRAndIprHistoryPair =
        iprService.updateAssignee(iprUpdateAssigneeRequest);
    publishSolrUpdateAndHistoryEventList(productIPRAndIprHistoryPair);
  }

  private void publishSolrUpdateAndHistoryEvent(
      Pair<ProductIPR, IPRHistoryEventModel> productIPRAndIprHistoryPair) {
    if (Objects.nonNull(productIPRAndIprHistoryPair.getLeft())) {
      kafkaProducer.send(kafkaTopicPropertiesConsumer.getAddIprProductSolrEvent(),
        productIPRAndIprHistoryPair.getLeft().getProductSku(),
        ConverterUtil.toAddProductToIprSolrEventModel(productIPRAndIprHistoryPair.getLeft()));
    }
    if (Objects.nonNull(productIPRAndIprHistoryPair.getRight())) {
      kafkaProducer.send(kafkaTopicPropertiesConsumer.getPublishHistoryForIprEvent(),
          productIPRAndIprHistoryPair.getLeft().getProductSku(),
          productIPRAndIprHistoryPair.getRight());
    }
  }

  private void publishSolrUpdateAndHistoryEventList(
      Pair<List<ProductIPR>, List<IPRHistoryEventModel>> productIPRAndIprHistoryPair) {
    if (CollectionUtils.isNotEmpty(productIPRAndIprHistoryPair.getLeft())) {
      productIPRAndIprHistoryPair.getLeft().forEach(productIPR -> {
        kafkaProducer.send(kafkaTopicPropertiesConsumer.getAddIprProductSolrEvent(),
            productIPR.getProductSku(), AddProductToIprSolrEventModel.builder()
                .iprProductSolr(ConverterUtil.convertToIPRProductSolr(productIPR)).build());
      });
    }
    if (CollectionUtils.isNotEmpty(productIPRAndIprHistoryPair.getRight())) {
      productIPRAndIprHistoryPair.getRight().forEach(iprHistoryEventModel -> {
        if(Objects.nonNull(iprHistoryEventModel)) {
          kafkaProducer.send(kafkaTopicPropertiesConsumer.getPublishHistoryForIprEvent(),
              iprHistoryEventModel.getProductSku(), iprHistoryEventModel);
        }
      });
    }
  }

  @Override
  public ErrorCategory performIprActionForProduct(IprActionRequest iprActionRequest, String storeId)
      throws Exception {
    ValidationUtil.validateIprActionRequest(iprActionRequest);
    iprActionRequest.setStoreId(storeId);
    IPRActionResponseDto iprActionResponseDto =
      iprService.performIprActionForProduct(iprActionRequest, storeId);
    if (StringUtils.isEmpty(iprActionResponseDto.getErrorCategory().getMessage())) {
      log.info("deleting product sku: {} from IPR solr.", iprActionRequest.getProductSku());
      kafkaProducer.send(kafkaTopicPropertiesConsumer.getAddIprProductSolrEvent(),
          iprActionRequest.getProductSku(),
          AddProductToIprSolrEventModel.builder().productSku(iprActionRequest.getProductSku())
              .deleteSolrDocument(true).build());
    }
    if (iprActionResponseDto.getErrorCategory().getMessage().equals(Constants.IN_SAME_STATE)) {
      return ErrorCategory.EMPTY_ERROR_MESSAGE;
    }
    if (CollectionUtils.isNotEmpty(iprActionResponseDto.getIprHistoryEventModels())) {
      for(IPRHistoryEventModel iprHistoryEventModel: iprActionResponseDto.getIprHistoryEventModels()) {
        kafkaProducer.send(kafkaTopicPropertiesConsumer.getPublishHistoryForIprEvent(),
            iprActionRequest.getProductSku(), iprHistoryEventModel);
      }
    }
    if (Objects.nonNull(iprActionResponseDto.getProductEmailEventModel())) {
      kafkaProducer.send(kafkaTopicPropertiesConsumer.getAddProductMailEvent(),
        iprActionRequest.getProductSku(), iprActionResponseDto.getProductEmailEventModel());
    }
    return iprActionResponseDto.getErrorCategory();
  }

  @Override
  public void suspendEvidenceRequestedProduct(String productSku) throws Exception {
    ProductIPR productIPR = iprService.findByProductSku(productSku);
    ValidationUtil.checkParameter(Objects.nonNull(productIPR),
        ErrorCategory.PRODUCT_SKU_NOT_FOUND.getMessage());
    ValidationUtil.checkParameter(
      Boolean.TRUE.equals(ProductStateIPR.EVIDENCE_REQUESTED.name().equals(productIPR.getState())),
      ErrorMessages.PRODUCT_IS_NOT_IN_EVIDENCE_REQUESTED_STATE);
    MDC.put(GdnMandatoryRequestParameterUtil.USERNAME_KEY_PARAMETER,
      productIPR.getEvidenceRequestedBy());
    IprActionRequest iprActionRequest = new IprActionRequest();
    BeanUtils.copyProperties(productIPR, iprActionRequest);
    iprActionRequest.setAssignee(productIPR.getAssignedTo());
    iprActionRequest.setSellerNotes(productIPR.getSellerNotes());
    iprActionRequest.setAction(ProductStateIPR.SUSPENDED.name());
    iprActionRequest.setBulkAction(true);
    iprActionRequest.setUpdatedBy(productIPR.getEvidenceRequestedBy());
    performIprActionForProduct(iprActionRequest, productIPR.getStoreId());
  }

  @Override
  public void addDSModelProductToIPR(String productSku, String storeId, String source) {
    AddingIprProductDTO addingDSModelIPRProductDTO =
        iprService.addDSModelProductToIPR(productSku, storeId, source);
    if (Objects.nonNull(addingDSModelIPRProductDTO.getIprProductSolr())) {
      kafkaProducer.send(kafkaTopicPropertiesConsumer.getAddIprProductSolrEvent(), productSku,
          AddProductToIprSolrEventModel.builder()
              .iprProductSolr(addingDSModelIPRProductDTO.getIprProductSolr()).build());
      if (!addingDSModelIPRProductDTO.isOnlyAssigneeUpdated()) {
        log.info("adding DS Model IPR Product sku: {} to IPR solr.", productSku);
        kafkaProducer.send(kafkaTopicPropertiesConsumer.getPublishHistoryForIprEvent(), productSku,
            IPRHistoryEventModel.builder().productSku(productSku)
                .activity(IPRHistoryActivity.ADD_TO_IPR.getValue())
                .description(PRODUCT_ADDED_TO_IPR_DESCRIPTION)
                .updatedBy(GdnMandatoryRequestParameterUtil.getUsername()).updatedDate(new Date())
                .build());
      }
    }
  }
}
